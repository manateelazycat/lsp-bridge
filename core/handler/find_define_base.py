import hashlib
import json
import os
import re
import tempfile
from urllib.parse import urlparse, unquote

from core.utils import *


def _detect_solidity_project_root(start_path):
    """Walk up from start_path to find the Solidity project root
    by looking for typical project markers (foundry.toml, hardhat.config, etc.)."""
    markers = ("foundry.toml", "hardhat.config.js", "hardhat.config.ts",
               "truffle-config.js", "brownie-config.yaml", "remappings.txt")
    d = os.path.dirname(os.path.abspath(start_path)) if os.path.isfile(start_path) else os.path.abspath(start_path)
    fallback = d
    while True:
        for marker in markers:
            if os.path.exists(os.path.join(d, marker)):
                return d
        parent = os.path.dirname(d)
        if parent == d:
            break
        d = parent
    return fallback


def _read_solidity_remappings(project_path):
    """Read Solidity import remappings from remappings.txt and/or foundry.toml."""
    remappings = []
    if not project_path:
        return remappings

    # ── Source 1: remappings.txt ──
    remappings_file = os.path.join(project_path, "remappings.txt")
    if os.path.isfile(remappings_file):
        try:
            with open(remappings_file, encoding="utf-8", errors="ignore") as f:
                for raw_line in f:
                    line = raw_line.strip()
                    if not line or line.startswith("#") or "=" not in line:
                        continue
                    # Strip optional context: prefix  (e.g. "ctx:@oz/=lib/oz/")
                    eq_idx = line.index("=")
                    left = line[:eq_idx]
                    if ":" in left:
                        left = left.split(":", 1)[1]
                    prefix = left.strip().strip("'\"")
                    target = line[eq_idx + 1:].strip().strip("'\"")
                    if prefix and target:
                        remappings.append((prefix, target))
        except OSError:
            pass

    # ── Source 2: foundry.toml ──
    if not remappings:
        foundry_toml = os.path.join(project_path, "foundry.toml")
        if os.path.isfile(foundry_toml):
            try:
                with open(foundry_toml, encoding="utf-8", errors="ignore") as f:
                    content = f.read()
                # Parse remappings = [...] without a TOML library.
                # Handles single-line and multi-line arrays.
                m = re.search(r'remappings\s*=\s*\[(.*?)\]', content, re.DOTALL)
                if m:
                    for entry in re.findall(r'["\']([^"\' ]+)["\']', m.group(1)):
                        entry = entry.strip()
                        if "=" not in entry:
                            continue
                        eq_idx = entry.index("=")
                        left = entry[:eq_idx]
                        if ":" in left:
                            left = left.split(":", 1)[1]
                        prefix = left.strip()
                        target = entry[eq_idx + 1:].strip()
                        if prefix and target:
                            remappings.append((prefix, target))
            except OSError:
                pass

    # ── Source 3: auto-detect lib/ subdirectories as implicit remappings ──
    # Always augment with lib/-based remappings.  Foundry auto-remaps lib/<pkg>/
    # using the package.json "name" field (e.g. @openzeppelin/contracts-upgradeable).
    lib_dir = os.path.join(project_path, "lib")
    if os.path.isdir(lib_dir):
        existing_prefixes = {p for p, _ in remappings}
        try:
            for entry in sorted(os.listdir(lib_dir)):
                entry_path = os.path.join(lib_dir, entry)
                if not os.path.isdir(entry_path):
                    continue
                src_dir = os.path.join(entry_path, "src")
                contracts_dir = os.path.join(entry_path, "contracts")
                if os.path.isdir(src_dir):
                    target_base = "lib/" + entry + "/src/"
                elif os.path.isdir(contracts_dir):
                    target_base = "lib/" + entry + "/contracts/"
                else:
                    target_base = "lib/" + entry + "/"

                # Directory-name remapping  (e.g. forge-std/ → lib/forge-std/src/)
                dir_prefix = entry + "/"
                if dir_prefix not in existing_prefixes:
                    remappings.append((dir_prefix, target_base))
                    existing_prefixes.add(dir_prefix)

                # Read package.json to discover @scope/package name (e.g.
                # @openzeppelin/contracts-upgradeable → lib/openzeppelin-contracts-upgradeable/contracts/)
                pkg_json_path = os.path.join(entry_path, "package.json")
                if os.path.isfile(pkg_json_path):
                    try:
                        with open(pkg_json_path, encoding="utf-8", errors="ignore") as pf:
                            pkg_data = json.load(pf)
                        pkg_name = pkg_data.get("name", "")
                        if pkg_name and pkg_name + "/" not in existing_prefixes:
                            remappings.append((pkg_name + "/", target_base))
                            existing_prefixes.add(pkg_name + "/")
                    except (OSError, ValueError, KeyError):
                        pass
        except OSError:
            pass

    return remappings


def _resolve_import_file_path(import_path, current_file, project_path):
    """Resolve a raw Solidity import path string to an absolute file path.
    Returns the resolved path or None."""
    if not import_path:
        return None

    candidates = []
    current_dir = os.path.dirname(current_file)

    if os.path.isabs(import_path):
        candidates.append(import_path)
    else:
        candidates.append(os.path.normpath(os.path.join(current_dir, import_path)))
        parent_dir = current_dir
        while True:
            next_parent = os.path.dirname(parent_dir)
            if next_parent == parent_dir:
                break
            parent_dir = next_parent
            candidates.append(os.path.normpath(os.path.join(parent_dir, import_path)))
        if project_path:
            candidates.append(os.path.normpath(os.path.join(project_path, import_path)))
            for prefix, target in _read_solidity_remappings(project_path):
                if import_path.startswith(prefix):
                    suffix = import_path[len(prefix):]
                    candidates.append(os.path.normpath(os.path.join(project_path, target, suffix)))

            # node_modules/ fallback (common for Hardhat / npm-based projects)
            nm_dir = os.path.join(project_path, "node_modules")
            if os.path.isdir(nm_dir):
                candidates.append(os.path.normpath(os.path.join(nm_dir, import_path)))

    checked = set()
    for candidate in candidates:
        if candidate in checked:
            continue
        checked.add(candidate)
        if os.path.isfile(candidate):
            return candidate

    return None


def _resolve_solidity_import_path(current_file, cursor_pos, project_path):
    """If the cursor line is part of an import statement, resolve its file path.
    Handles both single-line and multi-line imports."""
    if not current_file or not os.path.isfile(current_file):
        return None

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    lines = content.splitlines()
    line = cursor_pos.get("line", -1) if isinstance(cursor_pos, dict) else -1
    if line < 0 or line >= len(lines):
        return None

    line_content = lines[line]

    # Case 1: single-line import  e.g.  import "path.sol";  or  import { A } from "path.sol";
    match = re.search(r'^\s*import\s+(?:[^"\']+\s+from\s+)?["\']([^"\']+)["\']', line_content)
    if match:
        import_path = match.group(1).strip()
        if import_path:
            return _resolve_import_file_path(import_path, current_file, project_path)

    # Case 2: cursor is on the 'from "..."' line of a multi-line import.
    # e.g.:  } from "darkpoolv1-types/Settlement.sol";
    from_match = re.search(r'from\s+["\']([^"\']+)["\']', line_content)
    if from_match:
        # Verify this belongs to an import block by scanning upward for 'import {'
        for i in range(line - 1, max(line - 50, -1), -1):
            if i < 0:
                break
            if re.match(r'^\s*import\s', lines[i]):
                import_path = from_match.group(1).strip()
                return _resolve_import_file_path(import_path, current_file, project_path)
            # Stop if we hit a semicolon on a non-import line (different statement)
            if ';' in lines[i] and not re.match(r'^\s*import\s', lines[i]):
                break

    # Case 3: cursor is inside the braces of a multi-line import (on a name line).
    # Scan forward from cursor to find the 'from "..."' line.
    for j in range(line + 1, min(line + 50, len(lines))):
        fwd_from = re.search(r'from\s+["\']([^"\']+)["\']', lines[j])
        if fwd_from:
            # Verify by scanning backward from that line to find 'import'
            for i in range(j - 1, max(j - 50, -1), -1):
                if i < 0:
                    break
                if re.match(r'^\s*import\s', lines[i]):
                    if i <= line <= j:
                        import_path = fwd_from.group(1).strip()
                        return _resolve_import_file_path(import_path, current_file, project_path)
                    break
                if ';' in lines[i] and not re.match(r'^\s*import\s', lines[i]):
                    break
            break
        # Stop if we hit a new import or a non-from semicolon line
        if re.match(r'^\s*import\s', lines[j]):
            break
        if ';' in lines[j] and 'from' not in lines[j]:
            break

    return None


def _parse_all_imports(content):
    """Parse all import statements from Solidity source, handling both single-line
    and multi-line imports.

    Returns a list of tuples: (names_list, import_path)
      - names_list: list of (original_name, local_name) tuples, or None for wildcard imports
      - import_path: the string path from the import

    Examples:
      import "path.sol";                         => (None, "path.sol")
      import { A, B as C } from "path.sol";      => ([("A","A"), ("B","C")], "path.sol")
      import {\\n  A,\\n  B\\n} from "path.sol";    => ([("A","A"), ("B","B")], "path.sol")
    """
    results = []

    # Use re.DOTALL to match multi-line imports with {…} spanning lines
    # Pattern 1: named imports  import { ... } from "path";
    named_re = re.compile(
        r'import\s+\{([^}]*)\}\s+from\s+["\']([^"\']+)["\']',
        re.DOTALL
    )
    for m in named_re.finditer(content):
        names_part = m.group(1)
        import_path = m.group(2).strip()
        names = []
        for entry in names_part.split(","):
            entry = entry.strip()
            if not entry:
                continue
            parts = entry.split()
            original = parts[0]
            local = parts[2] if len(parts) == 3 and parts[1] == "as" else original
            names.append((original, local))
        results.append((names, import_path))

    # Pattern 2: simple imports  import "path";  (no braces, no from)
    simple_re = re.compile(r'import\s+["\']([^"\']+)["\']')
    for m in simple_re.finditer(content):
        # Avoid matching the 'from "path"' part of named imports
        prefix = content[:m.start()]
        if prefix.rstrip().endswith("from"):
            continue
        import_path = m.group(1).strip()
        results.append((None, import_path))

    # Pattern 3: import * as X from "path";
    star_re = re.compile(r'import\s+\*\s+as\s+(\w+)\s+from\s+["\']([^"\']+)["\']')
    for m in star_re.finditer(content):
        alias = m.group(1)
        import_path = m.group(2).strip()
        results.append(([(alias, alias)], import_path))

    return results


def _find_import_file_for_symbol(current_file, symbol_name, project_path, _visited=None):
    """Scan ALL import statements in current_file for one that provides symbol_name.
    Returns the resolved file path or None.

    Handles single-line and multi-line imports:
      import { SimpleTransfer, SimpleTransferType } from "path/to/file.sol";
      import {
          PartyMatchPayload,
          MatchProofs
      } from "darkpoolv1-types/Settlement.sol";
      import { Foo as Bar } from "other.sol";
      import "path.sol";   (wildcard — search inside file, follows transitive imports)
    """
    if _visited is None:
        _visited = set()
    if not current_file or not os.path.isfile(current_file):
        return None
    if current_file in _visited:
        return None
    _visited.add(current_file)

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    imports = _parse_all_imports(content)

    # First pass: check named imports
    for names, import_path in imports:
        if names is None:
            continue
        for original, local in names:
            if symbol_name == original or symbol_name == local:
                resolved = _resolve_import_file_path(import_path, current_file, project_path)
                if resolved:
                    return resolved

    # Second pass: check wildcard imports (import "path.sol";) by searching inside
    # the file.  If the symbol is not directly defined there, follow the imported
    # file's own imports transitively (e.g. ERC20.sol → IERC20.sol).
    for names, import_path in imports:
        if names is not None:
            continue
        resolved = _resolve_import_file_path(import_path, current_file, project_path)
        if resolved:
            found_pos = _find_symbol_in_file(resolved, symbol_name)
            if found_pos:
                return resolved
            # Follow transitive imports within the wildcard-imported file
            transitive = _find_import_file_for_symbol(resolved, symbol_name, project_path, _visited)
            if transitive:
                return transitive

    return None


def _extract_word_at(line_text, char_pos):
    """Extract the identifier at the given character position in a line."""
    if not line_text or char_pos < 0:
        return None
    if char_pos >= len(line_text):
        char_pos = len(line_text) - 1
    if char_pos < 0:
        return None

    start = char_pos
    while start > 0 and (line_text[start - 1].isalnum() or line_text[start - 1] == '_'):
        start -= 1
    end = char_pos
    while end < len(line_text) and (line_text[end].isalnum() or line_text[end] == '_'):
        end += 1

    word = line_text[start:end]
    if not word or not (word[0].isalpha() or word[0] == '_'):
        return None
    return word


def _get_symbol_at_cursor(current_file, cursor_pos):
    """Read the current file and extract the symbol name under the cursor."""
    if not current_file or not os.path.isfile(current_file):
        return None

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    lines = content.splitlines()
    line_num = cursor_pos.get("line", -1) if isinstance(cursor_pos, dict) else -1
    char_num = cursor_pos.get("character", 0) if isinstance(cursor_pos, dict) else 0
    if line_num < 0 or line_num >= len(lines):
        return None

    word = _extract_word_at(lines[line_num], char_num)
    return word


_SOLIDITY_DEF_KEYWORDS = r'\b(?:contract|interface|library|struct|enum|error|event|function|modifier|type)\s+'


def _find_symbol_in_file(filepath, symbol_name):
    """Search a single .sol file for a definition of the given symbol.
    Returns {"line": N, "character": N} or None."""
    try:
        with open(filepath, encoding="utf-8", errors="ignore") as f:
            file_content = f.read()
    except OSError:
        return None

    definition_pattern = re.compile(
        _SOLIDITY_DEF_KEYWORDS + re.escape(symbol_name) + r'\b'
    )
    for i, fline in enumerate(file_content.splitlines()):
        m = definition_pattern.search(fline)
        if m:
            return {"line": i, "character": m.start()}

    return None


def _find_struct_field_in_file(filepath, struct_name, field_name):
    """Search a .sol file for a field within a struct definition.
    Returns {"line": N, "character": N} or None."""
    try:
        with open(filepath, encoding="utf-8", errors="ignore") as f:
            content = f.read()
    except OSError:
        return None

    lines = content.splitlines()
    inside_struct = False
    brace_depth = 0
    struct_pattern = re.compile(r'\bstruct\s+' + re.escape(struct_name) + r'\s*\{')
    field_pattern = re.compile(r'\b' + re.escape(field_name) + r'\b')

    for i, line in enumerate(lines):
        if not inside_struct:
            m = struct_pattern.search(line)
            if m:
                inside_struct = True
                brace_depth = line.count('{') - line.count('}')
                # Check if field is on the same line as struct opening
                fm = field_pattern.search(line, m.end())
                if fm:
                    return {"line": i, "character": fm.start()}
        else:
            brace_depth += line.count('{') - line.count('}')
            fm = field_pattern.search(line)
            if fm:
                return {"line": i, "character": fm.start()}
            if brace_depth <= 0:
                inside_struct = False

    return None


def _resolve_struct_field_definition(current_file, cursor_pos, symbol_name, project_path):
    """Resolve a struct field access like `v2templates[templateId].period`
    or `someStruct.fieldName`.

    Detects the `.field` pattern, walks backward past brackets to find the
    root variable name, resolves its type from the declaration (handles
    mapping, array, and simple types), then locates the field within the
    struct definition.

    Returns (filepath, {"line": N, "character": N}) or None.
    """
    if not symbol_name or not current_file or not os.path.isfile(current_file):
        return None

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    lines = content.splitlines()
    line_num = cursor_pos.get("line", -1) if isinstance(cursor_pos, dict) else -1
    char_num = cursor_pos.get("character", 0) if isinstance(cursor_pos, dict) else 0
    if line_num < 0 or line_num >= len(lines):
        return None

    line_text = lines[line_num]

    # Find start of the symbol (field name)
    sym_start = char_num
    while sym_start > 0 and (line_text[sym_start - 1].isalnum() or line_text[sym_start - 1] == '_'):
        sym_start -= 1

    # Check if preceded by '.'
    dot_pos = sym_start - 1
    while dot_pos >= 0 and line_text[dot_pos] == ' ':
        dot_pos -= 1

    if dot_pos < 0 or line_text[dot_pos] != '.':
        return None  # Not a field access

    # Walk backward past brackets/indices to find the root variable name
    pos = dot_pos - 1
    while pos >= 0 and line_text[pos] == ' ':
        pos -= 1

    if pos < 0:
        return None

    # Skip past bracketed expressions: e.g. [templateId]
    while pos >= 0 and line_text[pos] == ']':
        bracket_depth = 1
        pos -= 1
        while pos >= 0 and bracket_depth > 0:
            if line_text[pos] == ']':
                bracket_depth += 1
            elif line_text[pos] == '[':
                bracket_depth -= 1
            pos -= 1
        # Skip whitespace after removing brackets
        while pos >= 0 and line_text[pos] == ' ':
            pos -= 1

    if pos < 0:
        return None

    # Extract the variable name
    var_end = pos + 1
    while pos >= 0 and (line_text[pos].isalnum() or line_text[pos] == '_'):
        pos -= 1
    pos += 1
    var_name = line_text[pos:var_end]
    if not var_name or not (var_name[0].isalpha() or var_name[0] == '_'):
        return None

    # Find the variable declaration to get its type
    _sol_keywords = {
        'function', 'event', 'modifier', 'return', 'returns', 'if', 'else',
        'for', 'while', 'do', 'mapping', 'emit', 'require', 'assert', 'revert',
        'new', 'delete', 'true', 'false', 'import', 'pragma', 'using',
        'contract', 'interface', 'library', 'struct', 'enum', 'super', 'this',
    }

    var_type = None

    # Pattern for mapping: mapping(... => TypeName) ... varName
    mapping_pattern = re.compile(
        r'\bmapping\s*\([^)]*=>\s*(\w+)\s*\)\s+'
        r'(?:(?:public|private|internal|external|immutable|constant|override)\s+)*'
        + re.escape(var_name) + r'\b'
    )
    # Pattern for array: TypeName[] ... varName
    array_pattern = re.compile(
        r'\b(\w+)\s*\[\s*\]\s+'
        r'(?:(?:public|private|internal|external|immutable|constant|override|memory|storage|calldata)\s+)*'
        + re.escape(var_name) + r'\b'
    )
    # Pattern for simple: TypeName ... varName
    simple_pattern = re.compile(
        r'\b(\w+)\s+'
        r'(?:(?:public|private|internal|external|immutable|constant|override|memory|storage|calldata)\s+)*'
        + re.escape(var_name) + r'(?:\s*[;=,)\[]|$)'
    )

    for fline in lines:
        m = mapping_pattern.search(fline)
        if m:
            var_type = m.group(1)
            break
        m = array_pattern.search(fline)
        if m and m.group(1) not in _sol_keywords:
            var_type = m.group(1)
            break
        m = simple_pattern.search(fline)
        if m and m.group(1) not in _sol_keywords:
            var_type = m.group(1)
            break

    if not var_type:
        return None

    # Search for the struct field — first in the current file
    found = _find_struct_field_in_file(current_file, var_type, symbol_name)
    if found:
        return (current_file, found)

    # Then via imports
    type_file = _find_import_file_for_symbol(current_file, var_type, project_path)
    if type_file:
        found = _find_struct_field_in_file(type_file, var_type, symbol_name)
        if found:
            return (type_file, found)

    # Project-wide search for the struct
    search_root = project_path if project_path and os.path.isdir(project_path) else os.path.dirname(current_file)
    struct_def_pat = re.compile(r'\bstruct\s+' + re.escape(var_type) + r'\s*\{')
    skip_dirs = {"node_modules", ".git", "cache", "artifacts", "out", "build", "__pycache__"}
    for dirpath, dirnames, filenames in os.walk(search_root):
        dirnames[:] = [d for d in dirnames if d not in skip_dirs]
        for fname in filenames:
            if not fname.endswith(".sol"):
                continue
            fpath = os.path.join(dirpath, fname)
            try:
                with open(fpath, encoding="utf-8", errors="ignore") as f:
                    fc = f.read()
            except OSError:
                continue
            if struct_def_pat.search(fc):
                found = _find_struct_field_in_file(fpath, var_type, symbol_name)
                if found:
                    return (fpath, found)

    return None


def _resolve_method_call_definition(current_file, cursor_pos, symbol_name, project_path):
    """When cursor is on a method name in a call like `obj.method(...)`,
    resolve by finding the receiver's type, then look for the function
    definition in that type's source file.

    Returns (filepath, {"line": N, "character": N}) or None.
    """
    if not symbol_name or not current_file or not os.path.isfile(current_file):
        return None

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    lines = content.splitlines()
    line_num = cursor_pos.get("line", -1) if isinstance(cursor_pos, dict) else -1
    char_num = cursor_pos.get("character", 0) if isinstance(cursor_pos, dict) else 0
    if line_num < 0 or line_num >= len(lines):
        return None

    line_text = lines[line_num]

    # Locate the symbol in the line and check if it is preceded by '.'
    sym_start = char_num
    while sym_start > 0 and (line_text[sym_start - 1].isalnum() or line_text[sym_start - 1] == '_'):
        sym_start -= 1

    dot_pos = sym_start - 1
    # skip whitespace between receiver and dot
    while dot_pos >= 0 and line_text[dot_pos] == ' ':
        dot_pos -= 1

    if dot_pos < 0 or line_text[dot_pos] != '.':
        return None  # Not a method call

    # Extract the receiver name (word before the dot)
    recv_end = dot_pos  # dot_pos points at '.'
    recv_pos = recv_end - 1
    while recv_pos >= 0 and (line_text[recv_pos].isalnum() or line_text[recv_pos] == '_'):
        recv_pos -= 1
    recv_pos += 1
    receiver_name = line_text[recv_pos:recv_end]
    if not receiver_name or not receiver_name[0].isalpha():
        return None

    # Find the type of the receiver variable in the current file.
    # Patterns: `TypeName varName`, `TypeName visibility varName`, `TypeName immutable varName`, etc.
    _sol_modifiers = r'(?:(?:public|private|internal|external|immutable|constant|override|payable|memory|storage|calldata)\s+)*'
    type_pattern = re.compile(
        r'\b(\w+)\s+' + _sol_modifiers + re.escape(receiver_name) + r'(?:\s*[;=,)\[]|$)'
    )

    _sol_keywords = {
        'function', 'event', 'modifier', 'return', 'returns', 'if', 'else',
        'for', 'while', 'do', 'mapping', 'emit', 'require', 'assert', 'revert',
        'new', 'delete', 'true', 'false', 'import', 'pragma', 'using',
        'contract', 'interface', 'library', 'struct', 'enum',
    }

    receiver_type = None
    for fline in lines:
        m = type_pattern.search(fline)
        if m:
            candidate = m.group(1)
            if candidate not in _sol_keywords:
                receiver_type = candidate
                break

    if not receiver_type:
        return None

    # Find the source file for the receiver type (via imports / project search)
    type_file = _find_import_file_for_symbol(current_file, receiver_type, project_path)
    if not type_file:
        # Try current file itself (type may be defined locally)
        if _find_symbol_in_file(current_file, receiver_type):
            type_file = current_file

    if not type_file:
        # Broader project search for the type definition
        search_root = project_path if project_path and os.path.isdir(project_path) else os.path.dirname(current_file)
        type_def_pat = re.compile(
            r'\b(?:contract|interface|library|abstract\s+contract)\s+' + re.escape(receiver_type) + r'\b'
        )
        skip_dirs = {"node_modules", ".git", "cache", "artifacts", "out", "build", "__pycache__"}
        for dirpath, dirnames, filenames in os.walk(search_root):
            dirnames[:] = [d for d in dirnames if d not in skip_dirs]
            for fname in filenames:
                if not fname.endswith(".sol"):
                    continue
                fpath = os.path.join(dirpath, fname)
                try:
                    with open(fpath, encoding="utf-8", errors="ignore") as f:
                        fc = f.read()
                except OSError:
                    continue
                if type_def_pat.search(fc):
                    type_file = fpath
                    break
            if type_file:
                break

    if not type_file:
        return None

    # Search for the function/event/error definition in the type's source file.
    found_pos = _find_symbol_in_file(type_file, symbol_name)
    if found_pos:
        return (type_file, found_pos)

    # The interface might inherit from another interface; scan the type file's imports too.
    try:
        with open(type_file, encoding="utf-8", errors="ignore") as f:
            type_content = f.read()
    except OSError:
        return None

    type_project = project_path
    for _, imp_path in _parse_all_imports(type_content):
        resolved = _resolve_import_file_path(imp_path, type_file, type_project)
        if resolved:
            fp = _find_symbol_in_file(resolved, symbol_name)
            if fp:
                return (resolved, fp)

    return None


def _resolve_inherited_function(current_file, symbol_name, project_path):
    """Resolve a function defined in a parent contract via the `is` inheritance chain.

    For example, when MyToken calls `_mint(...)` and `_mint` is defined in ERC20,
    this function parses `contract MyToken is ERC20, ERC20Burnable, ...`, resolves
    each parent type to its source file, and searches for `function _mint` there.
    Recurses through multi-level inheritance (depth limit 30).

    Returns (filepath, {"line": N, "character": N}) or None.
    """
    if not symbol_name or not current_file or not os.path.isfile(current_file):
        return None

    # BFS/DFS through the inheritance chain
    visited = set()
    queue = [current_file]
    depth = 0
    max_depth = 30

    while queue and depth < max_depth:
        depth += 1
        next_queue = []
        for src_file in queue:
            if src_file in visited:
                continue
            visited.add(src_file)

            try:
                with open(src_file, encoding="utf-8", errors="ignore") as f:
                    content = f.read()
            except OSError:
                continue

            # Find `contract/abstract contract/interface/library X is A, B(args), C {`
            inherit_re = re.compile(
                r'\b(?:abstract\s+)?(?:contract|interface|library)\s+\w+\s+is\s+([^{]+)\{',
                re.DOTALL
            )
            for m in inherit_re.finditer(content):
                parents_str = m.group(1)
                # Split parents by comma, strip constructor args: "ERC20(\"N\", \"S\")" → "ERC20"
                for parent_entry in parents_str.split(","):
                    parent_entry = parent_entry.strip()
                    if not parent_entry:
                        continue
                    # Remove constructor arguments: ERC20("Name", "SYM") → ERC20
                    paren_idx = parent_entry.find("(")
                    parent_name = parent_entry[:paren_idx].strip() if paren_idx >= 0 else parent_entry.strip()
                    # Clean up whitespace / newlines
                    parent_name = parent_name.split()[-1] if parent_name.split() else parent_name
                    if not parent_name or not (parent_name[0].isalpha() or parent_name[0] == '_'):
                        continue

                    # Resolve parent type to its source file
                    parent_file = _find_import_file_for_symbol(src_file, parent_name, project_path)
                    if not parent_file:
                        # Try current directory / project search for the type
                        pos = _find_symbol_in_file(src_file, parent_name)
                        if pos:
                            parent_file = src_file
                    if not parent_file:
                        continue

                    # Search for the function/modifier/event in the parent file
                    found_pos = _find_symbol_in_file(parent_file, symbol_name)
                    if found_pos:
                        return (parent_file, found_pos)

                    # Queue this parent for further traversal
                    if parent_file not in visited:
                        next_queue.append(parent_file)

        queue = next_queue

    return None


def _find_solidity_symbol_in_project(current_file, cursor_pos, project_path):
    """When the Solidity LSP returns file:/// and the cursor is not on an import line,
    try to locate the symbol definition by name in project .sol files."""
    word = _get_symbol_at_cursor(current_file, cursor_pos)
    if not word:
        return None

    search_root = project_path if project_path and os.path.isdir(project_path) else os.path.dirname(current_file)

    # Do not skip lib/ — Foundry/Hardhat dependencies often contain needed definitions.
    skip_dirs = {"node_modules", ".git", "cache", "artifacts", "out", "build", "__pycache__"}
    definition_pattern = re.compile(
        _SOLIDITY_DEF_KEYWORDS + re.escape(word) + r'\b'
    )

    for dirpath, dirnames, filenames in os.walk(search_root):
        dirnames[:] = [d for d in dirnames if d not in skip_dirs]
        for fname in filenames:
            if not fname.endswith(".sol"):
                continue
            fpath = os.path.join(dirpath, fname)
            try:
                with open(fpath, encoding="utf-8", errors="ignore") as f:
                    file_content = f.read()
            except OSError:
                continue
            for i, fline in enumerate(file_content.splitlines()):
                m = definition_pattern.search(fline)
                if m:
                    return (fpath, {"line": i, "character": m.start()})

    return None


def _get_solidity_server_name(obj):
    """Extract the LSP server name from file action."""
    server_name = ""
    if getattr(obj.file_action, "single_server", None) is not None:
        server_name = obj.file_action.single_server.server_info.get("name", "")
    elif getattr(obj.file_action, "multi_servers", None) is not None:
        server_name = ",".join(obj.file_action.get_lsp_server_names())
    return server_name


def _try_solidity_fallback(obj, define_jump_handler):
    """Attempt Solidity-specific definition recovery.
    Called when the LSP returns empty response or file:///.
    Returns True if recovery succeeded and jump was performed."""
    current_file = obj.file_action.filepath
    if not current_file or not current_file.endswith(".sol"):
        return False

    project_path = ""
    try:
        raw = obj.file_action.get_lsp_server_project_path()
        project_path = raw.decode("utf-8") if isinstance(raw, bytes) else str(raw)
    except Exception:
        project_path = ""

    # If the LSP didn't provide a valid project root, detect it from the file path.
    if not project_path or not os.path.isdir(project_path):
        project_path = _detect_solidity_project_root(current_file)

    symbol_name = _get_symbol_at_cursor(current_file, obj.pos)

    # ── Strategy 1: cursor is on an import line → resolve the import file path ──
    recovered_file = _resolve_solidity_import_path(current_file, obj.pos, project_path)
    if recovered_file:
        jump_pos = {"line": 0, "character": 0}
        if symbol_name:
            found_pos = _find_symbol_in_file(recovered_file, symbol_name)
            if found_pos:
                jump_pos = found_pos
        obj.file_action.create_external_file_action(recovered_file)
        eval_in_emacs(define_jump_handler, recovered_file, get_lsp_file_host(), jump_pos)
        return True

    # ── Strategy 2: scan all imports in current file for a named import of this symbol ──
    if symbol_name:
        import_file = _find_import_file_for_symbol(current_file, symbol_name, project_path)
        if import_file:
            found_pos = _find_symbol_in_file(import_file, symbol_name)
            jump_pos = found_pos or {"line": 0, "character": 0}
            obj.file_action.create_external_file_action(import_file)
            eval_in_emacs(define_jump_handler, import_file, get_lsp_file_host(), jump_pos)
            return True

    # ── Strategy 2.5: method call resolution (receiver.method) ──
    if symbol_name:
        result = _resolve_method_call_definition(current_file, obj.pos, symbol_name, project_path)
        if result:
            target_file, target_pos = result
            obj.file_action.create_external_file_action(target_file)
            eval_in_emacs(define_jump_handler, target_file, get_lsp_file_host(), target_pos)
            return True

    # ── Strategy 2.6: struct field resolution (var[idx].field, var.field) ──
    if symbol_name:
        result = _resolve_struct_field_definition(current_file, obj.pos, symbol_name, project_path)
        if result:
            target_file, target_pos = result
            obj.file_action.create_external_file_action(target_file)
            eval_in_emacs(define_jump_handler, target_file, get_lsp_file_host(), target_pos)
            return True

    # ── Strategy 2.7: inheritance chain resolution ──
    if symbol_name:
        result = _resolve_inherited_function(current_file, symbol_name, project_path)
        if result:
            target_file, target_pos = result
            obj.file_action.create_external_file_action(target_file)
            eval_in_emacs(define_jump_handler, target_file, get_lsp_file_host(), target_pos)
            return True

    # ── Strategy 3: project-wide symbol search ──
    result = _find_solidity_symbol_in_project(current_file, obj.pos, project_path)
    if result:
        recovered_file, start_position = result
        obj.file_action.create_external_file_action(recovered_file)
        eval_in_emacs(define_jump_handler, recovered_file, get_lsp_file_host(), start_position)
        return True

    return False


def find_define_response(obj, response, define_jump_handler) -> None:
    if not response:
        # For Solidity: empty response is common for named imports — try recovery.
        if _try_solidity_fallback(obj, define_jump_handler):
            return
        eval_in_emacs("lsp-bridge-find-def-fallback", obj.pos)
        return

    file_info = response[0] if isinstance(response, list) else response
    # volar return only LocationLink (using targetUri)
    file_uri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
    define_range = file_info["range"] if "range" in file_info else file_info["targetRange"]
    start_pos = define_range["start"]

    if file_uri.startswith("jdt://"):
        # for java
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "jdt_uri_resolver", file_uri, start_pos, define_jump_handler)
    elif file_uri.startswith("csharp:/metadata/"):
        # for csharp
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "csharp_uri_resolver", file_uri, start_pos, define_jump_handler)
    elif file_uri.startswith("jar://"):
        # for clojure
        raise NotImplementedError()
    elif file_uri.startswith("deno:"):
        # for deno
        # Deno will return targetUri like "deno:asset/lib.deno.ns.d.ts",
        # so we need send server deno/virtualTextDocument to request virtual text document from Deno.
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "deno_uri_resolver", file_uri, start_pos, define_jump_handler)
    else:
        # for normal file uri
        filepath = uri_to_path(file_uri)
        if not filepath or os.path.isdir(filepath):
            server_name = ""
            if getattr(obj.file_action, "single_server", None) is not None:
                server_name = obj.file_action.single_server.server_info.get("name", "")
            elif getattr(obj.file_action, "multi_servers", None) is not None:
                server_name = ",".join(obj.file_action.get_lsp_server_names())

            if file_uri == "file:///":
                # Solidity (and possibly other servers) return file:/// as a "no definition" placeholder.
                if _try_solidity_fallback(obj, define_jump_handler):
                    return

                message_emacs("No definition found: language server returned root URI placeholder (file:///).")

            eval_in_emacs("lsp-bridge-find-def-fallback", obj.pos)
            return
        obj.file_action.create_external_file_action(filepath)
        eval_in_emacs(define_jump_handler, filepath, get_lsp_file_host(), start_pos)

def create_decompile_external_file(uri_resolver, language_dir, decompile_dir, source_code):
    '''Some LSP server, such as Java or C#, LSP server need decompile source code to *external* file before jump to definition in *original* file.
    Base on lsp-bridge default policy, lsp-bridge will create NEW LSP server for *external* file if *external* file own by different directory.
    If *external* file use *new* LSP server, `lsp-bridge-find-def` won't response in *external* file,
    because *new* LSP server don't know project information about *original* project.

    So we call `create_external_file_action` on *external* file, pass *original* file's LSP server to FileAction structural function,
    to make sure *external* file share SAME LSP server with *original* file.

    Last, we can do `lsp-bridge-find-def` in *external* file.
    '''
    # Init.
    external_file_dir = ''

    # Build temp directory base on project hash.
    md5 = hashlib.md5()
    md5.update(uri_resolver.file_action.get_lsp_server_project_path())
    project_hash = md5.hexdigest()
    data_dir = pathlib.Path(os.path.join(tempfile.gettempdir(), language_dir, project_hash))
    external_file_dir = data_dir / decompile_dir

    # Build temp file path.
    url = urlparse(uri_resolver.external_file_link)
    path = unquote(url.path)[1:] # remove first / from path to make `external_file_dir` can join `path` at below
    external_file = external_file_dir / path

    # Create temp file if is not exists.
    external_file.parent.mkdir(exist_ok=True, parents=True)

    # Write source code to temp file.
    if not external_file.exists():
        with open(external_file, 'w') as f:
            f.write(source_code)

    external_file = external_file.as_posix()

    # Create external file action, make sure external file share one LSP server with current file.
    uri_resolver.file_action.create_external_file_action(external_file, uri_resolver.external_file_link)

    return external_file
