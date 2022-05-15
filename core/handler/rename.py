from core.handler import Handler
from core.utils import *


def get_rename_line_content(edit_info, line_offset_dict, lines, replace_line):
    # Get current line offset, if previous edit is same as current line.
    # We need add changed offset to make sure current edit has right column.
    replace_line_offset = 0
    if replace_line in line_offset_dict:
        replace_line_offset = line_offset_dict[replace_line]
    else:
        line_offset_dict[replace_line] = 0

    # Calculate replace column offset.
    replace_column_start = edit_info["range"]["start"]["character"] + replace_line_offset
    replace_column_end = edit_info["range"]["end"]["character"] + replace_line_offset

    # Get current line.
    line_content = lines[replace_line]

    # Get new changed offset.
    new_text = edit_info["newText"]
    replace_offset = len(new_text) - (replace_column_end - replace_column_start)

    # Overlapping new changed offset.
    line_offset_dict[replace_line] = line_offset_dict[replace_line] + replace_offset

    # Replace current line.
    new_line_content = line_content[:replace_column_start] + new_text + line_content[replace_column_end:]

    return new_line_content


def rename_symbol_in_file_changes(rename_info):
    rename_file = rename_info[0]
    if rename_file.startswith("file://"):
        rename_file = uri_to_path(rename_file)

    rename_counter = 0

    with open(rename_file, "r", encoding="utf-8") as f:
        lines = f.readlines()

        line_offset_dict = {}

        edits = rename_info[1]
        for edit_info in edits:
            # Get replace line.
            replace_line = edit_info["range"]["start"]["line"]
            lines[replace_line] = get_rename_line_content(edit_info, line_offset_dict, lines, replace_line)

            rename_counter += 1

    with open(rename_file, "w", encoding="utf-8") as f:
        f.writelines(lines)

    return rename_file, rename_counter


def rename_symbol_in_file__document_changes(rename_info):
    rename_file = rename_info["textDocument"]["uri"]
    if rename_file.startswith("file://"):
        rename_file = uri_to_path(rename_file)

    lines = []
    rename_counter = 0

    with open(rename_file, "r", encoding="utf-8") as f:
        lines = f.readlines()

        line_offset_dict = {}

        edits = rename_info["edits"]
        for edit_info in edits:
            # Get replace line.
            replace_line = edit_info["range"]["start"]["line"]
            lines[replace_line] = get_rename_line_content(edit_info, line_offset_dict, lines, replace_line)

            rename_counter += 1

    with open(rename_file, "w", encoding="utf-8") as f:
        f.writelines(lines)

    return rename_file, rename_counter


class Rename(Handler):
    name = "rename"
    method = "textDocument/rename"

    def process_request(self, position, new_name) -> dict:
        return dict(position=position, newName=new_name)

    def process_response(self, response: dict) -> None:
        if response is None:
            logger.info("No rename found.")

        counter = 0
        rename_files = []

        rename_infos = response["documentChanges"] if "documentChanges" in response else \
            response["changes"]

        if type(rename_infos) == dict:
            # JSON struct is 'changes'
            for rename_info in rename_infos.items():
                (rename_file, rename_counter) = rename_symbol_in_file_changes(rename_info)
                rename_files.append(rename_file)
                counter += rename_counter
        else:
            # JSON struct is 'documentChanges'
            for rename_info in rename_infos:
                (rename_file, rename_counter) = rename_symbol_in_file__document_changes(rename_info)
                rename_files.append(rename_file)
                counter += rename_counter

        eval_in_emacs("lsp-bridge-rename-finish", [rename_files, counter])