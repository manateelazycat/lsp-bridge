import json
import os
import unittest
from pathlib import Path
from unittest.mock import patch

from core.utils import replace_template
from test.common import BASE_DIR

_LANG_SERVER_DIR = BASE_DIR / "langserver"
_MULTI_SERVER_DIR = BASE_DIR / "multiserver"


class LangServerJson(unittest.TestCase):
    def test_json_content(self) -> None:
        def _check_json(p: Path) -> None:
            with open(p, "r", encoding="utf-8") as f:
                json.load(f)

        for d in (_LANG_SERVER_DIR, _MULTI_SERVER_DIR):
            for f in d.iterdir():
                if f.is_file() and f.suffix == ".json":
                    with self.subTest(file=f):
                        _check_json(f)

    def test_powershell_editor_services_config(self) -> None:
        config_path = _LANG_SERVER_DIR / "powershell-editor-services.json"
        with open(config_path, "r", encoding="utf-8") as f:
            config = json.load(f)

        self.assertEqual(config["name"], "powershell-editor-services")
        self.assertEqual(config["languageId"], "powershell")
        self.assertIn("-Stdio", config["command"])
        self.assertIn(
            "$LSP_BRIDGE_POWERSHELL_EDITOR_SERVICES_DIR/PowerShellEditorServices/Start-EditorServices.ps1",
            config["command"],
        )
        self.assertIn("$LSP_BRIDGE_POWERSHELL_LOG_PATH", config["command"])
        self.assertIn(
            "$LSP_BRIDGE_POWERSHELL_SESSION_DETAILS_PATH", config["command"]
        )

    def test_command_template_expands_environment_variables(self) -> None:
        with patch.dict(os.environ, {"LSP_BRIDGE_TEST_DIR": "/tmp/lsp-bridge"}):
            self.assertEqual(
                replace_template("$LSP_BRIDGE_TEST_DIR/server"),
                "/tmp/lsp-bridge/server",
            )
