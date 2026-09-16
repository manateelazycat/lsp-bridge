import unittest
from unittest.mock import Mock, patch

from lsp_bridge import LspBridge


class OrgBabelOpenFile(unittest.TestCase):
    def test_missing_server_does_not_load_configuration(self):
        bridge = Mock()
        bridge.copilot.is_initialized = False
        with patch("lsp_bridge.get_project_path", return_value="/tmp"), \
                patch("lsp_bridge.get_emacs_func_result") as query, \
                patch("lsp_bridge.load_single_server_info") as load:
            for missing in ([], None, ""):
                with self.subTest(server=missing):
                    query.side_effect = [[], missing]
                    self.assertFalse(LspBridge.open_file(bridge, "/tmp/example.org"))
                    load.assert_not_called()

    def test_valid_server_still_opens_file(self):
        bridge = Mock()
        bridge.copilot.is_initialized = False
        info = {"name": "basedpyright"}
        with patch("lsp_bridge.get_project_path", return_value="/tmp"), \
                patch("lsp_bridge.get_emacs_func_result", side_effect=[[], "basedpyright"]), \
                patch("lsp_bridge.load_single_server_info", return_value=info) as load, \
                patch("lsp_bridge.create_file_action_with_single_server") as create:
            self.assertTrue(LspBridge.open_file(bridge, "/tmp/example.org"))
            load.assert_called_once_with("basedpyright")
            create.assert_called_once_with(
                "/tmp/example.org", info, bridge.create_lsp_server.return_value)
