import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch

from watchdog.events import (
    DirModifiedEvent, FileCreatedEvent, FileDeletedEvent,
    FileModifiedEvent, FileMovedEvent,
)

from core.lspserver import LspServer, ProjectFileHandler
from core.utils import path_to_uri


class ProjectFileWatching(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name).resolve()
        self.server = Mock()
        self.handler = ProjectFileHandler(self.server, self.root, ["**/*.dconf"])
        self.addCleanup(self.handler.stop)
        self.timer_patch = patch("core.lspserver.threading.Timer")
        self.timer = self.timer_patch.start()
        self.addCleanup(self.timer_patch.stop)

    def test_batch_preserves_transitions_and_coalesces_duplicate_changes(self):
        path = str(self.root / "child.dconf")
        for event in (FileCreatedEvent(path), FileModifiedEvent(path),
                      FileModifiedEvent(path), FileDeletedEvent(path),
                      FileCreatedEvent(path)):
            self.handler.dispatch(event)
        self.server.send_workspace_did_change_watched_files_batch.assert_not_called()
        self.timer.assert_called_once()
        self.handler.flush()
        self.server.send_workspace_did_change_watched_files_batch.assert_called_once_with(
            [(path, 1), (path, 2), (path, 3), (path, 1)])
        self.handler.flush()
        self.assertEqual(self.server.send_workspace_did_change_watched_files_batch.call_count, 1)

    def test_filters_directories_other_extensions_and_paths_outside_project(self):
        for event in (DirModifiedEvent(str(self.root)),
                      FileModifiedEvent(str(self.root / "notes.txt")),
                      FileModifiedEvent(str(self.root.parent / "external.dconf"))):
            self.handler.dispatch(event)
        self.handler.flush()
        self.timer.assert_not_called()
        self.server.send_workspace_did_change_watched_files_batch.assert_not_called()

    def test_does_not_follow_symlinks_outside_project(self):
        with tempfile.TemporaryDirectory() as other:
            target = Path(other) / "external.dconf"
            target.write_text("[Script]\n", encoding="utf-8")
            link = self.root / "linked.dconf"
            try:
                link.symlink_to(target)
            except OSError:
                self.skipTest("File symlinks are unavailable")
            self.handler.dispatch(FileModifiedEvent(str(link)))
            self.handler.flush()
            self.server.send_workspace_did_change_watched_files_batch.assert_not_called()

    def test_rename_sends_delete_and_create_in_one_batch(self):
        old = str(self.root / "old.dconf")
        new = str(self.root / "nested" / "new.dconf")
        self.handler.dispatch(FileMovedEvent(old, new))
        self.handler.flush()
        self.server.send_workspace_did_change_watched_files_batch.assert_called_once_with(
            [(old, 3), (new, 1)])

    def test_stop_discards_pending_changes_and_late_callbacks(self):
        path = str(self.root / "child.dconf")
        self.handler.dispatch(FileModifiedEvent(path))
        self.handler.stop()
        self.timer.return_value.cancel.assert_called_once()
        self.handler.flush()
        self.handler.dispatch(FileModifiedEvent(path))
        self.handler.flush()
        self.server.send_workspace_did_change_watched_files_batch.assert_not_called()

    def test_opt_in_starts_only_one_watch_and_reuses_workspace_observer(self):
        server = LspServer.__new__(LspServer)
        server.project_path = str(self.root)
        server.project_file_watch_patterns = []
        server.project_file_watch_handler = None
        server.workspace_file_watcher = Mock()
        server.workspace_file_watch_handler = Mock()
        server.start_workspace_watch_files = Mock()
        server.start_project_watch_files()
        server.start_workspace_watch_files.assert_not_called()
        server.project_file_watch_patterns = ["**/*.dconf"]
        server.start_project_watch_files()
        handler = server.project_file_watch_handler
        assert isinstance(handler, ProjectFileHandler)
        server.start_project_watch_files()
        server.start_workspace_watch_files.assert_called_once()
        server.workspace_file_watcher.schedule.assert_called_once_with(
            handler, str(self.root), recursive=True)
        watcher = server.workspace_file_watcher
        handler.dispatch(FileModifiedEvent(str(self.root / "child.dconf")))
        server.stop_workspace_watch_files()
        watcher.unschedule_all.assert_called_once()
        watcher.stop.assert_called_once()
        watcher.join.assert_called_once()
        self.assertIsNone(server.workspace_file_watcher)
        self.assertIsNone(server.project_file_watch_handler)
        handler.flush()
        self.assertTrue(handler.stopped)
        server.stop_workspace_watch_files()

    def test_single_file_project_does_not_watch_parent_directory(self):
        path = self.root / "main.conf"
        path.touch()
        server = Mock()
        server.project_path = str(path)
        server.project_file_watch_patterns = ["**/*.dconf"]
        server.project_file_watch_handler = None
        LspServer.start_project_watch_files(server)
        server.start_workspace_watch_files.assert_not_called()

    def test_failed_schedule_stops_new_observer_and_reports_failure(self):
        server = LspServer.__new__(LspServer)
        self.addCleanup(server.stop_workspace_watch_files)
        root = self.root / "vanished"
        root.mkdir()
        server.project_path = str(root)
        server.project_file_watch_patterns = ["**/*.dconf"]
        server.project_file_watch_handler = None
        server.workspace_file_watcher = None
        server.workspace_file_watch_handler = None
        start = server.start_workspace_watch_files
        observers = []

        def disappear():
            start()
            observers.append(server.workspace_file_watcher)
            root.rmdir()

        with patch.object(server, "start_workspace_watch_files", side_effect=disappear), \
                patch("core.lspserver.message_emacs") as message:
            server.start_project_watch_files()
        message.assert_called_once()
        self.assertIn("Cannot watch project files", message.call_args.args[0])
        self.assertIsNone(server.project_file_watch_handler)
        self.assertIsNone(server.workspace_file_watcher)
        self.assertFalse(observers[0].is_alive())

    def test_failed_schedule_preserves_existing_dynamic_watcher(self):
        server = LspServer.__new__(LspServer)
        server.project_path = str(self.root)
        server.project_file_watch_patterns = ["**/*.dconf"]
        server.project_file_watch_handler = None
        server.workspace_file_watcher = Mock()
        server.workspace_file_watch_handler = Mock()
        watcher = server.workspace_file_watcher
        watcher.schedule.side_effect = OSError("Cannot watch directory")
        with patch("core.lspserver.message_emacs") as message:
            server.start_project_watch_files()
        message.assert_called_once()
        watcher.remove_handler_for_watch.assert_called_once()
        watcher.stop.assert_not_called()
        self.assertIs(server.workspace_file_watcher, watcher)
        self.assertIsNone(server.project_file_watch_handler)

    def test_exit_cleans_up_watchers(self):
        server = Mock()
        server.lsp_subprocess = None
        LspServer.exit(server)
        server.stop_workspace_watch_files.assert_called_once()

    def test_invalid_patterns_fail_before_starting_server_process(self):
        for patterns in ("**/*.dconf", [None], [""], ["/tmp/*.dconf"], ["../*.dconf"]):
            with self.subTest(patterns=patterns), patch("core.lspserver.subprocess.Popen") as popen:
                with self.assertRaisesRegex(ValueError, "watchFiles"):
                    LspServer(None, str(self.root), {"watchFiles": patterns}, "test", True)
                popen.assert_not_called()

    def test_batch_notification_preserves_event_types_and_escapes_uris(self):
        server = LspServer.__new__(LspServer)
        server.sender = Mock()
        path = str(self.root / "space # child.dconf")
        server.send_workspace_did_change_watched_files_batch([(path, 3), (path, 1)])
        server.sender.send_notification.assert_called_once_with(
            "workspace/didChangeWatchedFiles",
            {"changes": [{"uri": path_to_uri(path), "type": 3},
                         {"uri": path_to_uri(path), "type": 1}]})
