# -*- coding: utf-8 -*-
"""
Volar Proxy Handler for Hybrid Mode.

https://github.com/vuejs/language-tools/wiki/Neovim
"""
import traceback
import threading
from core.utils import logger, generate_request_id
from core.handler import Handler


class TsserverRequestHandler(Handler):
    """Minimal handler to receive tsserverRequest responses from TS LSP server."""
    name = "tsserver_request_proxy"
    method = "typescript.tsserverRequest"
    cancel_on_change = False
    send_document_uri = False

    def __init__(self, file_action):
        super().__init__(file_action)
        self.volar_server = None
        self.seq_id = None
        self._timeout_timer = None
        self._responded = False

    def process_request(self, *args, **kwargs) -> dict:
        return {}

    def _send_response_once(self, body) -> None:
        """Send tsserver/response back to volar exactly once."""
        if self._responded:
            return
        self._responded = True

        if self._timeout_timer:
            self._timeout_timer.cancel()
            self._timeout_timer = None

        # Always respond, even if body is None (prevents memory leak in volar).
        self.volar_server.sender.send_notification(
            "tsserver/response",
            [[self.seq_id, body]]
        )

    def on_timeout(self) -> None:
        """Timeout fallback: respond nil to volar."""
        logger.debug(f"VolarProxy: seq={self.seq_id} timed out, respond nil")
        self._send_response_once(None)

    def process_response(self, response: dict) -> None:
        """Process response from TS LSP server and forward to volar."""
        # Neovim: return r.body to vue_ls
        body = response.get("body") if isinstance(response, dict) else response
        self._send_response_once(body)


class VolarProxy:
    """Proxy handler for forwarding tsserver requests from Volar to TS LSP server."""

    @staticmethod
    def _find_ts_server(volar_server):
        """Find vtsls/ts_ls/typescript server from attached files."""
        for fa in volar_server.files.values():
            if hasattr(fa, "multi_servers") and fa.multi_servers:
                ts_server = (fa.multi_servers.get("vtsls") or
                             fa.multi_servers.get("ts_ls") or
                             fa.multi_servers.get("typescript"))
                if ts_server:
                    return ts_server, fa
        return None, None

    @staticmethod
    def handle_tsserver_request(volar_server, params):
        """
        Handle tsserver/request notification from volar and forward to TS LSP server.

        params format: [[id, command, payload]]
        """
        try:
            # Parse params: [[id, command, payload]]
            param = params[0] if isinstance(params, list) and len(params) > 0 else None
            if not isinstance(param, list) or len(param) < 3:
                logger.error(f"VolarProxy: Invalid params: {params}")
                return

            seq_id, command, payload = param[0], param[1], param[2]

            # Find TS server
            ts_server, file_action = VolarProxy._find_ts_server(volar_server)
            if not ts_server:
                logger.error("VolarProxy: Could not find vtsls/ts_ls/typescript server")
                volar_server.sender.send_notification("tsserver/response", [[seq_id, None]])
                return

            # Create handler with 200ms timeout
            request_id = generate_request_id()
            handler = TsserverRequestHandler(file_action)
            handler.volar_server = volar_server
            handler.seq_id = seq_id
            handler.latest_request_id = request_id
            handler._timeout_timer = threading.Timer(0.2, handler.on_timeout)
            handler._timeout_timer.daemon = True
            handler._timeout_timer.start()

            # Register handler to receive response
            ts_server.record_request_id(request_id, handler)

            # Forward request to TS server
            ts_server.sender.send_request(
                method="workspace/executeCommand",
                params={
                    "command": "typescript.tsserverRequest",
                    "arguments": [command, payload]
                },
                request_id=request_id
            )

            logger.debug(f"VolarProxy: Forwarded seq={seq_id}, command={command}")

        except Exception as e:
            logger.error(f"VolarProxy: Error: {e}")
            logger.error(traceback.format_exc())
