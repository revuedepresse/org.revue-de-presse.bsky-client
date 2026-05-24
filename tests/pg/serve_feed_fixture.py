#!/usr/bin/env python3
"""
Minimal static HTTP server for the SIGSEGV reproducer.

Serves the reconstructed feed-body.json on a chosen port so a Prolog
reproducer can drive the production http_open + json_chars + insert
path end-to-end without hitting the real Bluesky API. Every GET
returns the same body regardless of path or query string -- the
reproducer just needs the bytes.

Usage
-----
    PORT=8080 \\
    JSON_PATH=var/tmp/.../feed-body.json \\
    python3 tests/pg/serve_feed_fixture.py

The companion run_http_repro.sh starts this in the background,
runs the reproducer, and kills the server on exit.
"""
import http.server
import os
import socketserver
import sys

PORT = int(os.environ.get("PORT", "8080"))
JSON_PATH = os.environ.get(
    "JSON_PATH",
    "var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/feed-body.json",
)

if not os.path.isfile(JSON_PATH):
    sys.stderr.write(f"[KO] JSON_PATH does not exist: {JSON_PATH}\n")
    sys.exit(2)

with open(JSON_PATH, "rb") as handle:
    BODY = handle.read()


class JsonHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):  # noqa: N802 -- BaseHTTPRequestHandler API
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(BODY)))
        self.end_headers()
        self.wfile.write(BODY)

    def log_message(self, *_args, **_kwargs):
        # Quiet logging. Errors still go to stderr via log_error().
        pass


class ReusableTCPServer(socketserver.TCPServer):
    allow_reuse_address = True


with ReusableTCPServer(("127.0.0.1", PORT), JsonHandler) as srv:
    sys.stdout.write(
        f"[..] serving {len(BODY)} bytes of {JSON_PATH} at "
        f"http://127.0.0.1:{PORT}/\n"
    )
    sys.stdout.flush()
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        pass
