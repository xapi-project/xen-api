"""
Module Name:
    jsonrpc_server
Description:
    This module provides a simple JSON-RPC server implementation using aiohttp.
"""

import json
import os

from aiohttp import web  # pytype: disable=import-error


def load_json_files():
    """
    Load all JSON files from the 'spec' directory and merge their contents
    into a dictionary.

    Returns:
        dict: A dictionary containing the merged contents of all JSON files.
    """
    data = {}
    for path, _, files in os.walk("spec/"):
        for name in files:
            filepath = os.path.join(path, name)
            if filepath.endswith(".json"):
                with open(filepath, "r", encoding="utf-8") as f:
                    data.update(json.load(f))

    return data


async def handle(request):
    """
    Handle incoming requests and execute methods based on the test ID provided
    in the request headers.

    Args:
        request (aiohttp.web.Request): The incoming HTTP request.

    Returns:
        aiohttp.web.Response: The HTTP response containing the JSON-RPC result.
    """
    spec = load_json_files()
    test_id = request.headers.get("Test-ID")
    test_data = spec.get(test_id, {})
    data = await request.json()

    try:
        assert data.get("method") in test_data.get("method")
        assert test_data.get("params")[data.get("method")] == data.get("params")
    except Exception:
        response = {
            "jsonrpc": "2.0",
            "id": data.get("id"),
            "error": {
                "code": 500,
                "message": "Rpc server failed to handle the client request!",
                "data": test_id,
            }
        }
    else:
        response = {
            "jsonrpc": "2.0",
            "id": data.get("id"),
            **test_data.get("expected_result")[data.get("method")],
        }
    return web.json_response(response)


app = web.Application()
app.router.add_post("/jsonrpc", handle)

if __name__ == "__main__":
    web.run_app(app, port=5000)
