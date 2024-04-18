import json
import os

from aiohttp import web


def load_json_files():
    """
    Load all json files from spec directory
    """
    data = {}
    for filename in os.listdir("spec/"):
        if filename.endswith(".json"):
            with open(f"spec/{filename}", "r") as f:
                data.update(json.load(f))
    return data


async def handle(request):
    """
    Read test data from spec file and execute the method by test id
    """
    spec = load_json_files()
    test_id = request.headers.get("User-Agent")
    test_data = spec.get(test_id, {})
    data = await request.json()

    assert test_data.get("method") == data.get("method")
    assert test_data.get("params") == data.get("params")

    response = {
        "jsonrpc": "2.0",
        "id": data.get("id"),
        **test_data.get("expected_result", {}),
    }

    return web.json_response(response)


app = web.Application()
app.router.add_post("/", handle)

web.run_app(app, port=5000)
