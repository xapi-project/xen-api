# server.py

from aiohttp import web
import mock_methods
import inspect

methods_dict = {name: func for name, func in inspect.getmembers(mock_methods, inspect.iscoroutinefunction)}

async def handle(request):
    data = await request.json()
    method_func = methods_dict.get(data.get('method'))

    if method_func:
        args = data.get('params', {})
        result = await method_func(**args)
        response = {
            'jsonrpc': '2.0',
            'result': result,
            'id': data.get('id'),
        }
    else:
        response = {
            'jsonrpc': '2.0',
            'error': {
                'code': -32601,
                'message': 'Method not found',
            },
            'id': data.get('id'),
        }

    return web.json_response(response)

app = web.Application()
app.router.add_post('/', handle)

web.run_app(app, port=5000)