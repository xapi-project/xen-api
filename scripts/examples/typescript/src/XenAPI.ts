import { JSONRPCClient } from "json-rpc-2.0";

class XapiSession {
    private client: JSONRPCClient;
    private session_id: string | undefined;

    constructor(hostUrl: string | undefined) {
        this.client = new JSONRPCClient(async (request) => {
            const response = await fetch(`${hostUrl}/jsonrpc`, {
                method: "POST",
                headers: { "content-type": "application/json" },
                body: JSON.stringify(request),
            });
            if (response.status === 200) {
                const json = await response.json();
                return this.client.receive(json);
            } else if (request.id !== undefined) {
                return Promise.reject(new Error(response.statusText));
            }
        });
    }

    async login(method: string, args: any[] = []) {
        this.session_id = await this.client.request(`session.${method}`, [...args])
        return this.session_id
    }

    async xapi_request(method: string, args: any[] = []) {
        return await this.client.request(`${method}`, [this.session_id, ...args])
    }
}

function xapi_proxy(obj: XapiSession, path: any[] = []): any {
    return new Proxy(() => {}, {
        get (target, property) {
            return xapi_proxy(obj, path.concat(property))
        },
        apply (target, self, args) {
            if (path.length > 0){
                if(path[path.length-1].startsWith("login")) {
                    return obj.login(path[path.length-1], args)
                } else if (path[0].toLowerCase() == 'xenapi') {
                    return obj.xapi_request(path.slice(1).join('.'), args)
                }
            }
        }
    })
}

export function xapi_client(url: string|undefined) {
    return xapi_proxy(new XapiSession(url))
}
