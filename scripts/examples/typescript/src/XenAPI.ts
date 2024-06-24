import { JSONRPCClient } from "json-rpc-2.0";

class XapiSession {
	private client: JSONRPCClient;

	private sessionId: string = "";

	constructor (hostUrl: string) {
		this.client = new JSONRPCClient(async (request) => {
			const response = await fetch(`${hostUrl}/jsonrpc`, {
				method: "POST",
				headers: { 
                    "content-type": "application/json",
                    "user-agent": "xen api library"
                },
				body: JSON.stringify(request)
			});
			if (response.status === 200) {
				const json = await response.json();
				return this.client.receive(json);
			} else if (request.id !== undefined) {
				console.error("Connection error with url: ", hostUrl);
				return Promise.reject(new Error(response.statusText));
			}
		});
	}

	async loginWithPasswd (username: string, password: string): Promise<string> {
		this.sessionId = await this.client.request("session.login_with_password", [username, password]);
		return this.sessionId;
	}

	// we need to save session id into local storage and read it to login xapi after refresh the web browser
	async loginWithSession (sessionId: string): Promise<boolean> {
		this.sessionId = "";
		try {
			await this.client.request("session.get_all_subject_identifiers", [sessionId]);
			this.sessionId = sessionId;
			return true;
		} catch (error: any) {
			if (error?.message === "SESSION_INVALID") {
				return false;
			} else {
				throw error;
			}
		}
	}

	getSessionId (): string {
		return this.sessionId;
	}

	async xapiRequest (method: string, args: any[] = []): Promise<any> {
		try {
			return await this.client.request(method, [this.sessionId, ...args]);
		} catch (error: any) {
			console.error(`Failed to call ${method} with args [${args}]
			Error code: ${error?.code}
			Error message: ${error?.message}
			Error data: ${error?.data}`);
		}
	}
}

function xapi_proxy (obj: XapiSession, path: any[] = []): any {
	return new Proxy(() => { }, {
		get (_target, property) {
			return xapi_proxy(obj, path.concat(property));
		},
		apply (_target, _self, args) {
			if (path.length > 0) {
				if (path[path.length - 1].toLowerCase() == "login_with_password") {
					return obj.loginWithPasswd(args[0], args[1]);
				} else if (path[path.length - 1].toLowerCase() == "login_with_session") {
					return obj.loginWithSession(args[0]);
				} else if (path[path.length - 1].toLowerCase() == "get_session_id") {
					return obj.getSessionId();
				} else if (path[0].toLowerCase() == "xenapi") {
					return obj.xapiRequest(path.slice(1).join("."), args);
				} else {
					throw new Error(`Method ${path.join(".")} is not supported`);
				}
			}
		}
	});
}

export function xapi_client (url: string): any {
	return xapi_proxy(new XapiSession(url));
}
