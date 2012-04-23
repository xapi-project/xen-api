

module Impl = struct
	type context = unit
	module Foo = struct
		let add context i j = if i=2 then raise (Testidl.TestExn (i,j)) else i+j
	end
end


module Server = Testidl.Server(Impl) 

let rpc x = 
	Printf.printf "RPC: Got request: %s\n" (Rpc.string_of_call x);
	let result = Server.process () x in
	Printf.printf "RPC: Got response: %s\n" (Rpc.string_of_response result);
	result

module Client = Testidl.Client(struct let rpc = rpc end)

let _ =
	try
		Printf.printf "version=%s\n" (Client.get_version ());
		Printf.printf "1+2=%d\n" (Client.Foo.add 1 2);
		Printf.printf "2+3=%d\n" (Client.Foo.add 2 3)
	with Testidl.TestExn (i,j) ->
		Printf.printf "Got test exception (%d,%d)\n" i j
