type return_record = {
  result : string;
  metadata : (int * int) list;
  extras : string option;
} [@@deriving rpcty]

type variant_t =
  | Foo of string list
  | Bar
  | Baz of float
[@@deriving rpcty]

module API(R:Idl.RPC) = struct
  open R
  open Idl

  let description = Idl.Interface.{
      name="Test server";
      namespace=None;
      description=["Test interface"];
      version=(1,0,0);
    }

  let implementation = implement description

  (* Construct a bunch of arguments to use in our RPCs *)
  let arg1   = Param.mk ~name:"arg1" Rpc.Types.string
  let argx   = Param.mk ~name:"x" Rpc.Types.int
  let argopt = Param.mk ~name:"opt" Rpc.Types.{name="string opt"; description=[]; ty=Option (Basic String)}
  let argv   = Param.mk ~name:"v" variant_t
  let argi   = Param.mk ~name:"i" Rpc.Types.int64
  let argu   = Param.mk ~name:"return_u" Rpc.Types.unit
  let return = Param.mk ~name:"return" return_record

  (* We'll use the default error type *)
  let e      = Idl.DefaultError.err

  (* Construct 3 RPC definitions *)
  let rpc1 = declare "rpc1" ["Test RPC 1"] (arg1 @-> argx @-> returning return e)
  let rpc2 = declare "rpc2" ["Test RPC 2"] (argopt @-> argv @-> returning argu e)
  let rpc3 = declare "rpc3" ["Test RPC 3"] (argi @-> returning argi e)
end

module ImplM = struct
  open Rpc_async.M

  let rpc1 arg1 x =
    if x=5
    then return_err (Idl.DefaultError.InternalError "Boo")
    else begin
      Printf.printf "rpc1: %s %d\n" arg1 x;
      return {
        result = "OK!";
        metadata = [(1,2);(3,4)];
        extras = Some "bar";
      }
    end

  let rpc2 opt v =
    (match opt with
    | Some s -> Printf.printf "Got an optional string: %s\n" s;
    | None -> ());
    (match v with
    | Foo ss ->
      Printf.printf "Foo: [%s]\n" (String.concat ";" ss)
    | Bar ->
      Printf.printf "Bar\n"
    | Baz f ->
      Printf.printf "Baz: %f\n" f);
    return ()

  let rpc3 i =
    Printf.printf "%Ld\n" i;
    return (Int64.add i 1L)
end

let rpc rpc_fn call =
  let open Async.Deferred in
  let call_string = Jsonrpc.string_of_call call in
  Printf.printf "rpc function: call_string='%s'\n" call_string;
  let call = Jsonrpc.call_of_string call_string in
  rpc_fn call >>= fun response ->
  let response_str = Jsonrpc.string_of_response response in
  Printf.printf "rpc function: response_string = '%s'\n" response_str;
  return (Jsonrpc.response_of_string response_str)

module Server = API(Rpc_async.GenServer ())
module Client = API(Rpc_async.GenClient ())

let main () =
  let open Rpc_async.M in

  Server.rpc1 ImplM.rpc1;
  Server.rpc2 ImplM.rpc2;
  Server.rpc3 ImplM.rpc3;

  let funcs = Server.implementation in

  let rpc = rpc (Rpc_async.server funcs) in

  Client.rpc1 rpc "test argument" 2 >>= fun result ->
  Printf.printf "result.result='%s', metadata=[%s]\n"
    result.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.metadata));

  checked_bind (Client.rpc1 rpc "test argument" 5) (fun result ->
      Printf.printf "result.result='%s', metadata=[%s]\n"
        result.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.metadata));
      return ())
    (fun err ->
       Printf.printf "Error: %s\n" (match err with | Idl.DefaultError.InternalError s -> s);
       return ()
    )
  >>= fun () ->
  Client.rpc2 rpc None (Foo ["hello";"there"]) >>= fun _ ->
  Client.rpc2 rpc (Some "Optional") (Foo ["hello";"there"]) >>= fun _ ->
  Client.rpc3 rpc 999999999999999999L >>= fun i ->
  Printf.printf "%Ld\n" i;
  exit 0;
  return ()

let _ =
  ignore (main ());
  Core.never_returns (Async.Scheduler.go ())
