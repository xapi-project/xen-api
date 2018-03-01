open Sexplib.Std

type address = {
	domain: int;
	bus: int;
	dev: int;
	fn: int;
}
[@@deriving sexp, rpc, rpcty]

let address_of_string str =
	Scanf.sscanf str "%04x:%02x:%02x.%02x"
		(fun domain bus dev fn -> {domain; bus; dev; fn})

let string_of_address address =
	Printf.sprintf "%04x:%02x:%02x.%01x"
		address.domain address.bus address.dev address.fn
