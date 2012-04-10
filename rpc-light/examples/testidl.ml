let version = "1.0"

exception TestExn of (int * int)

module Foo = struct

	external add : int -> int -> int = ""

end
