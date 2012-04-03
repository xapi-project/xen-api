
exception TestExn of (int * int)

module Foo = struct

	external add : int -> int -> int = ""

end
