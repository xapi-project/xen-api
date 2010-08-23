module Int64 : sig

	module Operators : sig

		val ( +++ ) : int64 -> int64 -> int64
		val ( --- ) : int64 -> int64 -> int64
		val ( *** ) : int64 -> int64 -> int64
		val ( /// ) : int64 -> int64 -> int64
		val ( &&& ) : int64 -> int64 -> int64
		val ( ||| ) : int64 -> int64 -> int64
		val ( <<< ) : int64 -> int -> int64
		val ( >>> ) : int64 -> int -> int64
		val ( !!! ) : int64 -> int64

	end

end