module Int64 = struct

	module Operators = struct

		let ( +++ ) = Int64.add
		let ( --- ) = Int64.sub
		let ( *** ) = Int64.mul
		let ( /// ) = Int64.div
		let ( &&& ) = Int64.logand
		let ( ||| ) = Int64.logor
		let ( <<< ) = Int64.shift_left
		let ( >>> ) = Int64.shift_right_logical
		let ( !!! ) = Int64.lognot

	end

end
