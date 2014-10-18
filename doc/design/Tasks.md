
- cancellation
- transactions? always leaving the system in a recoverable state

	type async_result = unit

	type completion_t = {
		duration : float;
		result : async_result option
	}

	type state =
		| Pending of float
		| Completed of completion_t
		| Failed of Rpc.t

	type t = {
		id: id;
		dbg: string;
		ctime: float;
		state: state;
		subtasks: (string * state) list;
		debug_info: (string * string) list;
	}

- state observability
- responsibility for destruction. What does xapi do on restart?
