(** Parsing of s-expression *)

open Import
open Ppx_sexp_conv_lib

module type Parser_state = sig
  (** State of the parser *)
  type t

  (** Create a new parser state. [pos] is the initial position, it defaults to
      [{line=1;col=0;offset=0}]. *)
  val create : ?pos:Positions.pos -> unit -> t

  (** Reset the given parsing state. The following always succeed:

      {[
        reset t ?pos;
        assert (t = create ?pos ())
      ]}
  *)
  val reset : ?pos:Positions.pos -> t -> unit

  (** Number of characters fed to the parser *)
  val offset : t -> int

  (** Position in the text *)
  val line   : t -> int
  val column : t -> int

  val position : t -> Positions.pos

  (** Prevent the state from receiving any more characters. Trying to feed more characters
      will result in an exception, unless the state is reset. *)
  val stop : t -> unit
end

module type Parser_stack = sig
  (** Parser stack. The stack is not in [state] for optimization purposes. *)
  type t

  val empty : t
end

module type Parser = sig
  (** Values produced by the parser *)
  type parsed_value

  module State : Parser_state
  module Stack : Parser_stack

  (** Feed one character to the parser. In case of error, it raises [Parse_error] *)
  val feed : State.t -> char -> Stack.t -> Stack.t

  (** Instruct the parser that the end of input was reached. In case of error, it raises
      [Parse_error] *)
  val feed_eoi : State.t -> Stack.t -> parsed_value

  (** {3 Convenience functions} *)

  val feed_string : State.t -> string -> Stack.t -> Stack.t
  val feed_substring : State.t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes : State.t -> bytes -> Stack.t -> Stack.t
  val feed_subbytes : State.t -> bytes -> pos:int -> len:int -> Stack.t -> Stack.t

  (** {3 High-level functions} *)

  module Error : sig type t end

  val parse_string     : string -> (parsed_value, Error.t) result
  val parse_string_exn : string -> parsed_value
end

module type Eager_parser = sig
  (** Same as [Parser] but gives back a s-expression as soon as they are found in the
      input.

      For instance you can use this function to parse a stream and stop at the first
      s-expression:

      {[
        exception Got_sexp of Sexp.t

        let fetch_sexp stream =
          let module P = Parsexp.Sexp_parsing.Eager in
          let rec hot_loop state stream stack =
            match Stream.peek stream with
            | None -> P.feed_eoi state stack
            | Some char ->
              let stack = P.feed state char stack in
              Stream.junk stream;
              hot_loop state stream stack
          in
          let got_sexp state sexp =
            raise_notrace (Got_sexp sexp)
          in
          let count = Stream.count stream in
          let state = P.State.create ~f:got_sexp ~no_sexp_is_error:true in
          match hot_loop state stream P.Stack.empty with
          | () -> assert false
          | exception (Got_sexp sexp) ->
            (* This test is true if the s-expression includes the last character passed to
               the parser *)
            if P.State.offset state > Stream.count stream - count then Stream.junk stream;
            sexp
      ]}
  *)

  (** Values produces by the parser *)
  type parsed_value

  module State : sig
    include Parser_state

    module Read_only : sig
      type t
      (** Read-only handle to a parser state *)

      val offset   : t -> int
      val line     : t -> int
      val column   : t -> int
      val position : t -> Positions.pos
    end

    (** [create ~f] create a new eager parser state. [f] will be called on each
        s-expression found. If [f] raises, then the parser is made unusable ([stop t] is
        invoked).

        [no_sexp_is_error] controls the behavior of the parse when the end of input is
        reached and no s-expression has been found. When [no_sexp_is_error] is [false]
        (the default) [feed_eoi] just returns [()], when it is [false] [feed_eoi]
        raises. In any case, if the end of input is reached while parsing an incomplete
        s-expression such as [(abc], error is raised.

        [f] must not save the read-only parser state it receives to access it after
        returning. It is unspecified what values it will read if it does so. *)
    val create
      :  ?pos:Positions.pos
      -> ?no_sexp_is_error:bool (** default: false *)
      -> (Read_only.t -> parsed_value -> unit)
      -> t

    (**/**)
    val old_parser_cont_state : t -> Parser_automaton_internal.Public.Old_parser_cont_state.t
    (**/**)
  end

  module Stack : Parser_stack

  val feed : State.t -> char -> Stack.t -> Stack.t
  val feed_eoi : State.t -> Stack.t -> unit
  val feed_string : State.t -> string -> Stack.t -> Stack.t
  val feed_substring : State.t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes : State.t -> bytes -> Stack.t -> Stack.t
  val feed_subbytes : State.t -> bytes -> pos:int -> len:int -> Stack.t -> Stack.t

  module Lexbuf_consumer : sig
    type t

    val create : unit -> t

    (** Consume exactly one s-expression from the given lexing buffer *)
    val parse : t -> Lexing.lexbuf -> parsed_value

    (** Consume exactly one s-expression from the given lexing buffer. Returns [None] if
        the end of input is reached before seeing any s-expression. *)
    val parse_opt : t -> Lexing.lexbuf -> parsed_value option
  end
end

module type Conv = sig
  type 'a single_or_many

  module Parse_error   : sig type t end
  module Of_sexp_error : sig type t end
  module Conv_error    : sig type t end

  val parse_string     : string -> (Sexp.t -> 'a) -> ('a single_or_many, Conv_error.t) result
  val parse_string_exn : string -> (Sexp.t -> 'a) -> 'a single_or_many

  val conv
    :  Sexp.t single_or_many * Positions.t
    -> (Sexp.t -> 'a)
    -> ('a single_or_many, Of_sexp_error.t) result
  val conv_exn
    :  Sexp.t single_or_many * Positions.t
    -> (Sexp.t -> 'a)
    -> 'a single_or_many

  (** Convenience function for merging parsing and conversion errors.

      For instance if you have a [load] function as follow:

      {[
        val load : string -> (Sexp.t list * Positions.t, Parse_error.t) result
      ]}

      then you can create a [load_conv] function as follow:

      {[
        let load_conv : string -> (Sexp.t -> 'a) -> ('a list, Conv_error.t) result
          = fun filename f -> conv_combine (load filename) f
      ]}
  *)
  val conv_combine
    :  (Sexp.t single_or_many * Positions.t, Parse_error.t) result
    -> (Sexp.t -> 'a)
    -> ('a single_or_many, Conv_error.t) result
end

module type Parsexp = sig
  module Positions = Positions
  module Cst       = Cst

  module Parse_error : sig
    type t [@@deriving_inline sexp_of]

    include
    sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    end
    [@@@end]

    val position : t -> Positions.pos
    val message  : t -> string

    (** Report an error in a style similar to OCaml, for instance:

        File "blah", line 42, character 10:
        Error: s-expression parsing error;
        unterminated quoted string.
    *)
    val report : Format.formatter -> filename:string -> t -> unit
  end

  module type Parser       = Parser with module Error := Parse_error
  module type Eager_parser = Eager_parser

  (** Exception raised in case of a parsing error *)
  exception Parse_error of Parse_error.t

  module Single : Parser       with type parsed_value = Sexp.t
  module Many   : Parser       with type parsed_value = Sexp.t list
  module Eager  : Eager_parser with type parsed_value = Sexp.t

  module Single_and_positions : Parser       with type parsed_value = Sexp.t * Positions.t
  module Many_and_positions   : Parser       with type parsed_value = Sexp.t list * Positions.t
  module Eager_and_positions  : Eager_parser with type parsed_value = Sexp.t * Positions.t

  module Single_just_positions : Parser       with type parsed_value = Positions.t
  module Many_just_positions   : Parser       with type parsed_value = Positions.t
  module Eager_just_positions  : Eager_parser with type parsed_value = Positions.t

  module Many_cst  : Parser       with type parsed_value = Cst.t_or_comment list
  module Eager_cst : Eager_parser with type parsed_value = Cst.t_or_comment

  module Of_sexp_error : sig
    type t [@@deriving_inline sexp_of]

    include
    sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    end
    [@@@end]

    (** Exception raised by the user function *)
    val user_exn : t -> exn

    (** S-expression that failed to be converted *)
    val sub_sexp : t -> Sexp.t

    (** Position of [sub_sexp t] in the original source, if found *)
    val location : t -> Positions.range option

    (** Similar to [Parse_error.report] *)
    val report : Format.formatter -> filename:string -> t -> unit
  end

  (** Exception raised in case of a conversion error *)
  exception Of_sexp_error of Of_sexp_error.t

  module Conv_error : sig
    type t =
      | Parse_error   of Parse_error.t
      | Of_sexp_error of Of_sexp_error.t
    [@@deriving_inline sexp_of]

    include
    sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    end
    [@@@end]

    (** Similar to [Parse_error.report] *)
    val report : Format.formatter -> filename:string -> t -> unit
  end

  module type Conv = Conv
    with module Parse_error   := Parse_error
    with module Of_sexp_error := Of_sexp_error
    with module Conv_error    := Conv_error

  module Conv_single : Conv with type 'a single_or_many = 'a
  module Conv_many   : Conv with type 'a single_or_many = 'a list
end
