(** a variable name *)
type vname

module Rpn : sig
  (** RPN expressions for VDEF statements, see [rrdgraph_rpn(3)] *)
  module VDef : sig
    (** an RPN expression for VDEF, see [rrdgraph_data(3)] *)
    type t

    (** a VDEF RPN expression, see [rrdgraph_rpn(3)] *)
    type op = vname -> t

    val maximum : op
    (** see [rrdgraph_rpn(3)] *)

    val minimum : op
    (** see [rrdgraph_rpn(3)] *)

    val average : op
    (** see [rrdgraph_rpn(3)] *)

    val stdev : op
    (** see [rrdgraph_rpn(3)] *)

    val last : op
    (** see [rrdgraph_rpn(3)] *)

    val first : op
    (** see [rrdgraph_rpn(3)] *)

    val total : op
    (** see [rrdgraph_rpn(3)] *)

    val percent : op
    (** see [rrdgraph_rpn(3)] *)

    val percentnan : op
    (** see [rrdgraph_rpn(3)] *)

    val lsl_slope : op
    (** see [rrdgraph_rpn(3)] *)

    val lsl_intercept : op
    (** see [rrdgraph_rpn(3)] *)

    val lsl_correlation : op
    (** see [rrdgraph_rpn(3)] *)
  end

  module CDef : sig
    (** an RPN expression for CDEF, see [rrdgraph_data(3)] *)
    type t

    val vname : vname -> t
    (** [vname v] is [v] as an RPN expression *)

    val value : float -> t
    (** [value v] is [v] as an RPN expression *)

    val op1 : string -> t -> t
    (** [op1 op arg1] is [op arg1]. For valid operators see [rrdgraph_rpn(3)] *)

    val op2 : string -> t -> t -> t
    (** [op2 op arg1 arg2] is [op arg1 arg2]. For valid operators see [rrdgraph_rpn(3)] *)

    val op3 : string -> t -> t -> t -> t
    (** [op3 op arg1 arg2 arg3] is [op arg1 arg2 arg3]. For valid operators see [rrdgraph_rpn(3)] *)
  end
end

module Data : sig
  (** an rrd graph data definition, see [rrdgraph_data(3)] *)
  type t

  val def : string -> Fpath.t -> Rrd.rrd -> Rrd.rra -> Rrd.ds -> vname * t
  (** [def vname rrdfile rrd rra datasource] is a [DEF] (see [rrdgraph_data(3)]) that loads
      [datasource.ds_name] from the [rrdfile] and plots it according to the consolidation function in the
      specified [rra] and timestep calculated based on [rrd]. This data can be refered to as [vname]
      elsewhere. *)

  val vdef : string -> Rpn.VDef.t -> vname * t
  (** [vdef vname vdefrpn] defines [vname] through a [VDEF] (see [rrdgraph_data(3)]) using the
  specified [vdefrpn] expression. Conversion to RPN form is handled internally. *)

  val cdef : string -> Rpn.CDef.t -> vname * t
  (** [cdef vname cdefrpn] defines [vname] through a [CDEF] (see [rrdgraph_data(3)]) using the
  specified [cdefrpn] expression. Conversion to RPN form is handled internally. *)
end
