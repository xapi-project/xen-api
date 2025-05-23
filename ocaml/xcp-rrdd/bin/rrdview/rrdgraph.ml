open Rrd

type vname = VName of string

module Rpn = struct
  module VDef = struct
    (* see rrdgraph_rpn(3) *)
    type t = vname * string

    type op = vname -> t

    let op kind vname = (vname, kind)

    let maximum = op "MAXIMUM"

    let minimum = op "MINIMUM"

    let average = op "AVERAGE"

    let stdev = op "STDEV"

    let last = op "LAST"

    let first = op "FIRST"

    let total = op "TOTAL"

    let percent = op "PERCENT"

    let percentnan = op "PERCENTNAN"

    let lsl_slope = op "LSLSLOPE"

    let lsl_intercept = op "LSLSLINT"

    let lsl_correlation = op "LSLCORREL"
  end

  module CDef = struct
    type t = string Seq.t (* stores a serialized RPN expression *)

    let to_string r = r |> List.of_seq |> String.concat ","

    let vname (VName vname) = Seq.return vname

    let value f = Printf.sprintf "%g" f |> Seq.return

    (* reverse polish notation: arguments first, operator last *)

    let opn op args = Seq.append (List.to_seq args |> Seq.concat) (Seq.return op)

    let op1 op arg = opn op [arg]

    let op2 op arg1 arg2 = opn op [arg1; arg2]

    let op3 op arg1 arg2 arg3 = opn op [arg1; arg2; arg3]
  end
end

module Data = struct
  type t = string

  (* see rrdgraph_data (3) *)

  let def vname rrdfile rrd rra ds =
    let step = Int64.mul rrd.timestep @@ Int64.of_int rra.rra_pdp_cnt in
    ( VName vname
    , String.concat ":"
        [
          "DEF"
        ; vname ^ "=" ^ Fpath.to_string rrdfile
        ; ds.ds_name
        ; Rrd.cf_type_to_string rra.rra_cf
        ; Printf.sprintf "step=%Lu" step
        ]
    )

  let vdef vname (VName var, rpnvdefop) =
    (VName vname, Printf.sprintf "CDEF:%s=%s,%s" vname var rpnvdefop)

  let cdef vname rpn =
    (VName vname, Printf.sprintf "CDEF:%s=%s" vname (Rpn.CDef.to_string rpn))
end
