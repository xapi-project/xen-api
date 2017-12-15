(*open Ds*)

type fake_ds = {
  f_name : string;
  f_ty : Rrd.ds_type;
  f_val : float;
} [@@deriving rpc]

type fake_ds_list = fake_ds list [@@deriving rpc]

let fake_dir = Filename.concat "/var/lib/xcp" "fake_data"
