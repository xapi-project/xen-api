(*
 * Copyright (C) 2026 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type blackwell_type = {
    name: string
  ; id: int
  ; profile: int
  ; max_instance_per_gi: int
  ; profile_gib: int
  ; slices: int
  ; max_vgpus: int
}

let blackwell_profile_1g = 47

let blackwell_profile_2g = 35

let blackwell_profile_4g = 32

let blackwell_slice_gib = 24

(* Transcribed from the driver's vgpuConfig.xml *)
let blackwell_types =
  [
    {
      name= "DC-1-2Q"
    ; id= 1546
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-2A"
    ; id= 1547
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-2B"
    ; id= 1548
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-3Q"
    ; id= 1549
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ; {
      name= "DC-1-3A"
    ; id= 1550
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ; {
      name= "DC-1-4Q"
    ; id= 1551
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 6
    ; profile_gib= 4
    ; slices= 1
    ; max_vgpus= 24
    }
  ; {
      name= "DC-1-4A"
    ; id= 1552
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 6
    ; profile_gib= 4
    ; slices= 1
    ; max_vgpus= 24
    }
  ; {
      name= "DC-1-6Q"
    ; id= 1553
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 4
    ; profile_gib= 6
    ; slices= 1
    ; max_vgpus= 16
    }
  ; {
      name= "DC-1-6A"
    ; id= 1554
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 4
    ; profile_gib= 6
    ; slices= 1
    ; max_vgpus= 16
    }
  ; {
      name= "DC-1-8Q"
    ; id= 1555
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 3
    ; profile_gib= 8
    ; slices= 1
    ; max_vgpus= 12
    }
  ; {
      name= "DC-1-8A"
    ; id= 1556
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 3
    ; profile_gib= 8
    ; slices= 1
    ; max_vgpus= 12
    }
  ; {
      name= "DC-1-12Q"
    ; id= 1558
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 2
    ; profile_gib= 12
    ; slices= 1
    ; max_vgpus= 8
    }
  ; {
      name= "DC-1-12A"
    ; id= 1559
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 2
    ; profile_gib= 12
    ; slices= 1
    ; max_vgpus= 8
    }
  ; {
      name= "DC-1-24Q"
    ; id= 1561
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 1
    ; profile_gib= 24
    ; slices= 1
    ; max_vgpus= 4
    }
  ; {
      name= "DC-1-24A"
    ; id= 1562
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 1
    ; profile_gib= 24
    ; slices= 1
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-12Q"
    ; id= 1564
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 4
    ; profile_gib= 12
    ; slices= 2
    ; max_vgpus= 8
    }
  ; {
      name= "DC-2-12A"
    ; id= 1565
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 4
    ; profile_gib= 12
    ; slices= 2
    ; max_vgpus= 8
    }
  ; {
      name= "DC-2-16Q"
    ; id= 1567
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 3
    ; profile_gib= 16
    ; slices= 2
    ; max_vgpus= 6
    }
  ; {
      name= "DC-2-16A"
    ; id= 1568
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 3
    ; profile_gib= 16
    ; slices= 2
    ; max_vgpus= 6
    }
  ; {
      name= "DC-2-24Q"
    ; id= 1570
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 2
    ; profile_gib= 24
    ; slices= 2
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-24A"
    ; id= 1571
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 2
    ; profile_gib= 24
    ; slices= 2
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-48Q"
    ; id= 1573
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 1
    ; profile_gib= 48
    ; slices= 2
    ; max_vgpus= 2
    }
  ; {
      name= "DC-2-48A"
    ; id= 1574
    ; profile= blackwell_profile_2g
    ; max_instance_per_gi= 1
    ; profile_gib= 48
    ; slices= 2
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-24Q"
    ; id= 1576
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 4
    ; profile_gib= 24
    ; slices= 4
    ; max_vgpus= 4
    }
  ; {
      name= "DC-4-24A"
    ; id= 1577
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 4
    ; profile_gib= 24
    ; slices= 4
    ; max_vgpus= 4
    }
  ; {
      name= "DC-4-32Q"
    ; id= 1579
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 3
    ; profile_gib= 32
    ; slices= 4
    ; max_vgpus= 3
    }
  ; {
      name= "DC-4-32A"
    ; id= 1580
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 3
    ; profile_gib= 32
    ; slices= 4
    ; max_vgpus= 3
    }
  ; {
      name= "DC-4-48Q"
    ; id= 1582
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 2
    ; profile_gib= 48
    ; slices= 4
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-48A"
    ; id= 1583
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 2
    ; profile_gib= 48
    ; slices= 4
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-96Q"
    ; id= 1585
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 1
    ; profile_gib= 96
    ; slices= 4
    ; max_vgpus= 1
    }
  ; {
      name= "DC-4-96A"
    ; id= 1586
    ; profile= blackwell_profile_4g
    ; max_instance_per_gi= 1
    ; profile_gib= 96
    ; slices= 4
    ; max_vgpus= 1
    }
  ; {
      name= "DC-1-3B"
    ; id= 2158
    ; profile= blackwell_profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ]

let blackwell_type name = List.find (fun t -> t.name = name) blackwell_types

let blackwell_request (t : blackwell_type) =
  Gpu.Gpu_capacity.
    {profile= t.profile; vgpu_type= t.id; weight= t.max_instance_per_gi}

let blackwell_g1 =
  [
    blackwell_profile_1g
  ; blackwell_profile_1g
  ; blackwell_profile_1g
  ; blackwell_profile_1g
  ]

let blackwell_g2 = [blackwell_profile_1g; blackwell_profile_1g]

let blackwell_g3 =
  [blackwell_profile_2g; blackwell_profile_1g; blackwell_profile_1g]

type a30_profile = A30_1g_6gb | A30_2g_12gb | A30_4g_24gb

let a30_profiles = [A30_1g_6gb; A30_2g_12gb; A30_4g_24gb]

let a30_request p = Gpu.Gpu_capacity.{profile= p; vgpu_type= p; weight= 1}

let a30_measured = [A30_2g_12gb; A30_1g_6gb; A30_1g_6gb]

let empty_geometry profiles =
  Gpu.Gpu_capacity.(
    Partitions (List.map (fun profile -> {profile; occupancy= Empty}) profiles)
  )
