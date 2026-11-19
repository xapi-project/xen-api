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

type vgpu_type = {
    name: string
  ; id: int
  ; profile: int
  ; max_instance_per_gi: int
  ; profile_gib: int
  ; slices: int
  ; max_vgpus: int
}

let profile_1g = 47

let profile_2g = 35

let profile_4g = 32

let slice_gib = 24

(* Transcribed from the driver's vgpuConfig.xml *)
let types =
  [
    {
      name= "DC-1-2Q"
    ; id= 1546
    ; profile= profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-2A"
    ; id= 1547
    ; profile= profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-2B"
    ; id= 1548
    ; profile= profile_1g
    ; max_instance_per_gi= 12
    ; profile_gib= 2
    ; slices= 1
    ; max_vgpus= 48
    }
  ; {
      name= "DC-1-3Q"
    ; id= 1549
    ; profile= profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ; {
      name= "DC-1-3A"
    ; id= 1550
    ; profile= profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ; {
      name= "DC-1-4Q"
    ; id= 1551
    ; profile= profile_1g
    ; max_instance_per_gi= 6
    ; profile_gib= 4
    ; slices= 1
    ; max_vgpus= 24
    }
  ; {
      name= "DC-1-4A"
    ; id= 1552
    ; profile= profile_1g
    ; max_instance_per_gi= 6
    ; profile_gib= 4
    ; slices= 1
    ; max_vgpus= 24
    }
  ; {
      name= "DC-1-6Q"
    ; id= 1553
    ; profile= profile_1g
    ; max_instance_per_gi= 4
    ; profile_gib= 6
    ; slices= 1
    ; max_vgpus= 16
    }
  ; {
      name= "DC-1-6A"
    ; id= 1554
    ; profile= profile_1g
    ; max_instance_per_gi= 4
    ; profile_gib= 6
    ; slices= 1
    ; max_vgpus= 16
    }
  ; {
      name= "DC-1-8Q"
    ; id= 1555
    ; profile= profile_1g
    ; max_instance_per_gi= 3
    ; profile_gib= 8
    ; slices= 1
    ; max_vgpus= 12
    }
  ; {
      name= "DC-1-8A"
    ; id= 1556
    ; profile= profile_1g
    ; max_instance_per_gi= 3
    ; profile_gib= 8
    ; slices= 1
    ; max_vgpus= 12
    }
  ; {
      name= "DC-1-12Q"
    ; id= 1558
    ; profile= profile_1g
    ; max_instance_per_gi= 2
    ; profile_gib= 12
    ; slices= 1
    ; max_vgpus= 8
    }
  ; {
      name= "DC-1-12A"
    ; id= 1559
    ; profile= profile_1g
    ; max_instance_per_gi= 2
    ; profile_gib= 12
    ; slices= 1
    ; max_vgpus= 8
    }
  ; {
      name= "DC-1-24Q"
    ; id= 1561
    ; profile= profile_1g
    ; max_instance_per_gi= 1
    ; profile_gib= 24
    ; slices= 1
    ; max_vgpus= 4
    }
  ; {
      name= "DC-1-24A"
    ; id= 1562
    ; profile= profile_1g
    ; max_instance_per_gi= 1
    ; profile_gib= 24
    ; slices= 1
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-12Q"
    ; id= 1564
    ; profile= profile_2g
    ; max_instance_per_gi= 4
    ; profile_gib= 12
    ; slices= 2
    ; max_vgpus= 8
    }
  ; {
      name= "DC-2-12A"
    ; id= 1565
    ; profile= profile_2g
    ; max_instance_per_gi= 4
    ; profile_gib= 12
    ; slices= 2
    ; max_vgpus= 8
    }
  ; {
      name= "DC-2-16Q"
    ; id= 1567
    ; profile= profile_2g
    ; max_instance_per_gi= 3
    ; profile_gib= 16
    ; slices= 2
    ; max_vgpus= 6
    }
  ; {
      name= "DC-2-16A"
    ; id= 1568
    ; profile= profile_2g
    ; max_instance_per_gi= 3
    ; profile_gib= 16
    ; slices= 2
    ; max_vgpus= 6
    }
  ; {
      name= "DC-2-24Q"
    ; id= 1570
    ; profile= profile_2g
    ; max_instance_per_gi= 2
    ; profile_gib= 24
    ; slices= 2
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-24A"
    ; id= 1571
    ; profile= profile_2g
    ; max_instance_per_gi= 2
    ; profile_gib= 24
    ; slices= 2
    ; max_vgpus= 4
    }
  ; {
      name= "DC-2-48Q"
    ; id= 1573
    ; profile= profile_2g
    ; max_instance_per_gi= 1
    ; profile_gib= 48
    ; slices= 2
    ; max_vgpus= 2
    }
  ; {
      name= "DC-2-48A"
    ; id= 1574
    ; profile= profile_2g
    ; max_instance_per_gi= 1
    ; profile_gib= 48
    ; slices= 2
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-24Q"
    ; id= 1576
    ; profile= profile_4g
    ; max_instance_per_gi= 4
    ; profile_gib= 24
    ; slices= 4
    ; max_vgpus= 4
    }
  ; {
      name= "DC-4-24A"
    ; id= 1577
    ; profile= profile_4g
    ; max_instance_per_gi= 4
    ; profile_gib= 24
    ; slices= 4
    ; max_vgpus= 4
    }
  ; {
      name= "DC-4-32Q"
    ; id= 1579
    ; profile= profile_4g
    ; max_instance_per_gi= 3
    ; profile_gib= 32
    ; slices= 4
    ; max_vgpus= 3
    }
  ; {
      name= "DC-4-32A"
    ; id= 1580
    ; profile= profile_4g
    ; max_instance_per_gi= 3
    ; profile_gib= 32
    ; slices= 4
    ; max_vgpus= 3
    }
  ; {
      name= "DC-4-48Q"
    ; id= 1582
    ; profile= profile_4g
    ; max_instance_per_gi= 2
    ; profile_gib= 48
    ; slices= 4
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-48A"
    ; id= 1583
    ; profile= profile_4g
    ; max_instance_per_gi= 2
    ; profile_gib= 48
    ; slices= 4
    ; max_vgpus= 2
    }
  ; {
      name= "DC-4-96Q"
    ; id= 1585
    ; profile= profile_4g
    ; max_instance_per_gi= 1
    ; profile_gib= 96
    ; slices= 4
    ; max_vgpus= 1
    }
  ; {
      name= "DC-4-96A"
    ; id= 1586
    ; profile= profile_4g
    ; max_instance_per_gi= 1
    ; profile_gib= 96
    ; slices= 4
    ; max_vgpus= 1
    }
  ; {
      name= "DC-1-3B"
    ; id= 2158
    ; profile= profile_1g
    ; max_instance_per_gi= 8
    ; profile_gib= 3
    ; slices= 1
    ; max_vgpus= 32
    }
  ]

let find name = List.find (fun t -> t.name = name) types

let request (t : vgpu_type) =
  Gpu.Gpu_capacity.
    {profile= t.profile; vgpu_type= t.id; weight= t.max_instance_per_gi}

let g1 = [profile_1g; profile_1g; profile_1g; profile_1g]

let g2 = [profile_1g; profile_1g]

let g3 = [profile_2g; profile_1g; profile_1g]
