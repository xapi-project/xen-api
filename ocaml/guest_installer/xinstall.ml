(* Xen PV guest installer *)


open Distros

(* testing *)
let _ =
  let distro = rhel44 in
  let params = 
    {
      name="test";
      desc="test rhel44 install";
      version=1L;
      memory_static_max=268435456L;
      memory_static_min=268435456L;
      memory_dynamic_max=268435456L;
      memory_dynamic_min=268435456L;
      vcpus=1L;
      disk_size=268435456L
    } in
  let s = Operations.init_session "root" "xenroot" in
  let vm= Operations.vm_construct s distro params in
  ignore( Operations.boot_vm s vm );
  ignore( Operations.reset_bootloader s vm );
  
    
