(* Distribution records *)

type initial_boot = 
    {
      kernel_path : string;
      kernel_args : string;
      ramdisk_path : string;
    }

(* Unchanging distro specific bits and pieces *)
type distro = 
    {
      initial_boot : initial_boot;

      (* overkill perhaps *)
      memory_static_max_lim : int64*int64; 
      memory_static_min_lim : int64*int64;
      memory_dynamic_max_lim : int64*int64;
      memory_dynamic_min_lim : int64*int64;

      max_vcpus : int64;

      disk_device : string;
      disk_min_size : int64;

      other_media : string list; 
    }

(* set by user *)
type distro_params = 
    {
      name:string;
      desc:string;
      version:int64;
      memory_static_max:int64;
      memory_static_min:int64;
      memory_dynamic_max:int64;
      memory_dynamic_min:int64;
      vcpus:int64;
      disk_size:int64;
    }

      
(* Probably ought to read these in from a file or something *)

let rhel44 = 
  { 
    initial_boot = 
      {
	kernel_path = "/opt/xensource/packages/files/guest-installer/rhel4.4-vmlinuz";
	kernel_args = "text updates ramdisk_size=32000 nofb serial";
	ramdisk_path = "/opt/xensource/packages/files/guest-installer/rhel4.4-install-initrd.img"
      };
    memory_static_max_lim = (268435456L, 2147483648L); (* 0..2 gigs *)
    memory_static_min_lim = (268435456L, 2147483648L);
    memory_dynamic_max_lim = (268435456L, 2147483648L);
    memory_dynamic_min_lim = (268435456L, 2147483648L);
    max_vcpus = 1L;
    disk_device = "0";
    disk_min_size = 268435456L;
    other_media=[]
  }

