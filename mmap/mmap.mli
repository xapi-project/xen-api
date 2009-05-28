type mmap_interface
type mmap_prot_flag = RDONLY | WRONLY | RDWR
type mmap_map_flag = SHARED | PRIVATE

external mmap : Unix.file_descr -> mmap_prot_flag -> mmap_map_flag -> int -> int
             -> mmap_interface = "stub_mmap_init"
external unmap : mmap_interface -> unit = "stub_mmap_final"
external read : mmap_interface -> int -> int -> string = "stub_mmap_read"
external write : mmap_interface -> string -> int -> int -> unit
               = "stub_mmap_write"

external getpagesize : unit -> int = "stub_mmap_getpagesize"
