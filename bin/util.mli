val ok_or_exit : ('a, string) result -> 'a
val run_blocking : string -> string list -> (unit, string) result
val mkdir_p : string -> unit
val read_text_file : string -> string
val write_text_file : path:string -> contents:string -> unit
val is_normal_file : string -> bool
val is_normal_dir : string -> bool
val dir_contents : string -> string list
val do_in_dir : string -> f:(unit -> 'a) -> 'a
