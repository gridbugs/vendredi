let ok_or_exit = function
  | Ok x -> x
  | Error msg ->
      Printf.eprintf "%s" msg;
      exit 1

let run_blocking program args =
  Logs.info (fun m ->
      m "Running command: %s" (String.concat " " (program :: args)));
  let args_arr = Array.of_list (program :: args) in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let child_pid =
    Unix.create_process program args_arr dev_null dev_null dev_null
  in
  let got_pid, status = Unix.waitpid [] child_pid in
  Unix.close dev_null;
  if got_pid <> child_pid then failwith "wait returned unexpected pid";
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | _ ->
      let command_string = String.concat " " (program :: args) in
      Error (Printf.sprintf "`%s` did not exit successfully" command_string)

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Logs.info (fun m -> m "Creating directory: %s" path);
    Sys.mkdir path 0o755)

let write_text_file ~path ~contents =
  let out_channel = Out_channel.open_text path in
  Out_channel.output_string out_channel contents

let read_text_file path =
  let in_ = In_channel.open_text path in
  let s = In_channel.input_all in_ in
  In_channel.close in_;
  s

let is_normal_file path =
  let ({ st_kind; _ } : Unix.stats) = Unix.lstat path in
  match st_kind with S_REG -> true | _ -> false

let is_normal_dir path =
  let ({ st_kind; _ } : Unix.stats) = Unix.lstat path in
  match st_kind with S_DIR -> true | _ -> false

let dir_contents dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.map (Filename.concat dir)

let do_in_dir dir ~f =
  let cwd = Sys.getcwd () in
  Sys.chdir dir;
  let ret = f () in
  Sys.chdir cwd;
  ret
