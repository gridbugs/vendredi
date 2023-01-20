let do_in_dir dir ~f =
  let cwd = Sys.getcwd () in
  Sys.chdir dir;
  let ret = f () in
  Sys.chdir cwd;
  ret

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

let ok_or_exit = function
  | Ok x -> x
  | Error msg ->
      Printf.eprintf "%s" msg;
      exit 1

module Env = struct
  open Sexplib.Std
  module Sexp = Sexplib.Sexp

  type t = { opam_exe : string; git_exe : string } [@@deriving sexp_of]

  let default = { opam_exe = "opam"; git_exe = "git" }
  let to_string t = sexp_of_t t |> Sexp.to_string_hum
end

module Source = struct
  open Sexplib.Std
  module Sexp = Sexplib.Sexp

  module Opam = struct
    type t = string [@@deriving sexp_of]

    let of_string t = t

    let fetch_in_vendored_dir t ~(env : Env.t) =
      run_blocking env.opam_exe [ "source"; t ]
  end

  module Git = struct
    type t = string [@@deriving sexp_of]

    let of_string t = t

    let fetch_in_vendored_dir t ~(env : Env.t) =
      run_blocking env.git_exe [ "clone"; t ]
  end

  module Local = struct
    type t = string [@@deriving sexp_of]

    let of_string t = t

    let fetch_in_vendored_dir t =
      (* TODO platform-agnostic way of copying files *)
      run_blocking "cp" [ "-r"; t; "." ]
  end

  type t = Opam of Opam.t | Git of Git.t | Local of Local.t
  [@@deriving sexp_of]

  let to_string t = sexp_of_t t |> Sexp.to_string_hum

  let fetch_in_vendored_dir t ~(env : Env.t) =
    match t with
    | Git git -> Git.fetch_in_vendored_dir git ~env
    | Opam opam -> Opam.fetch_in_vendored_dir opam ~env
    | Local local -> Local.fetch_in_vendored_dir local
end

module String = struct
  include String

  let chop_prefix s ~prefix =
    if String.starts_with s ~prefix then
      let pos = String.length prefix in
      let len = String.length s - pos in
      Some (String.sub s pos len)
    else None

  module Set = struct
    open Sexplib.Std
    module Sexp = Sexplib.Sexp
    include Set.Make (String)

    let t_of_sexp sexp = list_of_sexp string_of_sexp sexp |> of_list
    let sexp_of_t t = elements t |> sexp_of_list sexp_of_string
  end
end

module Dune_project = struct
  module Templates = struct
    let dune_project ~name =
      Printf.sprintf "(lang dune 3.6)\n(package (name %s))" name

    let ml = "let () = print_endline \"Hello, World!\""

    let dune ~name ~all_libraries =
      let libraries_string = String.concat " " all_libraries in
      Printf.sprintf "(executable\n (public_name %s)\n (libraries %s))" name
        libraries_string

    let vendored_dune = "(vendored_dirs *)"
  end

  let vendored_dir = "vendored"

  let rec mkdir_p path =
    if Sys.file_exists path then ()
    else (
      mkdir_p (Filename.dirname path);
      Sys.mkdir path 0o755)

  let create_dir path =
    if Sys.file_exists path then
      Error
        (Printf.sprintf
           "There is already a file at \"%s\". Delete it and run this again."
           path)
    else (
      mkdir_p path;
      Sys.chdir path;
      Ok ())

  let write_text_file ~path ~contents =
    let out_channel = Out_channel.open_text path in
    Out_channel.output_string out_channel contents

  let read_text_file path =
    let in_ = In_channel.open_text path in
    let s = In_channel.input_all in_ in
    In_channel.close in_;
    s

  let read_dune_file path =
    let contents = read_text_file path in
    (if String.starts_with ~prefix:"(* -*- tuareg -*- *)" contents then
     Error "TODO: handle tuareg dune files"
    else Ok contents)
    |> ok_or_exit

  let is_normal_file path =
    let ({ st_kind; _ } : Unix.stats) = Unix.lstat path in
    match st_kind with S_REG -> true | _ -> false

  let is_normal_dir path =
    let ({ st_kind; _ } : Unix.stats) = Unix.lstat path in
    match st_kind with S_DIR -> true | _ -> false

  let dir_contents dir =
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.map (Filename.concat dir)

  let rec remove_directories_with_name ~root ~name =
    dir_contents root
    |> List.iter (fun path ->
           if is_normal_dir path then
             if String.equal (Filename.basename path) name then
               run_blocking "rm" [ "-rf"; path ] |> ok_or_exit
             else remove_directories_with_name ~root:path ~name)

  let rec list_files_recursive dir =
    dir_contents dir
    |> List.concat_map (fun path ->
           if is_normal_dir path then list_files_recursive path else [ path ])

  let list_dune_files path =
    list_files_recursive path
    |> List.filter (fun filename ->
           is_normal_file filename
           && String.equal (Filename.basename filename) "dune")

  let ignore_dune_file_by_path =
    let pattern = Re.Perl.re "test/blackbox-tests" |> Re.compile in
    fun path ->
      let matches = Re.matches pattern path in
      List.length matches > 0

  module Library = struct
    open Sexplib.Std

    type t = { public_name : string } [@@deriving sexp]
  end

  module Package = struct
    open Sexplib.Std
    module Sexp = Sexplib.Sexp

    type t = { name : string; path : string; libraries : Library.t list }
    [@@deriving sexp]

    let library_public_names { libraries; _ } =
      List.map (fun (library : Library.t) -> library.public_name) libraries

    let _to_string_hum t = Sexp.to_string_hum (sexp_of_t t)
  end

  let parse_dune_string_all_public_libraries s =
    let module Sexp = Sexplib.Sexp in
    let sexps = Sexp.of_string_many s in
    List.filter_map
      (fun sexp ->
        match sexp with
        | Sexp.List (Sexp.Atom "library" :: fields) ->
            let public_name =
              List.find_map
                (function
                  | Sexp.List [ Sexp.Atom "public_name"; Sexp.Atom public_name ]
                    ->
                      Some public_name
                  | _ -> None)
                fields
            in
            Option.map (fun public_name -> { Library.public_name }) public_name
        | _ -> None)
      sexps

  let is_library_valid_for_package_by_name ~library_name ~package_name =
    match String.equal library_name package_name with
    | true -> true
    | false -> (
        match String.chop_prefix library_name ~prefix:package_name with
        | None -> false
        | Some remainder -> String.starts_with remainder ~prefix:".")

  let packages_in_dir path =
    let all_public_libraries =
      list_dune_files path
      |> List.filter (Fun.negate ignore_dune_file_by_path)
      |> List.concat_map (fun dune_file_path ->
             let rel_dune_file_path =
               String.chop_prefix dune_file_path ~prefix:path |> Option.get
             in
             let rel_dune_file_path =
               String.chop_prefix rel_dune_file_path ~prefix:"/"
               |> Option.value ~default:rel_dune_file_path
             in
             read_dune_file (Filename.concat path rel_dune_file_path)
             |> parse_dune_string_all_public_libraries)
    in
    Sys.readdir path |> Array.to_list
    |> List.filter (fun filename ->
           String.equal (Filename.extension filename) ".opam")
    |> List.map (fun filename ->
           let name = Filename.chop_extension filename in
           let libraries =
             List.filter
               (fun (library : Library.t) ->
                 is_library_valid_for_package_by_name
                   ~library_name:library.public_name ~package_name:name)
               all_public_libraries
           in
           { Package.name; path; libraries })

  let all_vendored_libraries path =
    let vendored_subdirs = dir_contents path in
    List.concat_map
      (fun dir ->
        let packages = packages_in_dir dir in
        List.concat_map
          (fun package -> Package.library_public_names package)
          packages)
      vendored_subdirs

  let make_files ~path ~project_name ~all_libraries =
    do_in_dir path ~f:(fun () ->
        mkdir_p vendored_dir;
        write_text_file
          ~path:(Filename.concat vendored_dir "dune")
          ~contents:Templates.vendored_dune;
        write_text_file ~path:"dune-project"
          ~contents:(Templates.dune_project ~name:project_name);
        write_text_file
          ~path:(Printf.sprintf "%s.ml" project_name)
          ~contents:Templates.ml;
        write_text_file ~path:"dune"
          ~contents:(Templates.dune ~name:project_name ~all_libraries))

  let fetch_sources ~env ~path ~package_sources =
    do_in_dir path ~f:(fun () ->
        mkdir_p vendored_dir;
        do_in_dir vendored_dir ~f:(fun () ->
            List.iter
              (fun package_source ->
                Source.fetch_in_vendored_dir package_source ~env |> ok_or_exit)
              package_sources))

  let setup ~env ~path ~project_name ~package_sources =
    create_dir path
    |> Result.map (fun () ->
           fetch_sources ~env ~path ~package_sources;
           let dirs_to_remove =
             [ "_opam"; "_build"; "duniverse"; "vendored" ]
           in
           List.iter
             (fun name -> remove_directories_with_name ~root:vendored_dir ~name)
             dirs_to_remove;
           let all_libraries = all_vendored_libraries vendored_dir in
           make_files ~path ~project_name ~all_libraries)
end

let vendredi ~path ~project_name ~package_sources =
  (* TODO: make it possible to configure and validate the environment rather
     than always assuming the default is valid *)
  let env = Env.default in
  Logs.info (fun m -> m "Env: %s" (Env.to_string env));
  Logs.info (fun m -> m "Will create project \"%s\" in %s" project_name path);
  List.iter
    (fun source ->
      Logs.info (fun m -> m "Will vendor: %s" (Source.to_string source)))
    package_sources;
  Dune_project.setup ~env ~path ~project_name ~package_sources |> ok_or_exit

module Cli = struct
  open Cmdliner

  let path =
    Arg.(
      required
      & pos 0 (Arg.some string) None
          (info []
             ~doc:
               "Path to directory in which to create dune project. Must be \
                non-existent."
             ~docv:"PATH"))

  let project_name =
    Arg.(
      value & opt string "hello"
      & info [ "name" ] ~docs:"project name" ~docv:"PROJECT-NAME")

  let rec term_concat = function
    | [] -> Term.const []
    | h :: t -> Term.(const List.append $ h $ term_concat t)

  let mk_package_sources ~f ~info:info_ =
    let ps = Arg.(value & opt_all string [] & info_) in
    Term.(const (List.map f) $ ps)

  let package_sources =
    term_concat
      [
        mk_package_sources
          ~f:(fun s -> Source.Opam (Source.Opam.of_string s))
          ~info:(Arg.info [ "opam" ] ~docv:"OPAM-PACKAGE");
        mk_package_sources
          ~f:(fun s -> Source.Git (Source.Git.of_string s))
          ~info:(Arg.info [ "git" ] ~docv:"GIT-URL");
        mk_package_sources
          ~f:(fun s -> Source.Local (Source.Local.of_string s))
          ~info:(Arg.info [ "local" ] ~docv:"PATH-TO-DIR");
      ]

  let vendredi_t =
    Term.(
      const (fun path project_name package_sources ->
          vendredi ~path ~project_name ~package_sources)
      $ path $ project_name $ package_sources)

  let vendredi_cmd =
    let doc =
      "Tool for generating dune projects which vendor given packages for the \
       purpose of testing that their dependencies are vendor-friendly"
    in
    Cmd.v (Cmd.info "vendredi" ~doc) vendredi_t

  let main () = Cmd.eval vendredi_cmd
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  exit (Cli.main ())
