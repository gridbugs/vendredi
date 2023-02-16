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
      Util.run_blocking env.opam_exe [ "source"; t ]
  end

  module Git = struct
    type t = string [@@deriving sexp_of]

    let of_string t = t

    let fetch_in_vendored_dir t ~(env : Env.t) =
      Util.run_blocking env.git_exe [ "clone"; t ]
  end

  module Local = struct
    type t = string [@@deriving sexp_of]

    let of_string t = t

    let fetch_in_vendored_dir t =
      (* TODO platform-agnostic way of copying files *)
      Util.run_blocking "cp" [ "-r"; t; "." ]
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

module Templates = struct
  let dune_project ~name =
    (* TODO: don't hard-code the dune version *)
    Printf.sprintf "(lang dune 3.6)\n(package (name %s))" name

  let ml = "let () = print_endline \"Hello, World!\""

  let dune ~name ~all_libraries =
    let libraries_string = String.concat " " all_libraries in
    Printf.sprintf "(executable\n (public_name %s)\n (libraries %s))" name
      libraries_string

  let vendored_dune = "(vendored_dirs *)"
end

module Vendredi_dune_project = struct
  let vendored_dir = "vendored"

  let make_files ~path ~project_name ~all_libraries =
    Util.do_in_dir path ~f:(fun () ->
        Util.mkdir_p vendored_dir;
        Util.write_text_file
          ~path:(Filename.concat vendored_dir "dune")
          ~contents:Templates.vendored_dune;
        Util.write_text_file ~path:"dune-project"
          ~contents:(Templates.dune_project ~name:project_name);
        Util.write_text_file
          ~path:(Printf.sprintf "%s.ml" project_name)
          ~contents:Templates.ml;
        Util.write_text_file ~path:"dune"
          ~contents:(Templates.dune ~name:project_name ~all_libraries))

  let rec remove_directories_with_name ~root ~name =
    Util.dir_contents root
    |> List.iter (fun path ->
           if Util.is_normal_dir path then
             if String.equal (Filename.basename path) name then
               Util.run_blocking "rm" [ "-rf"; path ] |> Util.ok_or_exit
             else remove_directories_with_name ~root:path ~name)

  let fetch_sources ~env ~path ~package_sources =
    Util.do_in_dir path ~f:(fun () ->
        Util.mkdir_p vendored_dir;
        Util.do_in_dir vendored_dir ~f:(fun () ->
            List.iter
              (fun package_source ->
                Source.fetch_in_vendored_dir package_source ~env
                |> Util.ok_or_exit)
              package_sources))

  let create_dir path =
    if Sys.file_exists path then
      Error
        (Printf.sprintf
           "There is already a file at \"%s\". Delete it and run this again."
           path)
    else (
      Util.mkdir_p path;
      Logs.info (fun m -> m "Changing to directory: %s" path);
      Sys.chdir path;
      Ok ())

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
           let all_libraries =
             Dune_project.all_vendored_libraries vendored_dir
           in
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
  let path =
    if Filename.is_relative path then Filename.concat (Unix.getcwd ()) path
    else path
  in
  Vendredi_dune_project.setup ~env ~path ~project_name ~package_sources
  |> Util.ok_or_exit

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
