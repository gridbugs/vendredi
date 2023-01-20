open Sexplib.Std
module Sexp = Sexplib.Sexp

module Env = struct
  type t = { opam_exe : string; git_exe : string } [@@deriving sexp_of]

  let default = { opam_exe = "opam"; git_exe = "git" }
  let to_string t = sexp_of_t t |> Sexp.to_string
end

module Source = struct
  type t = Opam of string | Git of string | Local of string
  [@@deriving sexp_of]

  let to_string t = sexp_of_t t |> Sexp.to_string
end

let vendredi package_sources =
  (* TODO: make it possible to configure and validate the environment rather than always assuming the default is valid *)
  let env = Env.default in
  Logs.info (fun m -> m "Env: %s" (Env.to_string env));
  List.iter
    (fun source ->
      Logs.info (fun m -> m "Will vendor: %s" (Source.to_string source)))
    package_sources

module Cli = struct
  open Cmdliner

  let rec term_concat = function
    | [] -> Term.const []
    | h :: t -> Term.(const List.append $ h $ term_concat t)

  let mk_packages ~f ~info:info_ =
    let ps = Arg.(value & opt_all string [] & info_) in
    Term.(const (List.map f) $ ps)

  let packages =
    term_concat
      [
        mk_packages
          ~f:(fun s -> Source.Opam s)
          ~info:(Arg.info [ "opam" ] ~docv:"OPAM-PACKAGE");
        mk_packages
          ~f:(fun s -> Source.Git s)
          ~info:(Arg.info [ "git" ] ~docv:"GIT-URL");
        mk_packages
          ~f:(fun s -> Source.Local s)
          ~info:(Arg.info [ "local" ] ~docv:"PATH-TO-DIR");
      ]

  let vendredi_t = Term.(const vendredi $ packages)

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
