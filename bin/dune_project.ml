module String = struct
  include String

  let chop_prefix s ~prefix =
    if String.starts_with s ~prefix then
      let pos = String.length prefix in
      let len = String.length s - pos in
      Some (String.sub s pos len)
    else None

  module Set = struct
    module Sexp = Sexplib.Sexp
    include Set.Make (String)
  end
end

let read_dune_file path =
  let contents = Util.read_text_file path in
  (if String.starts_with ~prefix:"(* -*- tuareg -*- *)" contents then
   Error "TODO: handle tuareg dune files"
  else Ok contents)
  |> Util.ok_or_exit

let rec list_files_recursive dir =
  Util.dir_contents dir
  |> List.concat_map (fun path ->
         if Util.is_normal_dir path then list_files_recursive path else [ path ])

let list_dune_files path =
  list_files_recursive path
  |> List.filter (fun filename ->
         Util.is_normal_file filename
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
  let vendored_subdirs = Util.dir_contents path in
  List.concat_map
    (fun dir ->
      let packages = packages_in_dir dir in
      List.concat_map
        (fun package -> Package.library_public_names package)
        packages)
    vendored_subdirs
