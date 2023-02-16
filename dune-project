(lang dune 3.6)
(generate_opam_files true)
(name vendredi)

(cram enable)

(source (github gridbugs/vendredi))
(license MIT)
(authors "Stephen Sherratt")
(maintainers "https://github.com/gridbugs/vendredi/issues")

(package
 (name vendredi)
 (synopsis "Tool for generating dune projects which vendor given packages for the purpose of testing that their dependencies are vendor-friendly")
 (depends
  dune
  cmdliner
  logs
  fmt
  sexplib
  ppx_sexp_conv
  re))
