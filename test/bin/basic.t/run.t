  $ vendredi --quiet --local $PWD/inner outer
  $ cd outer
  $ echo 'let () = print_endline Inner.message' > hello.ml
  $ dune exec ./hello.exe
  foo
