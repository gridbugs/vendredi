# vendredi

Tool for generating dune projects which vendor given packages for the purpose of testing that their dependencies are vendor-friendly

## Example

To test whether the opam package `curses` can be vendored:
```bash
# creates a project at /tmp/curses-test which vendores the curses opam library
$ vendredi /tmp/curses-test --opam curses

$ cd /tmp/curses-test

$ ls
vendored  dune  dune-project  hello.ml

$ ls vendored
curses.1.0.10

$ cat dune
(executable
 (public_name hello)
 (libraries curses))

# build with dune (you'll have to install the dependencies of curses for this to work)
$ dune build
File "vendored/curses.1.0.10/dune", line 40, characters 0-207:
40 | (rule
41 |  (target curses.ml)
42 |  (deps _curses.ml _config.ml _functions.c _keys.ml config.h)
43 |  (action
44 |   (chdir
45 |    %{workspace_root}
46 |    (with-stdout-to
47 |     %{target}
48 |     (system "%{bin:gcc} -x c -E _curses.ml")))))
cc1: fatal error: _curses.ml: No such file or directory
compilation terminated.

# fix the package
$ patch -p1 <<EOF
> diff --git a/vendored/curses.1.0.10/dune b/vendored/curses.1.0.10/dune
> index 883f555..11f8032 100644
> --- a/vendored/curses.1.0.10/dune
> +++ b/vendored/curses.1.0.10/dune
> @@ -42,7 +42,7 @@
>   (deps _curses.ml _config.ml _functions.c _keys.ml config.h)
>   (action
>    (chdir
> -   %{workspace_root}
> +   %{project_root}
>     (with-stdout-to
>      %{target}
>      (system "%{bin:gcc} -x c -E _curses.ml")))))
> EOF
patching file vendored/curses.1.0.10/dune

# now it builds
$ dune build
```
