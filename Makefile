play:
	dune exec bin/main.exe

crime:
	dune exec bin/main.exe --profile release

lines:
	cloc --by-file --include-lang=OCaml .
