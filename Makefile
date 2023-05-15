.PHONY: test

play:
	dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/test.exe 

crime:
	dune exec bin/main.exe --profile release

crimetest:
	OCAMLRUNPARAM=b dune exec test/test.exe --profile release

lines:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build . 
