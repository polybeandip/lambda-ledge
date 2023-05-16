.PHONY: test

play:
	dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/test.exe 

lines:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build . 

doc:
	dune build @doc

zip:
	zip -r lambda.zip . 