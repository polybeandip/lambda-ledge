start:
	./_build/default/bin/main.exe	

recompile:
	dune exec bin/main.exe

lines:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build . 

doc:
	dune build @doc

zip:
	zip -r lambda.zip . 
