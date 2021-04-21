idris2 = idris2

build: src order-taking.ipkg FORCE
	$(idris2) --build order-taking.ipkg

FORCE:

watch:
	while inotifywait -e close_write -r src; do $(idris2) --typecheck order-taking.ipkg; done

clean:
	$(idris2) --clean order-taking.ipkg
	rm -r build
	rm ~/.cache/nvim/lsp.log

repl:
	rlwrap $(idris2) --repl order-taking.ipkg
