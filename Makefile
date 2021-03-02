pwd := $(shell pwd)

build:
	ocamlfind query coq.sysinit || (git submodule update --init coq && dune build coq/theories/Init/Prelude.vo)
	dune build vscoq-language-server.install

_build/.vscode-extensions:
	git submodule update --init vscoq && cd vscoq && make
	rm -rf _build/.vscode-extensions && mkdir _build/.vscode-extensions && unzip vscoq/vscoq-0.4.0.vsix -d _build/.vscode-extensions/ && cd _build/.vscode-extensions/ && mv extension maximedenes.vscoq-0.4.0

run: build _build/.vscode-extensions
	COQLIB=$(pwd)/_build/default/coq/ \
	PATH=$(pwd)/_build/install/default/bin:$$PATH \
	    code -n --extensions-dir $(pwd)/_build/.vscode-extensions -w tests/interactive