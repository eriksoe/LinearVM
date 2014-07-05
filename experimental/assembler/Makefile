build:
#	mlton assembler.mlb 2>&1 | sed 's/Error: \([A-Za-z0-9_]\+.sml\) \([0-9]\+\.[0-9]\+\)\./\1:\2:/' >&2
	mlton assembler.mlb

build4emacs:
	${MAKE} do_build 2>&1 | perl -ne 'if (s/^Error: ([A-Za-z0-9_]+.sml) ([0-9]+\.[0-9]+)\./\1:\2:/) {chomp}; print' >&2

oldtest:
	mlton -const 'Exn.keepHistory true' parser_test.mlb 2>&1 | perl -ne 'if (s/^Error: ([A-Za-z0-9_]+.sml) ([0-9]+\.[0-9]+)\./\1:\2:/) {chomp}; print' >&2
	./parser_test

.PHONY: test
test:
	(cat test.mlb.template; ls -1 *_test.sml; echo "end") > test.mlb
	mlton -const 'Exn.keepHistory true' test.mlb 2>&1 | perl -ne 'if (s/^Error: ([A-Za-z0-9_]+.sml) ([0-9]+\.[0-9]+)\./\1:\2:/) {chomp}; print' >&2
	./test

test.%:
	(cat test.mlb.template; ls -1 $*_test.sml; echo "end") > test.mlb
	mlton -const 'Exn.keepHistory true' test.mlb 2>&1 | perl -ne 'if (s/^Error: ([A-Za-z0-9_]+.sml) ([0-9]+\.[0-9]+)\./\1:\2:/) {chomp}; print' >&2
	./test

deps:
	git clone https://github.com/emilwall/DescribeSML
