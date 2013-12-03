SHELL=bash

build:
	gplc parser.pl --no-top-level

test: build
	good=0; bad=0; \
	for i in ../samples/*.livm ; do \
	./parser < $$i | tee test.log ; \
	if grep -q 'Program is OK' test.log ; then let good=good+1; else let bad=bad+1 ; fi ; \
	done ; \
	echo "==== Test results: good=$$good  bad=$$bad"

