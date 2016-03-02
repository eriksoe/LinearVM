FSHARPC_FLAGS = --debug
%.exe: %.fs
	fsharpc ${FSHARPC_FLAGS} "$<"

test-lin: linproof.exe
	mono --debug ./linproof.exe
