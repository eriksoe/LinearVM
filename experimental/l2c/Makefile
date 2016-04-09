MCS = gmcs
MCS_FLAGS = -debug
MONO = mono

all: compile

compile: l2c.exe


l2c.exe: *.cs
	${MCS} ${MCS_FLAGS} $^ -out:"$@"

test: compile
	mono --debug ./l2c.exe hello.libc0

check-syntax:
	${MCS} ${MCS_FLAGS} ${CHK_SOURCES} -t:module *.cs
	# `basename ${CHK_SOURCES}`

