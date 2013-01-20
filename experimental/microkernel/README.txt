The linear microkernel is an experimental prototype VM.

Data model:
- register-based - of sizeof(void*)

Code model:
- three kinds of functions
- native: calls though libffi
- builtin: a few things that native functions can't do, such as branching
- normal/composite: defined in terms of other linearvm functions.

Internal code representation:
(coderepr.h)
- a program consists of a set of named modules.
  Each module contains a set of functions.
  The code is represented internally as an array of pointers;
  alternately pointer to a function descriptor and pointers to argument
  descriptors (the number of arguments is variably and is determined
  by the function descriptor).

