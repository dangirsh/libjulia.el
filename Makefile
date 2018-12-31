EMACS = /home/dan/emacs-src/src/emacs

JL_SHARE = $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
JL_FLAGS = $(shell $(JL_SHARE)/julia-config.jl --cflags --ldflags --ldlibs)

.PHONY: all compile lib clean test

all: lib

lib: *.c
	gcc $^ -shared -Wall $(JL_FLAGS) -lffi -lltdl -o julia-core.so

clean:
	rm *.o *.so

test: lib julia-tests.el Makefile
	$(EMACS) --module-assertions -nw -Q -batch -L . -l ert -l julia-tests.el -f ert-run-tests-batch-and-exit

test-ffi: lib julia-ffi-test.el Makefile
	$(EMACS) --module-assertions -nw -Q -batch -L . -l ert -l julia-ffi-test.el -f ert-run-tests-batch-and-exit
