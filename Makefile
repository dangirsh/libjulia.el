EMACS = /home/dan/emacs-src/src/emacs

JL_SHARE = $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
JL_FLAGS = $(shell $(JL_SHARE)/julia-config.jl --cflags --ldflags --ldlibs)

.PHONY: all compile lib clean test

all: lib

lib: *.c
	gcc $^ -shared -Wall $(JL_FLAGS) -lffi -lltdl -o libjulia-wrapper.so

clean:
	rm *.o *.so

test: lib libjulia-test.el Makefile
	$(EMACS) --module-assertions -nw -Q -batch -L . -l ert -l seq -l libjulia-test.el -f ert-run-tests-batch-and-exit
