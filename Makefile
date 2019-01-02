# Based on the Makefile by tromey in emacs-ffi

JL_SHARE = $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
JL_FLAGS = $(shell $(JL_SHARE)/julia-config.jl --cflags --ldflags --ldlibs)

EMACS_BUILDDIR = /home/dan/emacs-src/

LIBS += -shared $(shell $(JL_SHARE)/julia-config.jl --ldlibs)
LDFLAGS = -shared $(shell $(JL_SHARE)/julia-config.jl --ldflags)
CFLAGS += -g3 -Og -shared -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/ $(shell $(JL_SHARE)/julia-config.jl --cflags)

# Set this to debug make check.
# GDB = gdb --args

all: libjulia-wrapper.so

libjulia-wrapper.so: libjulia-wrapper.o
	$(CC) $(LDFLAGS) -o libjulia-wrapper.so libjulia-wrapper.o $(LIBS)

libjulia-wrapper.o: libjulia-wrapper.c

clean:
	rm *.o *.so

test: libjulia-wrapper.so libjulia-test.el Makefile
	LD_LIBRARY_PATH=`pwd`/emacs-ffi:$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	$(GDB) $(EMACS_BUILDDIR)/src/emacs -batch -L `pwd` -l ert -l seq -l libjulia-test.el \
	  -f ert-run-tests-batch-and-exit
