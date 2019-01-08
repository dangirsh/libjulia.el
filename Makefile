# Based on the Makefile by tromey in emacs-ffi

EMACS_BUILDDIR = /home/dan/emacs-src/
EMACS_FFIDIR = /home/dan/treemax/.spacemacs.d/layers/mine/local/ffi
JULIA_LIBDIR = /home/dan/julia/usr/lib

LIBS += -shared
LDFLAGS = -shared
# TODO test if we can remove the links to on emacs_builddir
CFLAGS += -g3 -Og -shared -fPIC -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

all: libjulia-wrapper.so

libjulia-wrapper.so: libjulia-wrapper.o
	$(CC) $(LDFLAGS) -o libjulia-wrapper.so libjulia-wrapper.o $(LIBS)

libjulia-wrapper.o: libjulia-wrapper.c

clean:
	rm *.o *.so

# Uncomment this to debug via "make test".
GDB = gdb --args

test: libjulia-wrapper.so libjulia-test.el Makefile
	LD_LIBRARY_PATH=`pwd`:$(EMACS_FFIDIR):$(JULIA_LIBDIR):$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	$(GDB) $(EMACS_BUILDDIR)/src/emacs -batch -L `pwd` -L $(EMACS_FFIDIR) -l ffi -l ert -l seq -l libjulia-test.el \
	  -f ert-run-tests-batch-and-exit


test-julisp: libjulia-wrapper.so julisp-mode.el julisp-test.el Makefile
	LD_LIBRARY_PATH=`pwd`:$(EMACS_FFIDIR):$(JULIA_LIBDIR):$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	$(GDB) $(EMACS_BUILDDIR)/src/emacs -batch -L `pwd` -L $(EMACS_FFIDIR) -l ffi -l ert -l seq -l julisp-test.el \
	  -f ert-run-tests-batch-and-exit
