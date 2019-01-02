# Based on the Makefile by tromey in emacs-ffi

EMACS_BUILDDIR = /home/dan/emacs-src/
EMACS_FFIDIR = /home/dan/.emacs.d/elpa/local/emacs-ffi

LIBS += -sharedn
LDFLAGS = -shared
CFLAGS += -g3 -Og -shared -fPIC -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

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
