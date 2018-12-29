
JULIA_DIR = "/home/dan/julia"
JL_SHARE = "$(JULIA_DIR)/usr/share/julia"

CFLAGS   += $(shell $(JL_SHARE)/julia-config.jl --cflags)

LDFLAGS  += $(shell $(JL_SHARE)/julia-config.jl --ldflags) -Wl,--export-dynamic

LDLIBS   += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)

JL_FLAGS = $(CFLAGS) $(LDFLAGS) $(LDLIBS)

emacs-module-helpers.o: emacs-module-helpers.c emacs-module-helpers.h
	gcc -Wall -I/usr/local/include -fPIC $(GCC_FLAGS) -c emacs-module-helpers.c

libemacs-module-helpers.so: emacs-module-helpers.o
	gcc -shared -o libemacs-module-helpers.so emacs-module-helpers.o

emacs-julia.o: emacs-julia.c
	gcc -Wall $(JL_FLAGS) -c emacs-julia.c

libemacs-julia.so: emacs-julia.o libemacs-module-helpers.so
	gcc -shared $(JL_FLAGS) -L. -lemacs-module-helpers -o emacs-julia.so emacs-julia.o

test-emacs-julia: emacs-julia.so tests.el
	bin/emacs -batch -q -l tests.el -f test-emacs-julia

test: test-emacs-julia

.PHONY: all clean test

all: libemacs-module-helpers.so libemacs-julia.so

clean:
	rm *.o *.so
