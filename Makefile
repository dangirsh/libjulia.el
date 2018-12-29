FILES = emacs-julia.el
ELCFILES = $(FILES:.el=.elc)

EMACS = ./bin/emacs

JULIA_DIR = "/home/dan/julia"
JL_SHARE = "$(JULIA_DIR)/usr/share/julia"

CFLAGS   += $(shell $(JL_SHARE)/julia-config.jl --cflags)
LDFLAGS  += $(shell $(JL_SHARE)/julia-config.jl --ldflags)
LDLIBS   += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)
JL_FLAGS = $(CFLAGS) $(LDFLAGS) $(LDLIBS)

.PHONY: all compile lib clean test

all: lib compile

test: libemacs-julia.so tests.el
	$(EMACS) --module-assertions -nw -Q -batch -L . -l ert -l emacs-julla-tests.el \
--eval "(ert-run-tests-batch-and-exit)"

lib: libemacs-julia.so libemacs-module-helpers.so

clean:
	rm *.o *.so $(FILES:.el=.elc)

libemacs-module-helpers.so: emacs-module-helpers.c emacs-module-helpers.h
	gcc -shared -Wall -I/usr/local/include \
		-fPIC -o lib/libemacs-module-helpers.so -c emacs-module-helpers.c

libemacs-julia.so: emacs-julia.c lib/libemacs-module-helpers.so
	gcc emacs-julia.c --shared -Wall -L./lib -o lib/libemacs-julia.so \
	$(JL_FLAGS) \
	-lemacs-module-helpers


$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
