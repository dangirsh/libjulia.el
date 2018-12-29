FILES = emacs-julia.el
ELCFILES = $(FILES:.el=.elc)

EMACS = ./bin/emacs

JULIA_DIR = "/home/dan/julia"
JL_SHARE = "$(JULIA_DIR)/usr/share/julia"
JL_FLAGS = $(shell $(JL_SHARE)/julia-config.jl --cflags --ldflags --ldlibs)

.PHONY: all compile lib clean test

all: lib
# all: lib compile

lib: *.c
	gcc $^ --shared -Wall $(JL_FLAGS) -o emacs-julia.so

clean:
	rm *.o *.so $(FILES:.el=.elc)

test: lib emacs-julia-tests.el Makefile
	$(EMACS) --module-assertions -nw -Q -batch -L . -l ert -l emacs-julia-tests.el --eval "(ert-run-tests-batch-and-exit)"

# compile: $(ELCFILES)

# $(ELCFILES): %.elc: %.el
# 	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<
