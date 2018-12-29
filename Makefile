
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

emacs-libjulia.o: emacs-libjulia.c
	gcc -Wall $(JL_FLAGS) -c emacs-libjulia.c

emacs-libjulia.so: emacs-libjulia.o libemacs-module-helpers.so
	gcc -shared $(JL_FLAGS) -L. -lemacs-module-helpers -o emacs-libjulia.so emacs-libjulia.o

# test-emacs-libjulia: emacs-libjulia.so tests.el
# 	emacs -batch -q -l tests.el -f test-emacs-libjulia

# test: test-emacs-libjulia

# all: emacs-libjulia.so

# clean:
# 	rm *.o *.so

.PHONY: all clean

all: libemacs-module-helpers.so emacs-libjulia.so

clean:
	rm *.o *.so
