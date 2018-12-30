NOTE: This project is in-development not ready for production use! See the end of this document for notes on how to contribute.


# What is this?

Julia is designed to be [embeddable](https://docs.julialang.org/en/v1/manual/embedding/index.html%20). This project uses Julia's C API to create an [Emacs dynamic module](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html), thereby embedding Julia into Emacs. When loaded  into a running Emacs process, the dynamic module registers new Emacs Lisp functions that (indirectly) call Julia's C API functions.

The exposed wrappers in dynamic module are responsible for:

-   converting data to/from Emacs Lisp types and Julia types
-   handling errors that occur in the Julia runtime
-   managing object lifetimes

In addition, there are several included Emacs and Julia functions for things like:

-   converting some Julia code to a Elisp s-expression
-   &#x2026;


# Why?

Julia is [lispy](https://www.youtube.com/watch?v=dK3zRXhrFZY) and has [powerful metaprogramming features](https://docs.julialang.org/en/v1/manual/metaprogramming/index.html). Emacs is arguably the most powerful environment for working with lisps. By giving Emacs access to Julia's C API with a shared-memory model, I hope this project will enable


# Getting Started


## Requirements

-   Emacs 25+ that you can compile from source.
-   Julia 1.0+
-   Perserverance. This is not (yet) a 1-click install.

**NOTE: This has only been tested on Ubuntu 16.04.5 with Emacs 27.0.5 and Julia 1.0.2**.

I suspect this works on most modern GNU/Linux systems, and that OSX can be made to work with some tweaks.


## Compile Emacs

We need to compile Emacs with module support and with the RTLD\_GLOBAL flag added to dlopen.

WARNING: I can't confirm the dlopen modification will not break loading other shared libraries! I haven't seen issues in my configuration.

The requirement for this flag is mentioned here <https://docs.julialang.org/en/v1/manual/embedding/index.html#High-Level-Embedding-1>

Unfortunately, this requires modifying the Emacs source and recompiling. This is the ugliest part of the setup right now, and will hopefully go away. Let me know if you have ideas regarding this.

Assuming you've cloned Emacs to `~/emacs-src`, edit `~/emacs-src/src/dynlib.c` so that `RTLD_GLOBAL` is OR'd to the `dlopen` flags:

    dynlib_handle_ptr
    dynlib_open (const char *path)
    {
      return dlopen (path, RTLD_LAZY | RTLD_GLOBAL);
    }

Then, in `~/emacs-src`, run:

    make clean
    ./configure --with-modules
    make

If the build succeeds, your new Emacs executable will be `~/emacs-src/src/emacs`. You can `make install` it as root if you'd like, but I prefer to keep a clean Emacs in my `/usr/local/bin` as a backup.


## Setting shared library directories

You need to ensure the directory containing the Julia shared library `libjulia.so` (built by default when compiling Julia) is in the list of paths `ld` searches when dynamically linking. This must be set in the environment you launch Emacs from.

I currently do this by setting `export LD_LIBRARY_PATH=<libjulia_dir>` in the [bin/emacs](bin/emacs) wrapper script, where `<libjulia_dir>` is wherever `libjulia.so` is installed on your system. On Linux you can quickly find it by running `find / -name "\*libjulia.so\*".` If you built Julia from source, it's in `<julia-src-root>/usr/lib`.

TODO: I know there's a better way with `ldconfig`, but I haven't looked into it yet. Feel free to update this section of the docs if you know!

Read more about shared libraries [here](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=11&cad=rja&uact=8&ved=2ahUKEwjc__bkrMbfAhVG1hoKHZJyAmEQFjAKegQIChAB&url=http%253A%252F%252Ftldp.org%252FHOWTO%252FProgram-Library-HOWTO%252Fshared-libraries.html&usg=AOvVaw3xPHqyZEvQn6LR-oQzV4g1).


## Install the Emacs package

With your freshly-built Emacs launched with the above instructions, you should now clone this repository somewhere in your emacs load path. Then evaluate `(require 'julia)` and go through the first-load compilation phase (with lots of gcc warnings spewed, sorry I'm not as good as -Wall wants me to be). If successful, `julia-core.so` should be compiled and loaded into your Emacs process. Your `*Messages*` buffer should contain the line:

    Loading .../julia/julia-core.so (module)...done

Verify further by trying to call one of the functions exposed by `julia-core.so` from Elisp. For example:

    (julia-eval "\"Julia knows pi: $pi\"")

    Julia knows pi: π = 3.1415926535897...


# Running the tests

Tests run in a separate Emacs process in "batch" mode (so no new Emacs frame will appear).

From the top-level directory of this repo, run:

    make test


# Re-building the shared library

You should only need to do this if you modify the C sources.

From the top-level directory of this repo, run:

    make


# Status / Dev Notes


## Data Marshalling


### Emacs -> Julia

Unstarted.

For now we serialize any data from Emacs into a string representations of Julia code, then just eval it.

e.g.

    (julia-within-lib-dir
                 (julia-eval-blind
                  (with-temp-buffer
                    (insert-file-contents-literally "EmacsJulia/src/EmacsJulia.jl")
                    (buffer-string))))


### Julia -> Emacs <code>[2/5]</code>

-   [X] ints, floats
-   [X] strings
-   [ ] 1d arrays
-   [ ] multi-dimensional arrays
-   [ ] arbitrary serializable objects

Maybe we can just try to serialize complex objects with jld (hdf5 for Julia), then try to deserialize on the Emacs side.


## Error handling

Very little at the moment.

We do attempt to capture


## Memory Management

Current tests are too simple for exploring this.

The only relevant code for this right now is in [julia-core.c](julia-core.c):

    JL_GC_PUSH1(&val);
    emacs_value emacs_val = jl_to_elisp(env, val);
    JL_GC_POP();

which ensures `val` isn't garbage collected by Julia while we convert it to an Elisp value.


## Concurrency

Currently, if Julia blocks, Emacs hangs. Julia is being compiled with threads enabled, so this might be avoidable.

FWIW, emacs-zmq spawns a slave Emacs process to handle polling channels. I hope that's not necessary here.


## Tests


## Benchmarks

-   Round-trip speed test against Julia REPL and jupy-julia


## Build

-   Why is `make all` required, instead of just `make`?
-   Can we also build flisp as a shared library?


## Development

While we're still relying on a patched Emacs, it would be nice to:

-   provide a patch file for the dlopen tweak (one line)
-   provide a container image with the patched pre-built Emacs (for testing)


# Acknowledgments

-   <https://github.com/dzop/emacs-zmq> for inspiration and great examples. The first-load automatic compilation code was taken from here (and probably other things).
-   John Kitchin for his [module helpers](https://github.com/jkitchin/emacs-modules).
-   This thorough Emacs module documentation: <http://phst.github.io/emacs-modules.html>
-   THe `linefilter!` function to cleanup Julia `Expr` objects was taken from <https://github.com/chakravala/SyntaxTree.jl>
