NOTE: This project is in-development not ready for production use! See the end of this document for notes on how to contribute.


# What is this?

Julia is designed to be [embeddable](https://docs.julialang.org/en/v1/manual/embedding/index.html%20). This project uses Julia's C API to create an [Emacs dynamic module](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html), thereby embedding Julia into Emacs. When loaded  into a running Emacs process, the dynamic module registers new Emacs Lisp functions that (indirectly) call Julia's C API functions.

The exposed wrappers in dynamic module are responsible for:

-   converting data to/from Emacs Lisp types and Julia types
-   handling errors that occur in the Julia runtime
-   managing object lifetimes

In addition, this project includes several Emacs and Julia functions for things like:

-   converting Julia code to an s-expression readable by Emacs Lisp
-   &#x2026;


# Why?

Emacs is [arguably the best environment](https://www.youtube.com/watch?v=xzTH_ZqaFKI) for working with languages in the [Lisp family](http://wiki.c2.com/?LispFamily) (e.g. Common Lisp, Scheme, Emacs Lisp). Julia is a lisp in disguise (see [Julia metaprogramming](https://docs.julialang.org/en/v1/manual/metaprogramming/index.html), [this talk](https://confengine.com/functional-conf-2016/proposal/3153/julia-a-lisp-for-fast-number-crunching), and [this talk](https://www.youtube.com/watch?v=dK3zRXhrFZY)). By giving Emacs access to Julia's C API, we can better leverage the existing lisp tools and apply them to Julia.

Here are some examples of tools that don't yet exist, but seem within reach:

-   A structure editor (like [paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html), [smartparens](https://github.com/Fuco1/smartparens), [lispy](https://github.com/abo-abo/lispy), etc&#x2026;)
-   A code auto-formatter
-   An in-line macro-expander
-   Better hooks for tools like [expand-region](https://github.com/magnars/expand-region.el)
-   A [Rebugger](https://github.com/timholy/Rebugger.jl) mode.
-   A "deparser" for converting s-expressions back to Julia code.
    -   Note: Julia's femtolisp parser has a `deparse` function that's not exposed. The [emacs-julia-parser](https://github.com/dzop/emacs-julia-parser/) exposes `julia-deparse`, but it seems unfinished.

As a reference, the existing tools are spanned by

-   Julia Mode - Syntax highlighting, &#x2026;
-   [Juila REPL](https://github.com/tpapp/julia-repl) - Send code from a buffer to a Julia REPL controlled by Emacs.
-   [LanguageServer.jl](https://github.com/JuliaEditorSupport/LanguageServer.jl) - Jump to definition, etc&#x2026; Not yet working for Julia 1.0+.
-   [ob-ipython](https://github.com/gregsexton/ob-ipython) / [emacs-jupyter](https://github.com/dzop/emacs-jupyter) - Jupyter integration (evaluation, completion, inspection)
-   [emacs-julia-parser](https://github.com/dzop/emacs-julia-parser/) - An ambitious (in-progress) attempt to port Julia's femtolisp parser to Emacs Lisp.


# Getting Started


## Requirements

-   Emacs Lisp experience. There is currently very little use of this package other than to aid in developing Emacs Lisp tools for Julia.
-   Emacs 25+ that you can build from source.
-   A `libjulia.so` from Julia 1.0+.
    -   I have this in `/usr/local/lib` after building Julia from source with `prefix=/usr/local`.
-   Perseverance. This is not (yet) a 1-click install.

**NOTE: This has only been tested on Ubuntu 16.04.5 with Emacs 27.0.5 and Julia 1.0.2**.

I suspect this works on most modern GNU/Linux systems, and that OSX can be made to work with some tweaks.


## Compile Emacs

We need to compile Emacs with module support using the `--with-modules` configure option. Additionally, we need to add the `RTLD_GLOBAL` flag to the Emacs `dlopen` call, per the [Julia embedding docs](https://docs.julialang.org/en/v1/manual/embedding/index.html#High-Level-Embedding-1).

**WARNING: I can't confirm the `dlopen` modification will not break loading other shared libraries!** I haven't seen issues in my configuration.

Unfortunately, adding the `dlopen` flag requires modifying the Emacs source and recompiling. This is the ugliest part of the setup right now, and will hopefully go away soon. Please let me know if you have ideas on this.

Assuming you've cloned Emacs to `~/emacs-src`, edit `~/emacs-src/src/dynlib.c:dynlib_open` so that `RTLD_GLOBAL` is OR'd to the `dlopen` flags:

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


## Configure shared library directories

You need to ensure the directory containing the Julia shared library `libjulia.so` (built by default when compiling Julia) is in the list of paths `ld` searches when dynamically linking.

If you installed Julia to `/usr/local/`, you're likely fine.

Otherwise, you can use `ldconfig` or `export LD_LIBRARY_PATH=<julia_lib_dir>` to tell the linker the location of `libjulia.so`.

NB: Emacs won't re-load a dynamic module, so you'll have to restart it to test changes like this. `make test` will spawn a fresh Emacs process for you, which is helpful in cases like this.

NB2: On Linux you can quickly find the library location by running `find / -name "\*libjulia.so\*".`

Read more about shared libraries [here](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=11&cad=rja&uact=8&ved=2ahUKEwjc__bkrMbfAhVG1hoKHZJyAmEQFjAKegQIChAB&url=http%253A%252F%252Ftldp.org%252FHOWTO%252FProgram-Library-HOWTO%252Fshared-libraries.html&usg=AOvVaw3xPHqyZEvQn6LR-oQzV4g1).


## Install the Emacs package

Run your freshly-built Emacs launched with the above instructions, then clone this repository somewhere in your Emacs load path. Evaluate `(require 'julia)` and go through the first-load compilation steps (don't mind the `gcc` warnings). If successful, `julia-core.so` should be compiled and loaded into your Emacs process. Your `*Messages*` buffer should contain the line:

    Loading <install-dir>/julia-core.so (module)...done

Verify the bindings work by trying to call one of the functions exposed by `julia-core.so` from Emacs Lisp. For example:

    (julia-eval "\"Julia knows pi: $pi\"")

    Julia knows pi: π = 3.1415926535897...


# Re-building the shared library

You should only need to do this if you modify the C sources.

From the top-level directory of this repository, run:

    make

In case you need it, `make clean` wipes away the object files and shared library.


# Running the tests

Tests run in a separate Emacs process in "batch" mode (so no new Emacs frame will appear).

From the top-level directory of this repository, run:

    make test


# Status / Development Notes


## Data Marshalling


### Emacs -> Julia

Not started.

For now we serialize any data from Emacs into a string representations of Julia code, then just `eval` it.

e.g.

    (julia-within-lib-dir
                 (julia-eval-blind
                  (with-temp-buffer
                    (insert-file-contents-literally "EmacsJulia/src/EmacsJulia.jl")
                    (buffer-string))))


### Julia -> Emacs <code>[2/5]</code>

-   [X] integers, floats
-   [X] strings
-   [ ] 1d arrays
-   [ ] multi-dimensional arrays
-   [ ] arbitrary serialize-able objects

Maybe we can just try to serialize complex objects with `jld` (`hdf5` for Julia), then try to de-serialize on the Emacs side.


## Error handling

Very little at the moment.

We do capture exceptions thrown by Julia and return the exception type as a string to Emacs (+ emit some stdout). This is obviously brittle and needs work.


## Memory Management

Current tests are too simple for exploring this.

The only relevant code for this right now is in [julia-core.c](julia-core.c):

    jl_value_t *val = jl_eval_string(str_arg);
    ...
    JL_GC_PUSH1(&val);
    emacs_value emacs_val = jl_to_elisp(env, val);
    JL_GC_POP();

which ensures `val` isn't garbage collected by Julia while we convert it to an Emacs Lisp value.


## Concurrency

Currently, if Julia blocks, Emacs hangs. Julia is being compiled with threads enabled, so this might be avoidable.

FWIW, [emacs-zmq](https://github.com/dzop/emacs-zmq)sss spawns a slave Emacs process to handle polling channels. I hope that's not necessary here.


## Tests


## Benchmarks

-   Round-trip speed test against Julia REPL and jupy-julia


## Build

-   <del>Why is `make all` required, instead of just `make`?</del>
-   Can we also build `flisp` as a shared library?


## Development

While we're still relying on a patched Emacs, it would be nice to:

-   provide a patch file for the `dlopen` tweak (one line)
-   provide a container image with the patched pre-built Emacs (for testing)


# Acknowledgments

-   [emacs-zmq](https://github.com/dzop/emacs-zmq) for inspiration and great examples. The first-load automatic compilation code was taken from here (and probably other things).
-   John Kitchin for his [module helpers](https://github.com/jkitchin/emacs-modules).
-   This thorough Emacs module documentation: <http://phst.github.io/emacs-modules.html>
-   The `linefilter!` function to cleanup Julia `Expr` objects was taken from <https://github.com/chakravala/SyntaxTree.jl>
