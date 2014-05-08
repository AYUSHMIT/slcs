The Spatial Logics for Closure Spaces model checker
===================================================

This project is a research prototype, implementing a model checker
that operates on "space" in the sense of a model described by
"topological" means, or, more precisely, by the means of Closure
Spaces. Closure spaces encompass topological spaces and graph-like
structures.

The core implementation is independent of the chosen closure space,
however, currently the logical language is tailored to a specific
implementation. This will change soon in order to provide a fully
parameterised environment.

At the moment, there are two incarnations of the source code. The
"graphs" subdirectory is experimental, and we do not recommend its
usage right now. The "images" subdirectory contains a somewhat more
stable branch.

The prerequisites are the ocaml compiler, and the camlimages library.

To compile, type "make" in the "images" subdirectory. The tool is
named "csmc" and can be invoked with 1 to 3 arguments. "make test"
invokes a test.

The first argument to "csmc" is a 24-bits bmp image. The second
(optional) argument is the name of a text file containing code to be
loaded before the current model checking session. See
example-map/openstreetmap-pisa.csmc for an example. The third
(optional) argument is the output file name. If not present, the
session is interactive and uses camlgraphics to display the loaded
image and the output of the model checker.

This documentation will be updated as soon as a more stable version of
the tool and the input language of the model checker will be
available. 

For more information, please write to 

Vincenzo Ciancia

vincenzoml at gmail dot com 

