# diagrams-input-test
Testing of [diagrams-input](https://github.com/diagrams/diagrams-input) by loading and rendering of all images in a directory for testing.

Usage
=====

cabal install generates an executable that you can copy into a folder containing files of type
* png
* jpg
* svg

Executing the executable then reads all these images with diagrams-input and writes them again as .svg with and "out-" at the beginning using the native svg backend.
