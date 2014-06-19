# Cutaway

<img src="./cutaway.jpg" />

> Sometimes you just have to cut to the heart of the matter

Cutaway provides a mapping from persistent tree datastructures like
those used in `clojure.tools.analyzer` or `hiccup` to a mutable,
single depth tree, and back again. The goal of cutaway is to
simultaneously improve the performance and simplify the implementation
of algorithms requiring updates to nested structures.

Cutaway provides tools which assist in mapping complex nested
structures to flat, transient id to value mappings, update and
traversal operations over flattened structures and an efficient
mapping back to the original nested structure.

## Usage



## License

Copyright © 2014 Reid "arrdem" McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
