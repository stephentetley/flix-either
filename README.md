# flix-either

The `Either` and `ChoiceN` data types for Flix .

`Either` is the neutral anonymous sum type. It is equivalent to `Result` in the 
Flix standard library, but the naming of `Result` and its constructors biases
it to be used for programming with success and errors.

`Choice2` to `Choice8` are F# style alternatives to `Either` implementing
greater arities. Some utility functions are provided.

`Choice9` and higher are implemented as just datatypes with no utility 
functions, they are anticipated being used solely with pattern matching.


License: Apache 2.0

