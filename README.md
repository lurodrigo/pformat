# pformat
pformat provides powerful string interpolation and formatting capabilities. It
is a R language implementation of Python's new style string formatting API, 
plus some original features.

## Features

* Supports numeric (`integer`, `double`, `complex`), date (`Date`, 
`IDate`, `POSIXt`) and `character` types out-of-the-box.
* Supports **expressions** inside fields, avoiding intermediate variables.
* **`with` parameter**: named fields can be evaluated inside an list or 
data.frame, saving typing.
* Supports evaluation on the current environment. You can feed pformat() only
the format string, and it will look for corresponding data on the environment.
* It is **vectorized**, allowing formatting of extensive amounts of data with 
a single call.
* It is possible to **preparse** format strings, which may avoid unnecessary 
calls and reduce computing time inside loops.
* It is **extensible**: you can write custom formatters for your classes.

## Examples
