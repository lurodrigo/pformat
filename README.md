pformat
=======

pformat provides powerful string interpolation and formatting capabilities. It is a R language implementation of Python's new style string formatting API, coupled with some original features.

Features
--------

-   Supports numeric (`integer`, `double`, `complex`), date (`Date`, `IDate`, `POSIXt`) and `character` types out-of-the-box.
-   Supports **expressions** inside fields, avoiding intermediate variables.
-   named fields can be evaluated inside lists or data frames, saving typing.
-   Supports evaluation on the current environment. You can feed `pformat()` only the format string, and it will look for corresponding data on the environment.
-   It is **vectorized**, allowing formatting of extensive amounts of data with a single call.
-   It is possible to **preparse** format strings, which may avoid unnecessary calls and reduce computing time inside loops.
-   It is **extensible**: you can write custom formatters for your classes.

Examples
--------

### Basic usage

A pair of braces and everything inside them form a placeholder. The simplest use case is that of positional formatting: arguments are assigned to placeholders according to their position.

``` r
pformat("{} {}", "one", "two")
#> [1] "one two"
```

You can give placeholders an explicit positional index. This allows for re-arranging the order of display without changing the arguments. *Note:* contrary to Python, in pformat indices start from 1 and not from 0.

``` r
pformat("{2} {1}", "one", "two")
#> [1] "two one"
```

### Vectorization

pformat is vectorized, so you can generate many formatted strings using a single call

``` r
pformat("Name: {}; Age: {}", c("Abby", "Bob", "Carl"), 22:24)
#> [1] "Name: Abby; Age: 22" "Name: Bob; Age: 23"  "Name: Carl; Age: 24"
```

Usual recycling rules apply:

``` r
pformat("{}-{}{}", "expr", c("a", "b", "c"), 1:2)
#> [1] "expr-a1" "expr-b2" "expr-c1"
```

### Named placeholders

pformat provides three ways of using named placeholders:

-   keyword arguments.
-   passing a list, data frame, or environment as the first parameter. This allows pformat to be chained using the pipe operator `%>%` provided by the `magrittr` package.
-   evaluation on the current environment.

That's also the order which pformat uses when looking for corresponding names. The following example illustrates the three methods, where all calls produce same output.

``` r
# keyword arguments
pformat("Name: {name}; Age: {age}", name = c("Abby", "Bob", "Carl"), age = 22:24)
#> [1] "Name: Abby; Age: 22" "Name: Bob; Age: 23"  "Name: Carl; Age: 24"

# passing a data frame as the first parameter
people = data.frame(name = c("Abby", "Bob", "Carl"), age = 22:24)
pformat(people, "Name: {name}; Age: {age}")
#> [1] "Name: Abby; Age: 22" "Name: Bob; Age: 23"  "Name: Carl; Age: 24"

# the same as above but using pipe
library(magrittr)
people %>% pformat("Name: {name}; Age: {age}")
#> [1] "Name: Abby; Age: 22" "Name: Bob; Age: 23"  "Name: Carl; Age: 24"

# evaluation on the environment
name = c("Abby", "Bob", "Carl")
age = 22:24
pformat("Name: {name}; Age: {age}")
#> [1] "Name: Abby; Age: 22" "Name: Bob; Age: 23"  "Name: Carl; Age: 24"
```

### Expressions

Placeholders can hold not only identifiers but any R expression, provided that its result type is supported by pformat.

``` r
n = 9
pformat("{n} x {i} = {n * i}", i = 1:10)
#>  [1] "9 x 1 = 9"   "9 x 2 = 18"  "9 x 3 = 27"  "9 x 4 = 36"  "9 x 5 = 45" 
#>  [6] "9 x 6 = 54"  "9 x 7 = 63"  "9 x 8 = 72"  "9 x 9 = 81"  "9 x 10 = 90"
```

``` r
df = data.frame(name = c("Walter", "Frederick", "Lindsey"), 
                surname = c("Unzueta", "Winstead", "Chambers"))
df %>% pformat("{substr(name, 1, 1)}. {surname}")
#> [1] "W. Unzueta"  "F. Winstead" "L. Chambers"
```

Formatting
----------

### Integers, floating point numbers, and strings

pformat uses (will use) the same format string syntax as Python. See [Python docs](https://docs.python.org/3/library/string.html#string-formatting). Development in progress.

### Dates

Date formatting uses the traditional `strftime()` conversion specification, just like Python. See `strftime`'s help page for more details.

``` r
pformat("Mother's day: {:%d/%m/%Y}", as.Date("2016-05-08"))
#> [1] "Mother's day: 08/05/2016"
```
