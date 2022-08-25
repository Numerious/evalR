
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evalR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/evalR)](https://cran.r-project.org/package=evalR)
<!-- badges: end -->

## Overview

The purpose of this package is to generate verification trees and
evaluations of user supplied statements. Trees are made by parsing a
statement into a data structure composed of lists. Safe statement
evaluations are done by executing the verification trees.

### Verification Trees

Any statement can be represented by a tree data structure. Here is a
quote from Wikipedia to explain the concept:

> In computer science, a tree is a widely used abstract data type that
> represents a hierarchical tree structure with a set of connected
> nodes. Each node in the tree can be connected to many children
> (depending on the type of tree), but must be connected to exactly one
> parent, except for the root node, which has no parent. These
> constraints mean there are no cycles or “loops” (no node can be its
> own ancestor), and also that each child can be treated like the root
> node of its own subtree, making recursion a useful technique for tree
> traversal. In contrast to linear data structures, many trees cannot be
> represented by relationships between neighboring nodes in a single
> straight line.
>
> — Wikipedia.org Tree (data structure)

By default the package will know how to parse a R statement into a tree.
In theory, you could supply your own tokens and use the package to parse
any language that follows similar grammar.

These verification trees have been used to port R statements into other
languages. For example, one use case is to write out formulas in excel
that replicate the calculations done in R.

### Safe Evaluation

Writing code that executes unverified code can be both powerful and
dangerous. A common approach to doing this is with the following
pattern:

``` r
eval(parse(text="unverified_code"))
```

The power comes from the flexibility this pattern gives us. It usually
comes with a significant performance cost, but CPU time is cheaper than
our developers.

The danger comes from unknown unknowns. Your own input on your personal
computer may not pose much of a risk. The same does not hold for input
from nefarious/clever users on a server.

Execution of a verification tree generated on unverified code is a
different story. The risk is limited to what is deemed acceptable based
on the supplied tokens. It comes with a greater performance cost, but
removes the danger from unverified code.

## Functions

The focus of this package is limited to just creating and evaluating
trees. The next sections cover the main functions.

### create_tree

The `create_tree` function takes a string and generates a tree. For
example, we can parse the simple expression `2+3`:

``` r
x <- evalR::create_tree("2+3")
str(x)
#> List of 2
#>  $ pval: list()
#>  $ eval:List of 3
#>   ..$ : chr "+"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "2"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "3"
```

We can see the structure is a list of lists.

#### Under the hood

You don’t need to understand the structure of the tree to use it. Just
like you can drive a car without knowing how an engine works. This
section will help reveal how the trees are formed.

First lets confirm that we can replicate the tree structure:

``` r
tree <- list(
  pval = list(),
  eval = list("+", list("atomic", "2"), list("atomic", "3"))
)
testthat::expect_equal(x,tree)
```

This test passes with zero error.

A full tree is made up of two main branches:

-   `pval` - stands for parenthesis values
-   `eval` - stands for verification values

##### pval

The first thing the function does is find all parenthesis blocks and
treats them as sub statements. Each of these sub statements becomes an
element of the `pval` element. The `eval` tree will have references to
these `pval` entries.

For example, lets tweak the `2+3` to `(2)+(3)`:

``` r
x <- evalR::create_tree("(2)+(3)")
str(x)
#> List of 2
#>  $ pval:List of 2
#>   ..$ \0:List of 2
#>   .. ..$ pval: list()
#>   .. ..$ eval:List of 2
#>   .. .. ..$ : chr "atomic"
#>   .. .. ..$ : chr "2"
#>   ..$ \1:List of 2
#>   .. ..$ pval: list()
#>   .. ..$ eval:List of 2
#>   .. .. ..$ : chr "atomic"
#>   .. .. ..$ : chr "3"
#>  $ eval:List of 3
#>   ..$ : chr "+"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "\\0"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "\\1"
```

To replicate the structure:

``` r
pval_list <- list(
  "\\0"=list(
    pval = list(),
    eval = list("atomic", "2")
  ),
  "\\1"=list(
    pval = list(),
    eval = list("atomic", "3")
  )
)
tree <- list(
  pval = pval_list,
  eval = list("+", list("atomic", "\\0"), list("atomic", "\\1"))
)
testthat::expect_equal(x,tree)
```

This test passes with zero error.

Now the `pval` list is not empty. We have an entry for `(2)` and `(3)`.
Each entry in `pval` is a new tree unto itself and contains `pval` and
`eval` branches.

##### eval

The `eval` branch splits the statement by operators into “atomic”
elements.

For example, if we just parse `2`:

``` r
x <- evalR::create_tree("2")
str(x)
#> List of 2
#>  $ pval: list()
#>  $ eval:List of 2
#>   ..$ : chr "atomic"
#>   ..$ : chr "2"
```

The `eval` is one level deep and the first element is the string
`atomic`. This signifies that this is an end node of the tree.

Lets expand this just a little bit:

``` r
x <- evalR::create_tree("-2")
str(x)
#> List of 2
#>  $ pval: list()
#>  $ eval:List of 2
#>   ..$ : chr "-"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "2"
```

Now `eval` is two levels deep. The first element states the operator `-`
and the second element is another branch that looks exactly like the
`eval` branch in the previous example.

If a parenthesis block is found, then the atomic element will be a
reference to the which `pval` element:

``` r
x <- evalR::create_tree("-(2)")
str(x)
#> List of 2
#>  $ pval:List of 1
#>   ..$ \0:List of 2
#>   .. ..$ pval: list()
#>   .. ..$ eval:List of 2
#>   .. .. ..$ : chr "atomic"
#>   .. .. ..$ : chr "2"
#>  $ eval:List of 2
#>   ..$ : chr "-"
#>   ..$ :List of 2
#>   .. ..$ : chr "atomic"
#>   .. ..$ : chr "\\0"
```

In this example, the `eval` second level atomic element is `\0`. This is
a reference to the `\0` named element of the `pval` branch.

### eval_tree

Given a tree, we can execute it with the function `eval_tree`. Here is a
basic example:

``` r
x <- evalR::create_tree("2+3")
y <- evalR::eval_tree(x)
print(y)
#> [1] 5
```

### eval_text

There is a convenience function that contains the tree creation stage.
The `eval_text` takes text as an input:

``` r
y <- evalR::eval_text("2+3")
print(y)
#> [1] 5
```

## Shared Parameters

These three functions share the following parameters:

-   singular_operators - tokens of length 1 that operate on a right hand
    value. For example, the `-` token is an operator to negate a vector.
    `NULL` value will be replaced with `c("-", "!")`.
-   binary_operators - tokens of any length that operate on a left and
    right hand values. For example, the `+` token is an operator that
    adds a left vector to a right vector. `NULL` value will be replaced
    with
    `c(",","|", "&", "<=", "<", ">=", ">", "==", "!=", "+", "-", "*", "%/%", "/", "%%", "%in%", ":", "^")`.
    The order determines the precedence of the operators.
-   valid_functions - tokens of any length that are prefixed on a
    parenthesis block and specify a function to run on the provided
    parameters within the block. For example, the `log` token will
    evaluate the logarithm value of the first parameter. Note named
    parameters are not support. `NULL` value will be replaced with
    `c("log","c", "any","all", "abs","ifelse")`.

For example, if you want to be able to use the function `rnorm`, then
you need to provide that as a item in the `valid_functions` parameter:

``` r
set.seed(102)
y <- evalR::eval_text("2+rnorm(1)", valid_functions="rnorm")
print(y)
#> [1] 2.180523
```

## map Parameter

The `eval_tree` and `eval_text` share the following parameter:

-   map - a named list of data.frames/lists/matrices. Where the names
    are keys for referencing the values in the parameters.

This parameter limits the scope of the execution environment (not in a
strictly technical sense). In other words, they limit what values can be
reference.

Here is a basic concert example:

``` r
map_obj <- list("#" = data.frame(x = 1:5, y = 5:1))
y <- evalR::eval_text("log(#x#)", map=map_obj)
print(y)
#> [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379
```

Here is a more complex example:

``` r
map_obj <- list("#" = data.frame(x = 1:5, y = 5:1),"$" = list(z = -(1:5)))
y <- evalR::eval_text("#x# + $z$", map=map_obj)
print(y)
#> [1] 0 0 0 0 0
```

## microbenchmark

To get a sense of the performance. Lets compare different ways we can
run `log(1+2)`:

``` r
text <- "log(1+3)"
tree <- evalR::create_tree(text)
microbenchmark::microbenchmark(
  {log(1+2)},
  {eval(parse(text=text))},
  {evalR::eval_tree(tree)},
  {evalR::eval_text(text)},
  {evalR::create_tree(text)}, n=1000)
#> Warning in microbenchmark::microbenchmark({: Could not measure a positive
#> execution time for 60 evaluations.
#> Unit: nanoseconds
#>                              expr    min     lq   mean median     uq    max
#>                {     log(1 + 2) }    100    100    439    200    300  13500
#>  {     eval(parse(text = text)) }   4400   6100   8517   6800   8850  28400
#>    {     evalR::eval_tree(tree) }  19100  21050  26502  22300  25500  92500
#>    {     evalR::eval_text(text) } 158200 166900 194175 177250 218800 296400
#>  {     evalR::create_tree(text) } 138100 147600 170996 157150 185100 290000
#>                                 n      0      0      0      0      0      0
#>  neval
#>    100
#>    100
#>    100
#>    100
#>    100
#>    100
```

The pure R evaluation is much faster than any of the other methods. This
is no surprise.

The `evalR::eval_tree` block takes a couple times longer than the
`eval(parse(text))` execution. This is a trade off considering the
reduced risk. The `eval(parse(text))` execution is sometimes much slower
when ran outside R markdown.

We also see that `evalR::eval_text` takes much longer than
`evalR::eval_tree`. The majority of the `evalR::eval_text` time comes
from the internal call to `evalR::create_tree`. This is where the no
free lunch principle comes into play. The cost of the reduced risk is
spent in creating the tree.

If you’re lucky enough to have a set of user input that will be
evaluated multiple times, then the design pattern of generating the tree
once and using `evalR::eval_tree` will give similar performance to a
straight `eval` call.

## Final Thoughts

This package should be viewed as a building block and not as an end unto
itself. You can consider using it anytime you’re tempted to write an
`eval(parse(text))` statement.

## Credit

Package logo was created using the
[game-icons.net](https://game-icons.net) website. Thank you for such a
great product and for people willing to share their talents with others.
