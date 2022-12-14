## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  eval(parse(text="unverified_code"))

## -----------------------------------------------------------------------------
x <- evalR::create_tree("2+3")
str(x)

## -----------------------------------------------------------------------------
tree <- list(
  pval = list(),
  eval = list("+", list("atomic", "2"), list("atomic", "3"))
)
testthat::expect_equal(x,tree)

## -----------------------------------------------------------------------------
x <- evalR::create_tree("(2)+(3)")
str(x)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
x <- evalR::create_tree("2")
str(x)

## -----------------------------------------------------------------------------
x <- evalR::create_tree("-2")
str(x)

## -----------------------------------------------------------------------------
x <- evalR::create_tree("-(2)")
str(x)

## -----------------------------------------------------------------------------
x <- evalR::create_tree("2+3")
y <- evalR::eval_tree(x)
print(y)

## -----------------------------------------------------------------------------
y <- evalR::eval_text("2+3")
print(y)

## -----------------------------------------------------------------------------
y <- evalR::eval_text("2+rnorm(1)", valid_functions="rnorm")
print(y)

## -----------------------------------------------------------------------------
map_obj <- list("#" = data.frame(x = 1:5, y = 5:1))
y <- evalR::eval_text("log(#x#)", map=map_obj)
print(y)

## -----------------------------------------------------------------------------
map_obj <- list("#" = data.frame(x = 1:5, y = 5:1),"$" = list(z = -(1:5)))
y <- evalR::eval_text("#x# + $z$", map=map_obj)
print(y)

