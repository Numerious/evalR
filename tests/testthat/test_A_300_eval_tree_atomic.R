
context("A-300 eval_tree_atomic")

test_that("test A-300 eval_tree_atomic", {
  x <- evalR:::eval_tree_atomic(list("atomic", "2"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())

  expect_equal(
    x, 2
  )

  x <- evalR:::eval_tree_atomic(list("atomic", "T"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())

  expect_equal(
    x, T
  )

  x <- evalR:::eval_tree_atomic(list("atomic", "TRUE"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())

  expect_equal(
    x, T
  )

  x <- evalR:::eval_tree_atomic(list("atomic", "F"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())

  expect_equal(
    x, F
  )

  x <- evalR:::eval_tree_atomic(list("atomic", "FALSE"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())

  expect_equal(
    x, F
  )

  expect_warning(
    evalR:::eval_tree_atomic(list("atomic", "n"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list()),
    "NAs introduced by coercion"
  )


  expect_error(
    evalR:::eval_tree_atomic(list("atomic", "\\0"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = list())
  )

  pval <- list(
    "\\0" = list(
      pval = list(),
      eval = list("atomic", "FALSE")
    )
  )
  x <- evalR:::eval_tree_atomic(list("atomic", "\\0"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = pval)

  expect_equal(
    x, F
  )

  inner_pval <- list(
    "\\1" = list(
      pval = list(),
      eval = list("atomic", "FALSE")
    )
  )

  pval <- list(
    "\\1" = list(
      pval = inner_pval,
      eval = list("atomic", "\\1")
    )
  )
  x <- evalR:::eval_tree_atomic(list("atomic", "\\1"), singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c(), pval = pval)

  expect_equal(
    x, F
  )
})
