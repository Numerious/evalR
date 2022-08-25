
context("A-350 eval_tree")

test_that("test A-350 eval_tree", {
  tree <- list(
    pval = list(),
    eval = list("atomic", "FALSE")
  )
  x <- eval_tree(tree, singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c())

  expect_equal(
    x, F
  )


  inner_pval <- list(
    "\\1" = list(
      pval = list(),
      eval = list("atomic", "2")
    )
  )

  pval <- list(
    "\\2" = list(
      pval = inner_pval,
      eval = list("atomic", "\\1")
    )
  )

 tree <- list(
    pval = pval,
    eval = list("atomic", "\\2")
  )
  x <- eval_tree(tree, singular_operators = c(), binary_operators = c(), valid_functions = c(), map = list(), mapping_names = c())

  expect_equal(
    x, 2
  )




  expect_equal(eval_tree(create_tree("2+2")), 4)


 expect_equal(eval_tree(create_tree("2 * 3")), 6)


  expect_equal(eval_tree(create_tree("log(2 * 3)")), log(6))
})
