
context("A-200 create_tree")

test_that("test A-200 create_tree", {
  expect_error(create_tree("()"), "'text' is of length 0.")

  expect_equal(
    is.list(create_tree("(2)")), T
  )


  expect_equal(
    is.list(create_tree("2 + 2")), T
  )

  expect_equal(
    is.list(create_tree("2+2")), T
  )

  expect_equal(
    is.list(create_tree("(2+2)*3")), T
  )

  expect_equal(
    is.list(create_tree("-3")), T
  )

  expect_equal(
    is.list(create_tree("c(1,2)")), T
  )

  x <- create_tree("1 + 2")

  expect_equal(
    is.list(x[["pval"]]), T
  )

  expect_equal(
    is.list(x[["eval"]]), T
  )

  expect_equal(
    x[["eval"]][[1]], "+"
  )

  expect_equal(
    is.list(x[["eval"]][[2]]), T
  )

  expect_equal(
    is.list(x[["eval"]][[3]]), T
  )


  expect_equal(
    x[["eval"]][[2]][[1]], "atomic"
  )
  expect_equal(
    x[["eval"]][[2]][[2]], "1"
  )

  expect_equal(
    x[["eval"]][[3]][[1]], "atomic"
  )
  expect_equal(
    x[["eval"]][[3]][[2]], "2"
  )


  #########################################################################################################

  tree <- list(
    pval = list(),
    eval = list("atomic", "FALSE")
  )

  expect_equal(
    create_tree("FALSE"), tree
  )

  #########################################################################################################

  inner_pval <- list(
    "\\0" = list(
      pval = list(),
      eval = list("atomic", "2")
    )
  )

  pval <- list(
    "\\0" = list(
      pval = inner_pval,
      eval = list("atomic", "\\0")
    )
  )

  tree <- list(
    pval = pval,
    eval = list("atomic", "\\0")
  )


  expect_equal(
    create_tree("((2))"), tree
  )




  #########################################################################################################


  tree <- list(
    pval = list(),
    eval = list("+", list("atomic", "2"), list("atomic", "3"))
  )

  expect_equal(
    create_tree("2 +3"), tree
  )
 
  #########################################################################################################

  inner_pval <- list(
    "\\0" = list(
      pval = list(),
      eval = list("+", list("atomic", "3"), list("atomic", "4"))
    )
  )

  pval <- list(
    "\\0" = list(
      pval = inner_pval,
      eval = list("atomic", "\\0")
    )
  )

  tree <- list(
    pval = pval,
    eval = list("*", list("atomic", "2"), list("atomic", "\\0"))
  )

  expect_equal(
    create_tree("2 * ((3+4))"), tree
  )
})
