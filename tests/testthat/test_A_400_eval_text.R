

context("A-400 eval_text")

test_that("test A-400 eval_text", {
  expect_equal(eval_text("2"), 2)


  expect_equal(eval_text("2+2"), 4)

  expect_equal(eval_text("2 * 3"), 6)


  expect_equal(eval_text("log(2 * 3)"), log(6))


  map_obj <-
    list("@" = data.frame(x = 1:10), "$" = data.frame(x = 1:5))

  expect_warning(
    eval_text("@x@*2"),
    "NAs introduced by coercion"
  )

  expect_equal(eval_text("@x@*2", map = map_obj), (1:10) * 2)

  expect_equal(eval_text("log($x$*2)", map = map_obj), log((1:5) * 2))


  map_obj <- list("#" = data.frame(x = 1:5, y = 5:1))


  expect_equal(eval_text("log(#y#*2)", map = map_obj), log((5:1) * 2))









  expect_equal(eval_text("c(1)"), 1)



  expect_equal(eval_text("c(1,1)"), c(1, 1))

  expect_equal(eval_text("1:5"), 1:5)

  expect_equal(eval_text("2^5"), 2^5)
  expect_equal(eval_text("-5"), -5)
  expect_equal(eval_text("!5"), !5)
  expect_equal(eval_text("-55"), -55)
  expect_equal(eval_text("!55"), !55)

  expect_equal(eval_text("1 - 55"), 1 - 55)
  expect_equal(eval_text("1 + !55"), 1 + !55)

  expect_equal(eval_text("c(1:5,6:10)"), 1:10)


  expect_equal(eval_text("c(1:5,6*12)"), c(1:5, 6 * 12))



  expect_equal(eval_text("2 %in% c(1:5,6*12)"), T)

  expect_equal(eval_text("6 %in% c(1:5,6*12)"), F)


  expect_equal(eval_text("ifelse(0,1,2)"), 2)
  expect_equal(eval_text("ifelse(1,1,2)"), 1)

  expect_equal(eval_text("ifelse(1,1,ifelse(1,3,4))"), 1)
  expect_equal(eval_text("ifelse(0,1,ifelse(1,3,4))"), 3)
  expect_equal(eval_text("ifelse(0,1,ifelse(0,3,4))"), 4)


  expect_equal(eval_text("ifelse(c(0,1),1,ifelse(0,3,4))"), c(4, 1))





  map_obj <- list("#" = data.frame(x = 1:5, y = 5:1))


  expect_equal(eval_text("ifelse(#y#==1,log(#y#), #y#)", map = map_obj), ifelse((5:1) == 1, log((5:1)), (5:1)))

  expect_equal(eval_text("ifelse(#y#==1,log(#y#), #y# +100 *5)", map = map_obj), ifelse((5:1) == 1, log((5:1)), (5:1) + 100 * 5))

  expect_equal(eval_text("ifelse(#y#==1,log(#y#), #y# +100 * #x#)", map = map_obj), ifelse((5:1) == 1, log((5:1)), (5:1) + 100 * (1:5)))


  # pass list instead of data.frame
  map_obj <- list("#" = list(x = 1:5, y = 5:1))


  expect_equal(eval_text("ifelse(#y#==1,log(#y#), #y#)", map = map_obj), ifelse((5:1) == 1, log((5:1)), (5:1)))


  # pass matrix instead of data.frame
  my_mat <- matrix(NA, nrow = 5, ncol = 2)
  colnames(my_mat) <- c("x", "y")
  my_mat[, "x"] <- 1:5
  my_mat[, "y"] <- 5:1
  map_obj <- list("#" = my_mat)


  expect_equal(eval_text("ifelse(#y#==1,log(#y#), #y#)", map = map_obj), ifelse((5:1) == 1, log((5:1)), (5:1)))


  set.seed(103)
  expect_equal(eval_text("rnorm(1)", valid_functions = "rnorm"), -0.785973176398258)



  expect_equal(eval_text("1:2 %in% c(1,3,4,5)"), c(T, F))

  expect_equal(eval_text("all(1:2 %in% c(1,3,4,5))"), F)
})
