
context("A-500 parameters")

test_that("test A-500 parameters", {
    set.seed(101)
    y <- evalR::eval_text("2+rnorm(1)", valid_functions="rnorm")
    expect_equal(
        y, 1.67396350948461
    )


    expect_warning(
        evalR::eval_text("2+rnorm(1)")
    )
})
