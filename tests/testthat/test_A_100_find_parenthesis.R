
context("A-100 find_parenthesis")

test_that("test A-100 find_parenthesis", {
  expect_equal(
    find_parenthesis("()"), "()"
  )

  expect_equal(
    find_parenthesis("44 +rrf "), ""
  )

  expect_equal(
    find_parenthesis(" dd + (x*(3)+(nj+k))+c"), "(x*(3)+(nj+k))"
  )


  expect_equal(
    find_parenthesis(" 3 ^ (dd + (2+2))"), "(dd + (2+2))"
  )

  expect_equal(
    find_parenthesis(" (3 ^ (dd + (2+2)))"), "(3 ^ (dd + (2+2)))"
  )


  expect_equal(
    find_parenthesis("24 + ( (2 + (3)) )+ (9+6)"), "( (2 + (3)) )"
  )
})
