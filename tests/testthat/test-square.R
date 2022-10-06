test_that("square area calculations correct", {
  expect_equal(square.area(5), 25.0, tolerance=0.01)
})

test_that("square perimeter calculations correct", {
  expect_equal(square.perimeter(5), 20.0, tolerance=0.01)
})
