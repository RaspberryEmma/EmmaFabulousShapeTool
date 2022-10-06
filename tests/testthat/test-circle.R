test_that("circle area calculations correct", {
  expect_equal(circle.area(5), 78.54, tolerance=0.01)
})

test_that("circle perimeter calculations correct", {
  expect_equal(circle.perimeter(5), 31.42, tolerance=0.01)
})

