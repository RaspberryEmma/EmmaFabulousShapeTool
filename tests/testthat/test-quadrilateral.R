test_that("quadrilateral area calculations correct", {
  expect_equal(quadrilateral.area(5, 6), 30.0, tolerance=0.01)
})

test_that("quadrilateral perimeter calculations correct", {
  expect_equal(quadrilateral.perimeter(5, 6), 22, tolerance=0.01)
})
