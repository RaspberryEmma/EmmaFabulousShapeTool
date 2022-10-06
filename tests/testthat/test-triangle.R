test_that("triangle area calculations correct", {
  expect_equal(triangle.area.heron.formula(3, 4, 5), 6, tolerance=0.01)
  expect_equal(triangle.area.SAS.formula(3, 4, pi/2), 6, tolerance=0.01)
})

test_that("triangle perimeter calculations correct", {
  expect_equal(triangle.perimeter(3, 4, 5), 12, tolerance=0.01)
  expect_equal(triangle.perimeter.SAS.formula(3, 4, pi/2), 12, tolerance=0.01)
})

