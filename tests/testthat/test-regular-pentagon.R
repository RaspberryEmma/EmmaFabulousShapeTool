test_that("regular pentagon area calculations correct", {
  expect_equal(regular.pentagon.area(5), 43.01, tolerance=0.01)
})

test_that("regular pentagon perimeter calculations correct", {
  expect_equal(regular.pentagon.perimeter(5), 25.0, tolerance=0.01)
})
