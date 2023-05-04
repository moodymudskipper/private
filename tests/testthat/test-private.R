test_that("private", {
  print_cubed <- function(x) {
    x |> cube() |> format()
  }
  print_cubed %private% cube <- function(x) {x * square(x)}
  print_cubed %private% cube %private% square <- function(x) {x^2}
  print_cubed %private% format <- function(x) {paste("Cubed value:", x)}

  expect_equal(print_cubed(3), "Cubed value: 27")
})
