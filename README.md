
<!-- README.md is generated from README.Rmd. Please edit that file -->

# private

- Functions that are defined in the body of other functions cannot be
  tested easily and clutter the body of the parent function, it’s also
  not always clear if they use lexical scoping or not
- If we move these functions to the namespace they make the codebase
  harder to understand because we lose this relationship

{private} proposes a solution to these problems.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/private")
```

## Example

``` r
library(private)
print_cubed <- function(x) {
  x |> cube() |> format()
}
print_cubed %private% cube <- function(x) {x * square(x)}
# this works recursively
print_cubed %private% cube %private% square <- function(x) {x^2}
print_cubed %private% format <- function(x) {paste("Cubed value:", x)}

print_cubed(3)
#> [1] "Cubed value: 27"

# printing methods show the class, children and namespace
print_cubed
#> <private_parent> with children: cube(), format()
#> function(x) {
#>   x |> cube() |> format()
#> }
#> <environment: namespace:R_GlobalEnv/print_cubed>

print_cubed %private% format
#> <private_child>
#> function(x) {paste("Cubed value:", x)}
#> <environment: namespace:R_GlobalEnv/print_cubed>

print_cubed %private% cube
#> <private_parent> with children: square()
#> <private_child>
#> function(x) {x * square(x)}
#> <environment: namespace:R_GlobalEnv/print_cubed/cube>

print_cubed %private% cube %private% square
#> <private_child>
#> function(x) {x^2}
#> <environment: namespace:R_GlobalEnv/print_cubed/cube>

# private closures are not bound to the local environment (the namespace if we're in a package)
ls()
#> [1] "print_cubed"

ls(environment(print_cubed))
#> [1] "cube"   "format"

ls(environment(print_cubed %private% cube))
#> [1] "square"

# we can test the private methods
(print_cubed %private% cube)(4)
#> [1] 64
```

# Private S3 methods

Defining private S3 methods is straightforward

``` r
print_vec  <- function(data) {
  data <- as.character(data)
  writeLines(sprintf("%s: %s", seq_along(data), data))
  invisible(NULL)
}

print_vec %private% as.character.data.frame <- 
  function(x, ...) {stop("can't convert a data frame to a character")}
print_vec(letters[1:3])
#> 1: a
#> 2: b
#> 3: c
print_vec(cars)
#> Error in as.character.data.frame(data): can't convert a data frame to a character
```
