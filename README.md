
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
print_cubed %private% format <- function(x) {paste("Square value:", x)}

print_cubed(3)
#> [1] "Square value: 27"

# parents have a printing method
print_cubed
#> <private_parent> (R_GlobalEnv) with children: cube(), format()
#> function(x) {
#>   x |> cube() |> format()
#> }
#> <environment: 0x11f03c3c8>

# children too
print_cubed %private% format
#> <private_child>
#> function(x) {paste("Square value:", x)}

# they work together
print_cubed %private% cube
#> <private_child>
#> <private_parent> (R_GlobalEnv) with children: square()
#> function(x) {x * square(x)}
#> <environment: 0x11f0dfe88>

# private closures are not bound to the local environment (the namespace if we're in a package)
ls()
#> [1] "print_cubed"

# we can test the private methods
(print_cubed %private% cube)(4)
#> [1] 64
```
