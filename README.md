
# compsuppress

<!-- badges: start -->

<!-- badges: end -->

compsuppress allows for easy complementary data suppression via a
singular function call. This is done via a recursive call stack and a
series of helper functions.

## Installation

You can install the development version of compsuppress from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JuliaHealeyParera/compsuppress")
```

## Example

``` r
library(compsuppress)

x <- data.frame( 
  x = c('ggg', 'hhh', 'iii', 'xxx', 'aaa', 'sss', 'ddd'),
  y = c(1, 2, 34, 12, 23, 12, 1),
  z = c(23, 41, 1, 0, 24, 21, 1),
  w = c(123, 3, 12, 2, 23, 45, 3), 
  r = c(45, 23, 5, 34, 1, 23, 4)
)

suppress(x, 5, '-', c('y', 'z', 'w', 'r'))
#>     x  y  z   w  r
#> 1 ggg  -  - 123 45
#> 2 hhh  - 41   - 23
#> 3 iii 34  -  12  -
#> 4 xxx  -  0   - 34
#> 5 aaa  - 24  23  -
#> 6 sss 12 21  45 23
#> 7 ddd  -  -   -  -
```
