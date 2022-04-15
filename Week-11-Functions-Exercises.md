Functions
================
YOUR NAME

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.8
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- tibble::tibble(
a = c(rnorm(9), -99),
b = c(-999, -99, rnorm(8)),
c = c(0, rnorm(9)),
d = rnorm(10)
)
```

``` r
(df$a <- (df$a - min(df$a)) / (max(df$a) - min(df$a)))
```

    ##  [1] 0.9873621 0.9924994 0.9920401 0.9827906 0.9871695 0.9833538 1.0000000
    ##  [8] 0.9828969 0.9798950 0.0000000

``` r
(df$b <- (df$b - min(df$b)) / (max(df$b) - min(df$b)))
```

    ##  [1] 0.0000000 0.8988692 1.0000000 0.9954003 0.9966566 0.9982396 0.9998651
    ##  [8] 0.9959312 0.9986175 0.9978911

``` r
(df$c <- (df$c - min(df$c)) / (max(df$c) - min(df$c)))
```

    ##  [1] 0.3777947 0.4407489 0.8269697 0.3889949 0.3374585 0.5957958 0.3005257
    ##  [8] 0.1959551 0.0000000 1.0000000

``` r
(df$d <- (df$d - min(df$d)) / (max(df$d) - min(df$c)))
```

    ##  [1] 0.125635210 0.898276266 0.297412128 0.115299591 0.967683646 0.424226294
    ##  [7] 0.924133556 0.000000000 1.603135648 0.004815206

Write a function to automate the calculation of the rescale:

``` r
rescale01 <- function(x) {
  rng <- range(df$a)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(df$a)
```

    ##  [1] 0.9873621 0.9924994 0.9920401 0.9827906 0.9871695 0.9833538 1.0000000
    ##  [8] 0.9828969 0.9798950 0.0000000

``` r
rescale01(df$b)
```

    ##  [1] 0.0000000 0.8988692 1.0000000 0.9954003 0.9966566 0.9982396 0.9998651
    ##  [8] 0.9959312 0.9986175 0.9978911

``` r
rescale01(df$c)
```

    ##  [1] 0.3777947 0.4407489 0.8269697 0.3889949 0.3374585 0.5957958 0.3005257
    ##  [8] 0.1959551 0.0000000 1.0000000

``` r
rescale01(df$d)
```

    ##  [1] 0.125635210 0.898276266 0.297412128 0.115299591 0.967683646 0.424226294
    ##  [7] 0.924133556 0.000000000 1.603135648 0.004815206

## Your Turn 1

Rerun the tibble to revert back to the original data. Write a function
to replace -99 with NA, called `fix_missing()`

``` r
df <- tibble::tibble(
a = c(rnorm(9), -99),
b = c(-999, -99, rnorm(8)),
c = c(0, rnorm(9)),
d = rnorm(10)
)


#Here are the repeated calculations
df$a[df$a==-99]<-NA
df$b[df$b==-99]<-NA
df$c[df$c==-99]<-NA
df$d[df$d==-99]<-NA
```

``` r
fix_missing <- function(y) {
  y[y==-99]<-NA
  y
}

fix_missing(df)
```

    ## # A tibble: 10 × 4
    ##         a         b      c      d
    ##     <dbl>     <dbl>  <dbl>  <dbl>
    ##  1 -0.648 -999       0     -1.13 
    ##  2 -0.946   NA       1.06  -0.913
    ##  3 -0.467    0.199   0.790  1.59 
    ##  4  0.525    1.22    0.143  1.63 
    ##  5  2.05    -0.381   1.28   0.838
    ##  6  0.991    0.967   0.224  0.756
    ##  7 -1.05     0.336  -1.47  -0.182
    ##  8 -0.458    0.0459 -1.67  -0.298
    ##  9 -1.88     1.01   -1.60   1.47 
    ## 10 NA        0.884  -0.200  0.269

``` r
df
```

    ## # A tibble: 10 × 4
    ##         a         b      c      d
    ##     <dbl>     <dbl>  <dbl>  <dbl>
    ##  1 -0.648 -999       0     -1.13 
    ##  2 -0.946   NA       1.06  -0.913
    ##  3 -0.467    0.199   0.790  1.59 
    ##  4  0.525    1.22    0.143  1.63 
    ##  5  2.05    -0.381   1.28   0.838
    ##  6  0.991    0.967   0.224  0.756
    ##  7 -1.05     0.336  -1.47  -0.182
    ##  8 -0.458    0.0459 -1.67  -0.298
    ##  9 -1.88     1.01   -1.60   1.47 
    ## 10 NA        0.884  -0.200  0.269

Now let’s write a function that allows us to easily adapt our rescaling
to include a range (min, max) to modify the data.

``` r
#Rescale to [0, 1]
0 + (1 - 0) * ((df$a - min(df$a)) / (max(df$a) - min(df$a)))
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
# Rescale to [-1, 1]
-1 + (1 - -1) * ((df$b - min(df$b)) / (max(df$b) - min(df$b)))
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
# Rescale to [0, 10]
0 + (10 - 0) * ((df$c - min(df$c)) / (max(df$c) - min(df$c)))
```

    ##  [1]  5.6682270  9.2576660  8.3489523  6.1533306 10.0000000  6.4272376
    ##  [7]  0.6871781  0.0000000  0.2265444  4.9898374

``` r
rescale <- function(x, min =  0, max = 1) {
  min + (max - min) * ((df$a - min(df$a)) / (max(df$a) - min(df$a)))

}

rescale(df$a)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
rescale(df$b, min = -1, max = 1)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
rescale(df$c, max = 10)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

## Your Turn 2

Expand your function from ‘Your Turn 1’ to allow for any possible
missing value type.

``` r
fix_missing <- function(y, missing_val = -99) {
  y[y==missing_val]<-NA
  y
}

fix_missing(df$a, -99)
```

    ##  [1] -0.6478970 -0.9462778 -0.4666582  0.5245643  2.0464840  0.9906984
    ##  [7] -1.0485426 -0.4582941 -1.8824686         NA

``` r
fix_missing(df$b, -999)
```

    ##  [1]          NA          NA  0.19939700  1.22057319 -0.38051419  0.96723056
    ##  [7]  0.33633754  0.04591833  1.01137520  0.88351054

# Take Aways

To write a function,

1.  Write code that solves the problem for a real object  
2.  Wrap the code in `function(){}` to save it  
3.  Add the name of the real object as the function argument

This sequence will help prevent bugs in your code (and reduce the time
you spend correcting bugs).
