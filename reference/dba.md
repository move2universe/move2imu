# Calculate dynamic body acceleration (DBA) for an `acc` vector

Compute vectorial dynamic body acceleration (VeDBA) or overall dynamic
body acceleration (ODBA) for each burst in an `acc` vector.

Dynamic body acceleration is computed by subtracting the static
acceleration component from each axis and then summarizing the remaining
dynamic acceleration with:

- VeDBA: mean of the Euclidean norm across samples

- ODBA: mean of the sum of absolute values across samples

## Usage

``` r
vedba(x)

odba(x)
```

## Arguments

- x:

  An `acc` vector.

## Value

A numeric vector the same length as `x`.

## Examples

``` r
a <- acc_example()

vedba(a)
#> [1] 0.6996903 0.5915996

odba(a)
#> [1] 0.8675162 0.8263061
```
