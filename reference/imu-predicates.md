# Check sensor type of an IMU vector

Determine if an IMU vector inherits from a particular class. These
functions return `TRUE` for `imu` vectors of the given subclass and
`FALSE` for all other objects.

## Usage

``` r
is_acc(x)

is_mag(x)

is_gyro(x)
```

## Arguments

- x:

  An object

## Value

`TRUE` if the object inherits from the indicated subclass. `FALSE`
otherwise.

## Examples

``` r
x <- acc(
  bursts = list(cbind(X = 1:5, Y = 1:5, Z = 1:5)),
  frequency = units::as_units(20, "Hz")
)

is_acc(x)
#> [1] TRUE

is_mag(x)
#> [1] FALSE

is_gyro(x)
#> [1] FALSE
```
