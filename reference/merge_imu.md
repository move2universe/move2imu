# Merge adjacent bursts in an IMU vector

For a given IMU vector, identify temporally adjacent bursts and merge
them into a single burst. Bursts whose end time coincides with the start
time of the next burst (within a given `tolerance`) are considered
adjacent. Bursts with different frequencies, axes, or burst data units
will not be merged.

To merge bursts with differing units, convert them to a common unit
first with
[`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md).

## Usage

``` r
merge_imu(x, ids = NULL, tolerance = 1e-06, drop = FALSE)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- ids:

  Vector indicating groups to which the elements in `x` belong. If
  provided, bursts in `x` will not be merged across different values of
  this vector, even if their timestamps and frequencies align.

- tolerance:

  Noise tolerance to use when determining whether two bursts can be
  merged. Two bursts are considered adjacent when the gap between the
  first burst's end and the second burst's start is within `tolerance`.
  Two bursts are considered to have the same frequency when their sample
  gap times (1 / frequency) are within `tolerance`.

  Increase this value to avoid merge failures at burst boundaries
  because of small timestamp irregularities. Note that this may come at
  the cost of reducing sample timestamp precision in the merged bursts.

- drop:

  Logical indicating whether to drop entries that have been merged into
  other bursts. If `drop = FALSE` (default), the output will have the
  same length as the input `x`, with `NA` values at positions where
  bursts were merged into a preceding burst. This is useful for
  retaining index matching between the input and output vectors.

## Value

A vector of the same class as `x`.

## Examples

``` r
a <- acc(
  list(cbind(X = 1:60, Y = 1:60), cbind(X = 61:100, Y = 61:100), cbind(X = 101:140)),
  frequency = units::set_units(20, "Hz"),
  start = as.POSIXct(c(0, 3, 5), tz = "UTC")
)

merge_imu(a)
#> <acceleration[3]>
#> [1] (50.5 50.5) <NA>        (120.5)    
#> # frequency: 20 [Hz]
```
