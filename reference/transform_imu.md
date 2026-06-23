# Apply a sensor calibration to an IMU vector

Transforms raw values from an IMU sensor to physical units (e.g., meters
per second squared) using a specified calibration.

Use
[`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
to create a calibration for `acc` vectors.

## Usage

``` r
transform_imu(x, calibration)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`).

- calibration:

  An `imu_calibration` object whose subclass matches the sensor type of
  `x`. Must be the same length as `x` or length 1, in which case the
  calibration is recycled to all elements of `x`.

  Currently, only
  [`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
  is supported.

## Value

An IMU vector of the same length as `x`, with each burst transformed by
the corresponding calibration.

## Details

An `acc_calibration` object may contain missing (`NA`) elements (e.g. if
produced by
[`as_acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)).
`transform_imu()` returns `NA` in such cases and emits a warning if any
bursts are lost because of missing calibration specifications.

If an `acc_calibration` only has calibration parameters for certain axes
(e.g. `offset_x = 2048` and `slope_x = 0.001`), then only those axes
will be transformed by `transform_imu()`. Values for other axes will be
converted to `NA`. The dimension of the input burst matrices therefore
remains the same.

## See also

[`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
to construct an accelerometer calibration.

## Examples

``` r
a <- acc_example()

# Transform values using the standard Ornitela calibration formula
transform_imu(a, acc_calibration("ornitela"))
#> <acceleration[2]>
#> [1] (0.01 0 0.01) [m/s^2]  (0 -0.01 0.01) [m/s^2]
#> # frequency: 20 [Hz]

# Transform values using a set of custom acc calibrations.
# Calibrations will be mapped to the input IMU vector by index.
transform_imu(
  a,
  acc_calibration(offset = c(2048, 2046), slope = c(0.001, 0.002))
)
#> <acceleration[2]>
#> [1] (-20.08 -20.08 -20.07) [m/s^2] (-40.13 -40.14 -40.11) [m/s^2]
#> # frequency: 20 [Hz]
```
