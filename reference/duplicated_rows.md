# Identify rows of a `move2` object with multiple sources of IMU data

Return a logical vector flagging rows of a `move2` object where more
than one column set for a given sensor contains data. Functions that
extract IMU data will error if a single timestamp contains multiple
sources of IMU data for the same sensor.

To resolve duplicated rows, pass a specific set of IMU columns to the
`colset` argument of `as_*()` or remove the duplicated data.

- `duplicated_acc_rows()` — checks acceleration column sets used by
  [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md).

- `duplicated_mag_rows()` — checks magnetometer column sets used by
  [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md).

- `duplicated_gyro_rows()` — checks gyroscope column sets used by
  [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md).

## Usage

``` r
duplicated_acc_rows(x, colsets = NULL)

duplicated_mag_rows(x, colsets = NULL)

duplicated_gyro_rows(x, colsets = NULL)
```

## Arguments

- x:

  A `move2` object.

- colsets:

  A list of `imu_colset` objects to check for overlap. Defaults to the
  column sets detected by the corresponding `active_*_colsets()`.

## Value

A logical vector of length `nrow(x)` with `TRUE` values indicating rows
that contain multiple sources of IMU data across the indicated column
sets.

## See also

[`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_mag_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
to identify available column sets in a `move2` object.

[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
to extract IMU data from a `move2` object.
