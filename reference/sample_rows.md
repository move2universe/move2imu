# Identify rows in a `move2` that contain IMU data

These functions return a logical vector flagging the rows of an input
`move2` object that contain sample data for the specified sensor. These
are the rows that will be used to build IMU bursts when calling
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
or
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md).

## Usage

``` r
acc_sample_rows(x, colset = NULL)

mag_sample_rows(x, colset = NULL)

gyro_sample_rows(x, colset = NULL)
```

## Arguments

- x:

  A `move2` object.

- colset:

  An `imu_colset` object or list of `imu_colset` objects specifying the
  columns to check for IMU data. By default, all active colsets detected
  in `x` are considered (see
  [`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)).

## Value

A logical vector the same length as `nrow(x)`. `TRUE` values indicate
rows where IMU data is present under at least one active colset.

## Details

If `x` has data in more than one active IMU column set,
`*_sample_rows()` will return `TRUE`. However, these rows cannot be
parsed by
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
etc. as they contain duplicated IMU data. To ensure that
`*_sample_rows()` only considers certain column sets, use the `colset`
argument.

If no active colset is detected (e.g. a `move2` with only GPS data),
`*_sample_rows()` returns `FALSE` for all rows.

For expanded-format data (where multiple rows compose a single burst)
all rows that contain IMU data are flagged `TRUE`. However, the output
of `as_*()` will not necessarily return bursts at each of these
locations, as multiple of these rows will be combined into a single
burst.

## See also

[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
to extract IMU data from a `move2` object.

[`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_mag_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
to inspect which colsets are detected in `x`.

## Examples

``` r
alb <- albatrosses()

head(acc_sample_rows(alb))
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

# Filter to rows with acc data without building bursts
nrow(alb[acc_sample_rows(alb), ])
#> [1] 45
```
