# Identify IMU columns present in a `move2` object

Determine the column sets that will be used by default when extracting
IMU data from a `move2` object. Column sets are processed independently,
but a single `move2` may contain multiple active column sets for one IMU
sensor.

- `active_acc_colsets()` — column sets used by
  [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md).

- `active_mag_colsets()` — column sets used by
  [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md).

- `active_gyro_colsets()` — column sets used by
  [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md).

If no active colsets are found, you can use
[`imu_colset()`](https://move2universe.github.io/move2imu/reference/imu_colset.md)
to specify a custom set of columns that contain IMU data.

## Usage

``` r
active_acc_colsets(x)

active_mag_colsets(x)

active_gyro_colsets(x)
```

## Arguments

- x:

  A `move2` object.

## Value

A list of `imu_colset` objects.

## Details

`move2` objects store IMU data in two ways:

- **Expanded-format** columns store each IMU sample (possibly for
  multiple axes) in its own row.

- **Compact-format** columns store a burst of IMU samples as a
  space-delimited string. This string must be segmented into
  axis-specific values using an associated column that indicates the
  axes present in the burst. A further column provides the sampling
  frequency of the burst. All three of these columns must be present to
  form a valid compact-format column set.

## See also

[`movebank_acc_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md),
[`movebank_mag_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md),
[`movebank_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
for the supported default colsets.

[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
to extract IMU data from a `move2` object.

## Examples

``` r
active_acc_colsets(albatrosses())
#> $eobs
#> <imu_colset> [
#>   bursts = "eobs_accelerations_raw",
#>   axes = "eobs_acceleration_axes",
#>   frequency = "eobs_acceleration_sampling_frequency_per_axis"
#> ]
#> 

# Multiple colsets may be available
active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
#> $eobs
#> <imu_colset> [
#>   bursts = "eobs_accelerations_raw",
#>   axes = "eobs_acceleration_axes",
#>   frequency = "eobs_acceleration_sampling_frequency_per_axis"
#> ]
#> 
#> $raw_xyz
#> <imu_colset> [
#>   X = "acceleration_raw_x",
#>   Y = "acceleration_raw_y",
#>   Z = "acceleration_raw_z"
#> ]
#> 

# Missing expanded-format axes are not included in the set
g <- gulls()
g$acceleration_raw_x <- NULL
active_acc_colsets(g)
#> $raw_xyz
#> <imu_colset> [
#>   Y = "acceleration_raw_y",
#>   Z = "acceleration_raw_z"
#> ]
#> 

# Columns with no data are also removed
g$acceleration_raw_y <- NA
active_acc_colsets(g)
#> $raw_xyz
#> <imu_colset> [
#>   Z = "acceleration_raw_z"
#> ]
#> 

# Some column sets must be present in their entirety
alb <- albatrosses()
alb$eobs_acceleration_axes <- NULL

if (FALSE) { # \dontrun{
active_acc_colsets(alb)
} # }
```
