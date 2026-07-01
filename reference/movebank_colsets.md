# View standard Movebank IMU data column sets

Movebank has several standard ways to store data for each IMU sensor.
These functions show the recognized columns for each sensor that can be
extracted from a `move2` object by default.

- `movebank_acc_colsets()` — standard column sets for
  [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md).

- `movebank_mag_colsets()` — standard column sets for
  [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md).

- `movebank_gyro_colsets()` — standard column sets for
  [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md).

To extract IMU data from a `move2` with column names that don't
correspond to Movebank's conventions, provide a custom set of IMU
columns with
[`imu_colset()`](https://move2universe.github.io/move2imu/reference/imu_colset.md).

## Usage

``` r
movebank_acc_colsets()

movebank_mag_colsets()

movebank_gyro_colsets()
```

## Value

A named list of `imu_colset` objects.

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

### Alternate column name separators

Some column names may differ depending on how the data were downloaded.
The Movebank API (e.g.
[`move2::movebank_download_study()`](https://bartk.gitlab.io/move2/reference/movebank_download_study.html))
provides columns with `_` separators, while manually downloaded data
uses `:` and `-` separators and occasionally includes additional
prefixes. For full compatibility, the `active_*_colsets()` functions
recognize these alternate spellings as additional column sets even
though `movebank_*_colsets()` lists only the standard API names.

For future compatibility, consider converting data with the
manually-downloaded column names to use `_` separators. To use a custom
column set, provide the names explicitly with
[`imu_colset()`](https://move2universe.github.io/move2imu/reference/imu_colset.md).

## See also

[`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_mag_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md),
[`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
to identify column sets present in a given `move2` object.

## Examples

``` r
movebank_acc_colsets()
#> $eobs
#> <imu_colset> [
#>   bursts = "eobs_accelerations_raw",
#>   axes = "eobs_acceleration_axes",
#>   frequency = "eobs_acceleration_sampling_frequency_per_axis"
#> ]
#> 
#> $raw
#> <imu_colset> [
#>   bursts = "accelerations_raw",
#>   axes = "acceleration_axes",
#>   frequency = "acceleration_sampling_frequency_per_axis"
#> ]
#> 
#> $acc
#> <imu_colset> [
#>   bursts = "accelerations",
#>   axes = "acceleration_axes",
#>   frequency = "acceleration_sampling_frequency_per_axis"
#> ]
#> 
#> $xyz
#> <imu_colset> [
#>   X = "acceleration_x",
#>   Y = "acceleration_y",
#>   Z = "acceleration_z"
#> ]
#> 
#> $raw_xyz
#> <imu_colset> [
#>   X = "acceleration_raw_x",
#>   Y = "acceleration_raw_y",
#>   Z = "acceleration_raw_z"
#> ]
#> 

movebank_mag_colsets()
#> $raw
#> <imu_colset> [
#>   bursts = "magnetic_fields_raw",
#>   axes = "magnetic_field_axes",
#>   frequency = "magnetic_field_sampling_frequency_per_axis"
#> ]
#> 
#> $xyz
#> <imu_colset> [
#>   X = "magnetic_field_x",
#>   Y = "magnetic_field_y",
#>   Z = "magnetic_field_z"
#> ]
#> 
#> $raw_xyz
#> <imu_colset> [
#>   X = "magnetic_field_raw_x",
#>   Y = "magnetic_field_raw_y",
#>   Z = "magnetic_field_raw_z"
#> ]
#> 

movebank_gyro_colsets()
#> $raw
#> <imu_colset> [
#>   bursts = "angular_velocities_raw",
#>   axes = "gyroscope_axes",
#>   frequency = "gyroscope_sampling_frequency_per_axis"
#> ]
#> 
#> $xyz
#> <imu_colset> [
#>   X = "angular_velocity_x",
#>   Y = "angular_velocity_y",
#>   Z = "angular_velocity_z"
#> ]
#> 
```
