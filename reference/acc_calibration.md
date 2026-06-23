# Create calibrations for raw acceleration values

Generate an `acc_calibration` object holding the per-burst calibration
parameters to be applied by
[`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md).

- Use `acc_calibration()` to specify calibration parameters manually.
  Arguments are vectorized and matched by index.

- Use `as_acc_calibration()` to convert a data.frame containing row-wise
  burst calibration parameters to an `acc_calibration` object.

This allows you to provide burst-specific calibration parameters to
flexibly convert raw acceleration values to physical units in `acc`
vectors that contain data from heterogeneous sources.

## Usage

``` r
acc_calibration(
  manufacturer = NULL,
  tag_id = NULL,
  sensitivity = NULL,
  offset = NULL,
  offset_x = offset,
  offset_y = offset,
  offset_z = offset,
  slope = NULL,
  slope_x = slope,
  slope_y = slope,
  slope_z = slope,
  orientation = NULL,
  orientation_x = orientation,
  orientation_y = orientation,
  orientation_z = orientation,
  units = "m/s^2"
)

as_acc_calibration(df)
```

## Arguments

- manufacturer:

  Manufacturer of the tag. Currently, `"eobs"` and `"ornitela"` are
  supported. For other manufacturers, leave `NULL` and manually specify
  the calibration parameters below.

- tag_id:

  If `manufacturer = "eobs"`, the e-obs tag ID for the tag. See details.

- sensitivity:

  If `manufacturer = "eobs"`, the sensitivity of the tag. Defaults to
  `"low"` if none provided. See details.

- offset, offset_x, offset_y, offset_z:

  Custom offset to use when calibrating. To specify axis-specific
  offsets, use `offset_x`, `offset_y`, and/or `offset_z`.

  Required if no `manufacturer` is specified.

- slope, slope_x, slope_y, slope_z:

  Custom slope to use when calibrating. To specify axis-specific slope,
  use `slope_x`, `slope_y`, and/or `slope_z`.

  Required if no `manufacturer` is specified.

- orientation, orientation_x, orientation_y, orientation_z:

  Either `1` or `-1` indicating the orientation of the tag's axes. To
  specify axis-specific orientations, use `orientation_x`,
  `orientation_y`, and/or `orientation_z`. Defaults to `1`.

  This is useful to standardize orientations across tags of different
  manufacturers or generations.

- units:

  Output units. Either `"m/s^2"` (default) or `"standard_free_fall"`.

- df:

  data.frame containing columns with names corresponding to the
  available arguments in `acc_calibration()`. Each row produces a single
  calibration.

## Value

An `acc_calibration` vector.

## Details

An `acc_calibration` can either be built from a `manufacturer` and
`tag_id` combination or from manual inputs of the `offset` and `slope`
parameters. If neither of these options is provided in full, then a
calibration cannot be built and `NA` is returned for that element.
Passing missing calibrations to
[`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md)
returns `NA` for that burst.

Currently if `manufacturer` is provided, it must be either `"ornitela"`
or `"eobs"`. If `"eobs"`, then a corresponding `tag_id` must also be
provided.

This is because e-obs tags have default calibration parameters that vary
depending on the tag's generation. Use
[`eobs_default_specs()`](https://robe2037.github.io/move2imu/reference/eobs_default_specs.md)
for a summary table showing the default offset, slope, and orientation
parameters used for each e-obs tag ID. The tag ID defines the tag
generation. Note that tags from generation 1 could be set either to low
or high sensitivity, each with their own default calibration parameters.

If no manufacturer is provided, then both `offset_*` and `slope_*` must
be provided for at least one axis.

If calibration parameters are provided for some axes and not others
(e.g. `offset_x = 2048` and `slope_x = 0.001`), then only those axes
will be transformed by
[`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md).
Values for other axes will be converted to `NA`.

If both `manufacturer` and a custom `offset` or `slope`, and/or
`orientation` are provided, then the value of the custom parameters will
override the manufacturer defaults for that calibration entry.

## See also

[`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md)
to apply a calibration to the entries in an `acc` vector.

## Examples

``` r
# Calibration for ornitela tags:
acc_calibration(manufacturer = "ornitela")
#> <acc_calibration[1]>
#> [1] {offset=[0] slope=[0.001]}

# E-obs tag defaults vary by tag_id and sensitivity (default `"low"`)
acc_calibration(manufacturer = "eobs", tag_id = 1000, sensitivity = "high")
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.001] orientation=[1, -1, 1]}
acc_calibration(manufacturer = "eobs", tag_id = 4000)
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.0022]}

# Provide vector arguments to generate element-wise calibrations:
acc_calibration(
  manufacturer = c("eobs", "ornitela"),
  tag_id = c(1000, NA)
)
#> <acc_calibration[2]>
#> [1] {offset=[2048] slope=[0.0027] orientation=[1, -1, 1]}
#> [2] {offset=[0] slope=[0.0010]}                          

# Calibration with explicit offset and slope
acc_calibration(offset = 2048, slope = 1 / 512)
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.00195]}

# Calibrate specific axes with axis-specific args:
cal <- acc_calibration(
  offset_x = 2048, 
  offset_y = 2046,
  offset_z = 2048,
  slope = 1 / 512, 
  orientation_y = -1 # Flip y axis orientation
)

# Apply calibration with transform_imu()
transform_imu(acc_example(), cal)
#> <acceleration[2]>
#> [1] (-39.21 39.19 -39.21) [m/s^2] (-39.23 39.2 -39.21) [m/s^2] 
#> # frequency: 20 [Hz]

# Convert a data.frame of calibration specs into a calibration vector
# (Useful for instance if specifications are stored as metadata alongside
# acc data in a move2)
cal <- as_acc_calibration(
  data.frame(manufacturer = "eobs", tag_id = c(1000, 4000))
)
```
