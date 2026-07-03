# Convert an object to a `gyro` vector

Extract `gyro` data from a `move2` or convert an object to a `gyro`
vector.

For a `move2`, `gyro` data are extracted from the object's
[`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md).

## Usage

``` r
as_gyro(x, ...)

# Default S3 method
as_gyro(x, ...)

# S3 method for class 'move2'
as_gyro(
  x,
  colset = NULL,
  min_freq = 0,
  tolerance = 1e-06,
  merge_continuous = TRUE,
  drop = FALSE,
  ...
)
```

## Arguments

- x:

  A `move2` containing gyroscope data. Most of the time this will be
  either loaded from disk using
  [move2::mt_read](https://bartk.gitlab.io/move2/reference/mt_read.html)
  or downloaded using
  [move2::movebank_download_study](https://bartk.gitlab.io/move2/reference/movebank_download_study.html).

- ...:

  currently not used

- colset:

  An `imu_colset` object or list of `imu_colset` objects specifying the
  columns of `x` that contain gyroscope data. By default, constructs
  bursts for all column sets that are detected in `x` that also contain
  data (see
  [`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)).

  Several common colsets are listed under
  [`movebank_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md).
  To specify a custom set of columns, use
  [`imu_colset()`](https://move2universe.github.io/move2imu/reference/imu_colset.md).

- min_freq:

  Numeric value indicating the minimum sampling rate to use when
  combining samples into a single burst. Samples recorded at a rate
  slower than this value will instead be split into individual
  (length-1) "bursts". Increase this value to avoid producing
  slow-frequency bursts. By default, all samples recorded at consistent
  intervals will be combined into bursts, regardless of their sampling
  rate.

  Ignored for compact-format data, where values are already in
  predefined bursts.

- tolerance:

  Tolerance (in seconds) to use when identifying timestamp
  irregularities that should be treated as noise when constructing
  bursts. This is the largest amount by which a sample's timestamp may
  deviate from the value suggested by the adjacent samples, assuming
  samples are collected at a consistent rate. For example, for 1 Hz data
  with a tolerance of 0.001, a timestamp recorded 1.001 seconds after
  another would still be considered to belong to the same burst.

  Increase this value to avoid splitting samples into separate IMU
  bursts because of small timestamp irregularities. See details.

- merge_continuous:

  Logical value indicating whether to merge adjacent bursts. Two
  adjacent bursts can be merged if the end of the first burst coincides
  with the start of the second burst (within `tolerance`) and the burst
  frequency is consistent between the two. This is useful for processing
  continuous data that have been stored in chunks split at regular
  intervals (e.g. e-obs data).

- drop:

  Logical indicating whether empty bursts should be dropped from the
  output. If `drop = FALSE`, then the length of the output will match
  the number of rows in the input data `x` and bursts will be stored at
  the index location corresponding to the start time of the burst.

## Details

The resulting vector will be as long as the input. This means it can,
for example, be added as a column to a `data.frame`. For some tags this
means `NA` values are inserted when one burst is stored over multiple
rows of a `data.frame`.

## See also

[`movebank_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
for supported gyroscope column sets in Movebank.
