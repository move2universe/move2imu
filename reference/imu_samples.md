# Flatten an IMU vector into a data frame of samples

Convert an IMU vector to a data frame with one row for each sample in
each burst. Each row records which burst the sample came from, when it
was recorded, and its value on each axis.

Most analyses can be done on IMU vectors directly. This function is
intended primarily for methods that are hard to apply to IMU vectors,
such as those that require individual sample timestamps and values.

## Usage

``` r
imu_samples(x)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`).

## Value

A data frame with one row per sample and columns:

- `burst`: The position of the sample's burst in `x`.

- `time`: The time the sample was recorded.

- `frequency`: The sampling frequency of the sample's burst.

- `X`, `Y`, `Z`: The sample's value on each axis recorded in `x`.

## Details

The time of each sample is calculated from its burst's start time and
sampling frequency. Samples from bursts without a start time or
frequency are recorded with a `NA` time.

The `burst` column shows the index position in `x` for the burst that
the sample belongs to. Missing bursts contribute no rows to the output
but are included in the index count. If `x` is a column in a
`data.frame` or `move2` object, this means that the value of `burst` can
be used to look up attributes in the corresponding rows of that data
source by index, for instance to attach burst-level metadata (e.g.,
track ID) to the sample data frame (see examples). Attributes that can
vary within a burst should not be matched this way, as the value of that
attribute at the start of the burst will be used for the entirety of the
burst in the sample output. In those cases, consider joining on time
instead.

Axis values keep the units of `x`. All bursts must have the same units,
or all must be unitless. Use
[`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md)
to convert bursts to a single unit, or
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
to calibrate raw (unitless) values, before calling `imu_samples()`.

Axes that are not present in certain bursts are filled with `NA` values
for that burst's rows.

## Examples

``` r
a <- set_imu_units(acc_example(), "standard_free_fall")

samples <- imu_samples(a)

# (Times are recorded to sub-second precision despite printing to the second)
head(samples)
#>   burst                time frequency                               X
#> 1     1 1970-01-01 00:00:00   20 [Hz] 0.09983342 [standard_free_fall]
#> 2     1 1970-01-01 00:00:00   20 [Hz] 0.19866933 [standard_free_fall]
#> 3     1 1970-01-01 00:00:00   20 [Hz] 0.29552021 [standard_free_fall]
#> 4     1 1970-01-01 00:00:00   20 [Hz] 0.38941834 [standard_free_fall]
#> 5     1 1970-01-01 00:00:00   20 [Hz] 0.47942554 [standard_free_fall]
#> 6     1 1970-01-01 00:00:00   20 [Hz] 0.56464247 [standard_free_fall]
#>                                Y                      Z
#> 1 0.9950042 [standard_free_fall] 1 [standard_free_fall]
#> 2 0.9800666 [standard_free_fall] 1 [standard_free_fall]
#> 3 0.9553365 [standard_free_fall] 1 [standard_free_fall]
#> 4 0.9210610 [standard_free_fall] 1 [standard_free_fall]
#> 5 0.8775826 [standard_free_fall] 1 [standard_free_fall]
#> 6 0.8253356 [standard_free_fall] 1 [standard_free_fall]

# Store acc bursts in a column of the input data
alb <- albatrosses()
alb$acc <- as_acc(alb)

samples <- imu_samples(alb$acc)

# Use the `burst` ID column to look up attributes from the input. `burst`
# is the row of `alb` containing the burst that each sample belongs to.
samples$track <- move2::mt_track_id(alb)[samples$burst]

head(samples)
#>   burst                time frequency    X    Y         track
#> 1     2 2008-07-27 00:00:56    5 [Hz] 1856 1900 4266-84831108
#> 2     2 2008-07-27 00:00:56    5 [Hz] 1816 1931 4266-84831108
#> 3     2 2008-07-27 00:00:56    5 [Hz] 1812 1902 4266-84831108
#> 4     2 2008-07-27 00:00:56    5 [Hz] 1826 1920 4266-84831108
#> 5     2 2008-07-27 00:00:56    5 [Hz] 1818 1928 4266-84831108
#> 6     2 2008-07-27 00:00:57    5 [Hz] 1835 1922 4266-84831108
```
