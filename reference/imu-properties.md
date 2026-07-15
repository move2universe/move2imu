# Burst properties of an IMU vector

These functions describe characteristics of the bursts in an IMU vector.

- `n_axis()` — number of axes (columns) in each burst

- `n_samples()` — number of samples (rows) in each burst.

- `burst_dur()` — duration of each burst, in seconds.

- `burst_intervals()` — interval between each burst and its preceding
  burst, in seconds.

- `imu_units()` — units for each burst's data values

- `is_uniform()` — logical indicating whether every burst in a vector
  shares a consistent structure (axes, frequency, sample count, and
  units)

## Usage

``` r
n_axis(x)

n_samples(x)

burst_dur(x)

burst_intervals(x, ids = NULL, from = "end")

imu_units(x)

is_uniform(x)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- ids:

  For `burst_intervals()`, an optional sorted vector the same length as
  `x` giving the group (e.g. animal ID) of each burst. Intervals are not
  measured across changes in `ids`.

- from:

  For `burst_intervals()`, where to measure each interval from: `"end"`
  (default) gives the gap between the end of the previous burst and the
  start of the current one, while `"start"` gives the time between
  consecutive burst starts.

## Value

`is_uniform()` returns a length-1 logical. All others return a vector of
`length(x)`

## Details

`burst_intervals()` measures intervals between consecutive bursts in
vector order. Missing (`NA`) bursts are ignored when calculating
intervals. Thus, element `i` is the interval in between the most recent
preceding non-NA burst and burst `i`.

Only bursts flagged with [`is.na()`](https://rdrr.io/r/base/NA.html) are
considered missing. Bursts with data but lacking a start time are
retained but will produce `NA` intervals. Bursts with data but lacking a
frequency are also retained and will produce `NA` intervals when
`from = "end"`, as the frequency is required to determine the burst end
time.

Pass `ids` to measure intervals within groups (e.g. per animal).
Intervals are not measured across group boundaries. Intervals are taken
in vector order, so a vector mixing sources should be ordered by group.

## Examples

``` r
x <- acc(
  bursts = list(
    cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
    cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
  ),
  frequency = units::as_units(c(20, 30), "Hz"),
  start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 60)
)

# Number of axes for which data was collected
n_axis(x)
#> [1] 3 2

# Number of samples in the burst
n_samples(x)
#> [1] 30 20

# Time duration of the burst
burst_dur(x)
#> Units: [s]
#> [1] 1.5000000 0.6666667

# Gap from the end of each burst to the start of the next
burst_intervals(x)
#> Units: [s]
#> [1]   NA 58.5

# Or measure between consecutive burst starts
burst_intervals(x, from = "start")
#> Units: [s]
#> [1] NA 60

# The interval value shows the interval to the preceding present burst,
# ignoring intervening NA bursts.
x_na <- c(
  x[1], 
  acc(list(NULL), frequency = units::set_units(NA, "Hz")),
  x[2]
)

burst_intervals(x_na)
#> Units: [s]
#> [1]   NA   NA 58.5

# Units for the burst data
imu_units(x)
#> [1] NA NA

# Check if all bursts have uniform structure
is_uniform(x)
#> [1] FALSE
```
