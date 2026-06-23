# Summarize and plot an IMU vector

Provides a diagnostic overview of an IMU vector (`acc`, `mag`, or
`gyro`) — axis combinations, frequencies, burst sizes, timing, and a
coarse quantile summary of the burst sample values. Calling
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
draws a multi-panel histogram of those same distributions.

Intervals are the gaps between consecutive bursts (end of one to the
start of the next), computed in vector order (see
[`burst_intervals()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)).
If bursts come from different sources (e.g. different tags), there may
be noticeable interval artifacts between some bursts where sources
change.

Note that the distribution of sample values considers all axes and units
simultaneously.

## Usage

``` r
# S3 method for class 'imu'
summary(object, ...)

# S3 method for class 'imu_summary'
plot(x, panel = NULL, ...)
```

## Arguments

- object:

  An `imu` object.

- ...:

  For [`plot()`](https://rdrr.io/r/graphics/plot.default.html), passed
  to [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html).

- x:

  An `imu_summary` object (returned by
  [`summary()`](https://rdrr.io/r/base/summary.html)).

- panel:

  Optional character vector of panel names or integer vector of panel
  positions, restricting which panels are drawn. Valid names:
  `"Frequency"`, `"Samples per burst"`, `"Duration"`, `"Intervals"`,
  and/or `"Values"`. By default, all panels are drawn.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an
`imu_summary` object.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) invisibly
returns its input.

## Examples

``` r
a <- acc_example()
s <- summary(a)
s
#> 2 acc bursts
#> from 1970-01-01 to 1970-01-01 00:00:10 UTC 
#> 
#> Axes: XYZ (2) 
#> Frequencies: 20 -- 20 [Hz] 
#> Samples per burst: 20 -- 30 
#> Durations: 1 -- 1.5 [s] 
#> Intervals: [ 8.5 / 8.5 / 8.5 / 8.5 / 8.5 ] [s]  (min/Q1/med/Q3/max) 
#> 
#> Values:  [ -1 / -0.1 / 0.73 / 1 / 1 ]  (min/Q1/med/Q3/max) 
#> Units:   [no units] 
plot(s)


# Focus on a single panel, with custom binning/xlim
plot(s, panel = "Values", breaks = 50, xlim = c(0, 1))
```
