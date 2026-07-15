# Summarize an IMU vector

Provides a diagnostic overview of an IMU vector (`acc`, `mag`, or
`gyro`).

Includes information about data time range, axes, sampling frequencies,
burst lengths, inter-burst intervals, and sample values.

## Usage

``` r
# S3 method for class 'imu'
summary(object, ...)
```

## Arguments

- object:

  An `imu` object.

- ...:

  Ignored.

## Value

An `imu_summary` object

## Details

The intervals shown are the gaps between consecutive bursts (end of one
to the start of the next), computed in vector order (see
[`burst_intervals()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)).
If bursts come from different sources (e.g. different tags), there may
be noticeable interval artifacts between some bursts where sources
change.

Note that the sample-value quantiles consider all axes and units
simultaneously.

## Examples

``` r
a <- acc_example()
summary(a)
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
#> Units:   NULL 
```
