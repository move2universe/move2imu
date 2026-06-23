# Calibrating acceleration bursts

Many accelerometer tags store their measurements as raw integers
produced by the analog-to-digital converter (ADC). By themselves, these
values have no physical units. To convert the raw ADC values to
interpretable acceleration units (e.g., m/s²), we need to define the
transformation that maps each ADC value to its corresponding value in
the target unit.

For acceleration, this is a simple linear transformation, requiring:

1.  An **offset**, which is the ADC value that corresponds to 0
    acceleration;
2.  A **slope**, which defines the output unit increment that
    corresponds to a one-unit ADC increment.

Further, different tag manufacturers and generations may define the axis
space differently. That is, one tag manufacturer may define the y-axis
to point in the opposite direction of another manufacturer. To account
for this, a transformation function also takes an **orientation**
indicator, which identifies the direction that each axis points. This
allows you to ensure that all tags being considered together use the
same axis orientation definitions.

For a set of acceleration bursts that all come from the same tag
manufacturer and tag generation, the transformation from ADC to physical
units is likely to be essentially identical. However, for a more complex
dataset with long-running deployments and/or multiple tag manufacturers,
each tag will need its own transformation.

move2imu facilitates this transformation process with two functions:

- [`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
  specifies the parameters for a set of transformation functions
- [`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
  applies that specification to an `acc` vector, yielding a vector with
  proper physical units.

We’ll work through this with the example
[`albatrosses()`](https://move2universe.github.io/move2imu/reference/example_data.md)
dataset, which contains raw data collected with e-obs tags.

``` r

library(move2imu)
library(move2)
library(dplyr)
```

First, we’ll load the data and extract the acceleration burst data. This
sample dataset is shipped with move2imu:

``` r

alb <- albatrosses()

alb <- alb |>
  mutate(acceleration = as_acc(alb))

head(alb$acceleration)
#> <acceleration[6]>
#> [1] <NA>              (1824.17 1913.83) (1904.3 1926.5)   (1823.27 1913.42)
#> [5] (1826.7 1915.7)   (1719.07 1908.8) 
#> # frequency: 5 [Hz]
```

From the summary, we can see that no units have yet been attached to the
values:

``` r

summary(alb$acceleration)
#> 54 acc bursts (9 NA)
#> from 2008-07-27 00:00:14 to 2008-07-27 01:00:00 UTC 
#> 
#> Axes: XY (45) 
#> Frequencies: 5 -- 5 [Hz] 
#> Samples per burst: 60 -- 60 
#> Durations: 12 -- 12 [s] 
#> Intervals: [ -3598 / 833 / 888 / 888 / 891 ] [s]  (min/Q1/med/Q3/max) 
#> 
#> Values:  [ 1462 / 1875 / 1936 / 2010 / 2988 ]  (min/Q1/med/Q3/max) 
#> Units:   [no units]
```

Note as well that the raw acc values are in the thousands, which would
be unreasonable were these values already in m/s² or *g*.

We can confirm with
[`imu_units()`](https://move2universe.github.io/move2imu/reference/imu-properties.md):

``` r

imu_units(alb$acceleration)
#>  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#> [26] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#> [51] NA NA NA NA
```

Before we can truly understand these data, we’ll need to convert them to
physical units.

## Tag manufacturer calibration

The easiest way to convert ADC values is with the tag manufacturer
default transformation settings.

move2imu currently has built-in support for two tag manufacturers:
[e-obs](https://e-obs.de/) and [Ornitela](https://www.ornitela.com/).

These sample data were collected with e-obs tags. The default
calibration suggested by e-obs differs by tag generation. Thus, to build
a calibration, we need to specify both the tag manufacturer and the tag
ID, which identifies the tag generation. In this case, imagine that
these data were collected with the latest generation of e-obs tags (tags
with IDs greater than 4117):

``` r

cal <- acc_calibration(manufacturer = "eobs", tag_id = 4266)
cal
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.00195]}
```

Then, we simply use
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
to apply this transformation to the albatross acceleration bursts:

``` r

alb$acc_cal <- transform_imu(alb$acceleration, calibration = cal)

head(alb$acc_cal)
#> <acceleration[6]>
#> [1] <NA>                  (-4.29 -2.57) [m/s^2] (-2.75 -2.33) [m/s^2]
#> [4] (-4.3 -2.58) [m/s^2]  (-4.24 -2.53) [m/s^2] (-6.3 -2.67) [m/s^2] 
#> # frequency: 5 [Hz]
```

Now that values have been transformed, computations on the acceleration
bursts will respect the attached units:

``` r

vedba(alb$acc_cal)
#> Units: [m/s^2]
#>  [1]         NA 0.42592544 0.63469795 0.46723033 0.42602852 0.52097709
#>  [7]         NA 0.72788691 1.06822539 0.81265780 0.45922480 0.51736803
#> [13] 1.60998841         NA 1.67764039 0.72574021 0.64973569 0.77873477
#> [19] 1.31999039         NA 0.90810530 1.71834229 0.96066700 1.39964558
#> [25] 0.40135076         NA 0.58057912 0.51380267 0.33742077 0.43802696
#> [31] 1.95526814         NA 0.28129788 0.08598663 0.08436676 0.08193444
#> [37] 0.08279857         NA 0.06927485 0.08091683 0.08286561 0.08829448
#> [43] 0.07738145         NA 0.70910821 0.15095153 0.08536001 0.19591598
#> [49] 1.42934267         NA 0.43037142 0.46979953 0.46734662 0.48806595
```

By default,
[`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
converts to m/s². If you instead want your data in *g*, you can convert
with
[`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md):

``` r

set_imu_units(alb$acc_cal, "standard_free_fall")
#> <acceleration[54]>
#>  [1] <NA>                               (-0.44 -0.26) [standard_free_fall]
#>  [3] (-0.28 -0.24) [standard_free_fall] (-0.44 -0.26) [standard_free_fall]
#>  [5] (-0.43 -0.26) [standard_free_fall] (-0.64 -0.27) [standard_free_fall]
#>  [7] <NA>                               (-0.2 -0.04) [standard_free_fall] 
#>  [9] (-0.23 -0.07) [standard_free_fall] (-0.24 -0.08) [standard_free_fall]
#> [11] (-0.21 -0.05) [standard_free_fall] (-0.21 -0.05) [standard_free_fall]
#> [13] (-0.15 -0.01) [standard_free_fall] <NA>                              
#> [15] (-0.17 -0.03) [standard_free_fall] (0.03 -0.05) [standard_free_fall] 
#> [17] (0.08 -0.04) [standard_free_fall]  (0.07 0.01) [standard_free_fall]  
#> [19] (-0.31 -0.24) [standard_free_fall] <NA>                              
#> [21] (-0.39 -0.2) [standard_free_fall]  (-0.33 -0.21) [standard_free_fall]
#> [23] (-0.4 0.09) [standard_free_fall]   (-0.53 0.16) [standard_free_fall] 
#> [25] (-0.46 -0.19) [standard_free_fall] <NA>                              
#> [27] (-0.5 -0.24) [standard_free_fall]  (-0.46 -0.24) [standard_free_fall]
#> [29] (-0.48 -0.22) [standard_free_fall] (-0.46 -0.22) [standard_free_fall]
#> [31] (-0.07 0.35) [standard_free_fall]  <NA>                              
#> [33] (-0.15 0.37) [standard_free_fall]  (0.16 0.07) [standard_free_fall]  
#> [35] (-0.1 0.05) [standard_free_fall]   (0 0.02) [standard_free_fall]     
#> [37] (-0.14 -0.35) [standard_free_fall] <NA>                              
#> [39] (-0.25 0.16) [standard_free_fall]  (-0.33 -0.25) [standard_free_fall]
#> [41] (-0.24 -0.21) [standard_free_fall] (-0.19 -0.17) [standard_free_fall]
#> [43] (-0.22 -0.25) [standard_free_fall] <NA>                              
#> [45] (-0.73 -0.36) [standard_free_fall] (-0.21 -0.25) [standard_free_fall]
#> [47] (0.05 -0.19) [standard_free_fall]  (-0.84 -0.2) [standard_free_fall] 
#> [49] (-0.39 -0.25) [standard_free_fall] <NA>                              
#> [51] (-0.41 -0.23) [standard_free_fall] (-0.4 -0.23) [standard_free_fall] 
#> [53] (-0.38 -0.23) [standard_free_fall] (-0.43 -0.23) [standard_free_fall]
#> # frequency: 5 [Hz]
```

Alternatively, we could have also specified the units in the calibration
itself:

``` r

acc_calibration(
  manufacturer = "eobs",
  tag_id = 4266,
  units = "standard_free_fall"
)
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.00195]}
```

Note the difference between these two approaches:
[`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md)
converts *between* compatible units (e.g., m/s² to *g*) or *assigns* new
units, whereas
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
*transforms* raw values to their physical counterpart.

Thus, if you have raw values in your data, you must first use
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
to convert the raw ADC values to meaningful units using the provided
calibration. Otherwise, you will end up simply assigning units to raw
values without changing the values themselves.

``` r

alb$acceleration[2]
#> <acceleration[1]>
#> [1] (1824.17 1913.83)
#> # frequency: 5 [Hz]

set_imu_units(alb$acceleration[2], "standard_free_fall")
#> <acceleration[1]>
#> [1] (1824.17 1913.83) [standard_free_fall]
#> # frequency: 5 [Hz]
```

## Vectorized calibration

The problem with the above approach is that it assumes that *all* tags
used in the study had ID `4266` (generation 3). If all the albatrosses
tagged in the study used generation 3 e-obs tags, then this wouldn’t be
a problem.

However, e-obs has revised the default calibration over time, so earlier
generations of tags don’t necessarily have the same transformation
function. (You can see the default calibration specifications for e-obs
tags with
[`eobs_default_specs()`](https://move2universe.github.io/move2imu/reference/eobs_default_specs.md).)

In these cases, you can build a vector of per-burst calibration
specifications, which when passed to
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
will be matched to bursts by index.

For instance, imagine that the e-obs tag IDs were stored in the original
`move2` object like so:

``` r

# Example IDs that span e-obs generations
fake_ids <- c(1000, 2000, 2500, 2501, 2502, 4200, 4201, 4202, 4203)

# Set example IDs in albatross track data
alb <- mt_set_track_data(
  alb,
  mutate(mt_track_data(alb), tag_id = fake_ids)
)

mt_track_data(alb)
#> # A tibble: 9 × 52
#>   deployment_id tag_id individual_id animal_life_stage attachment_type
#>         <int64>  <dbl>       <int64> <fct>             <fct>          
#> 1       9472222   1000       2911065 adult             tape           
#> 2       9472220   2000       2911067 adult             tape           
#> 3       9472218   2500       2911060 adult             tape           
#> 4       9472214   2501       2911066 adult             tape           
#> 5       9472208   2502       2911074 adult             tape           
#> 6       2911178   4200       2911094 adult             tape           
#> 7       2911168   4201       2911093 adult             tape           
#> 8       2911167   4202       2911092 adult             tape           
#> 9       2911150   4203       2911091 adult             tape           
#> # ℹ 47 more variables: deployment_comments <chr>, deploy_on_timestamp <dttm>,
#> #   duty_cycle <chr>, deployment_local_identifier <fct>,
#> #   manipulation_type <fct>, study_site <chr>, tag_readout_method <fct>,
#> #   sensor_type_ids <chr>, capture_location <POINT [°]>,
#> #   deploy_on_location <POINT [°]>, deploy_off_location <POINT [°]>,
#> #   individual_comments <chr>, individual_local_identifier <fct>,
#> #   taxon_canonical_name <fct>, individual_number_of_deployments <int>, …
```

We could use this track-level metadata to build a vectorized set of
calibration functions based on the tag ID:

``` r

cals <- acc_calibration("eobs", tag_id = mt_track_data(alb)$tag_id)

# Reattach calibrations to each track
alb <- mt_set_track_data(
  alb,
  mutate(mt_track_data(alb), cal = cals)
)

mt_track_data(alb)$cal
#> <acc_calibration[9]>
#> [1] {offset=[2048] slope=[0.00270] orientation=[1, -1, 1]}
#> [2] {offset=[2048] slope=[0.00270] orientation=[1, -1, 1]}
#> [3] {offset=[2048] slope=[0.00220]}                       
#> [4] {offset=[2048] slope=[0.00220]}                       
#> [5] {offset=[2048] slope=[0.00220]}                       
#> [6] {offset=[2048] slope=[0.00195]}                       
#> [7] {offset=[2048] slope=[0.00195]}                       
#> [8] {offset=[2048] slope=[0.00195]}                       
#> [9] {offset=[2048] slope=[0.00195]}
```

The only issue is that these calibrations are linked to each track, but
our acceleration bursts are stored as individual events in the *event*
data of our `move2` object.

To expand each calibration to its corresponding bursts (linking by track
ID), we can use move2’s
[`mt_as_event_attribute()`](https://bartk.gitlab.io/move2/reference/mt_as_track_attribute.html):

``` r

mt_as_event_attribute(alb, cal)$cal
#> <acc_calibration[54]>
#>  [1] {offset=[2048] slope=[0.00220]}                       
#>  [2] {offset=[2048] slope=[0.00220]}                       
#>  [3] {offset=[2048] slope=[0.00220]}                       
#>  [4] {offset=[2048] slope=[0.00220]}                       
#>  [5] {offset=[2048] slope=[0.00220]}                       
#>  [6] {offset=[2048] slope=[0.00220]}                       
#>  [7] {offset=[2048] slope=[0.00270] orientation=[1, -1, 1]}
#>  [8] {offset=[2048] slope=[0.00270] orientation=[1, -1, 1]}
#>  [9] {offset=[2048] slope=[0.00270] orientation=[1, -1, 1]}
#> ...
```

Passing this to
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
maps each burst to its corresponding calibration function by index:

``` r

transform_imu(alb$acceleration, mt_as_event_attribute(alb, cal)$cal)
#> <acceleration[54]>
#>  [1] <NA>                  (-4.83 -2.89) [m/s^2] (-3.1 -2.62) [m/s^2] 
#>  [4] (-4.85 -2.9) [m/s^2]  (-4.77 -2.85) [m/s^2] (-7.1 -3) [m/s^2]    
#>  [7] <NA>                  (-2.77 0.53) [m/s^2]  (-3.18 0.91) [m/s^2] 
#> [10] (-3.21 1.04) [m/s^2]  (-2.83 0.64) [m/s^2]  (-2.86 0.69) [m/s^2] 
#> [13] (-1.7 -0.08) [m/s^2]  <NA>                  (-1.84 -0.32) [m/s^2]
#> [16] (0.3 -0.53) [m/s^2]   (0.92 -0.48) [m/s^2]  (0.77 0.08) [m/s^2]  
#> [19] (-4.25 3.2) [m/s^2]   <NA>                  (-5.34 2.64) [m/s^2] 
#> [22] (-4.42 2.91) [m/s^2]  (-5.47 -1.2) [m/s^2]  (-7.12 -2.16) [m/s^2]
#> [25] (-5.09 -2.07) [m/s^2] <NA>                  (-5.56 -2.6) [m/s^2] 
#> [28] (-5.09 -2.69) [m/s^2] (-5.25 -2.44) [m/s^2] (-5.1 -2.38) [m/s^2] 
#> [31] (-0.68 3.43) [m/s^2]  <NA>                  (-1.48 3.65) [m/s^2] 
#> [34] (1.58 0.66) [m/s^2]   (-0.97 0.5) [m/s^2]   (0.03 0.24) [m/s^2]  
#> [37] (-1.38 -3.44) [m/s^2] <NA>                  (-2.45 1.54) [m/s^2] 
#> [40] (-3.23 -2.44) [m/s^2] (-2.31 -2.04) [m/s^2] (-1.89 -1.64) [m/s^2]
#> [43] (-2.13 -2.47) [m/s^2] <NA>                  (-7.19 -3.49) [m/s^2]
#> [46] (-2.03 -2.41) [m/s^2] (0.48 -1.84) [m/s^2]  (-8.24 -1.98) [m/s^2]
#> [49] (-3.87 -2.43) [m/s^2] <NA>                  (-4.01 -2.27) [m/s^2]
#> [52] (-3.92 -2.27) [m/s^2] (-3.75 -2.27) [m/s^2] (-4.18 -2.22) [m/s^2]
#> # frequency: 5 [Hz]
```

In general, expanding within the call to
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
is ideal, as the calibrations that are duplicated across bursts do not
need to persist in memory afterwards.

## Custom calibration

For tags that come from manufacturers without built-in move2imu support,
you can build your own calibration function by manually specifying the
offset and slope for the transformation function:

``` r

acc_calibration(offset = 2048, slope = 0.001)
#> <acc_calibration[1]>
#> [1] {offset=[2048] slope=[0.001]}
```

If the calibration differs by axis, you can specify per-axis parameters:

``` r

acc_calibration(offset_x = 2048, offset_y = 2040, slope = 0.001, orientation_y = -1)
#> <acc_calibration[1]>
#> [1] {offset=[2048, 2040, NA] slope=[0.001] orientation=[1, -1, 1]}
```

You can also specify individual arguments to override a manufacturer
default—for instance, if you have detailed information about some
calibration parameters but not others.

This can also be useful to standardize different tags that have defined
their axes in different directions. For instance, you may want to
redefine the y-axis orientation for Ornitela tags to match other tags
used in the study if those tags define the y-axis in the opposite
direction:

``` r

cal1 <- acc_calibration("ornitela")
cal2 <- acc_calibration("ornitela", orientation_y = -1)

transform_imu(alb$acceleration[2], cal1)
#> <acceleration[1]>
#> [1] (17.89 18.77) [m/s^2]
#> # frequency: 5 [Hz]

transform_imu(alb$acceleration[2], cal2)
#> <acceleration[1]>
#> [1] (17.89 -18.77) [m/s^2]
#> # frequency: 5 [Hz]
```

User-supplied values always take precedence over the manufacturer
defaults.

## External calibration specification

For studies with many tags, manually specifying detailed per-axis
offset, slope, and orientation values quickly becomes tedious. Instead,
these values can be generated once by a calibration procedure and saved
to an external file for later reuse.

move2imu supports the conversion of a `data.frame` that contains these
calibration parameters to an `acc_calibration` vector on a per-row basis
with
[`as_acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md).
Any columns in the `data.frame` that correspond to the available
arguments to
[`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
will be used to build calibration functions.

For instance, for the following set of calibration specifications:

``` r

cal_table <- read.csv("calibration.csv")
```

``` r

cal_table
#> # A tibble: 9 × 8
#>   tag_id offset_x offset_y offset_z slope_x slope_y slope_z orientation_y
#>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>         <dbl>
#> 1   1000     2049     2047     2050 0.00271 0.00269 0.0027             -1
#> 2   2000     2048     2050     2046 0.0027  0.00271 0.00269            -1
#> 3   2500     2050     2047     2052 0.00221 0.0022  0.00219             1
#> 4   2501     2047     2049     2048 0.0022  0.00219 0.0022              1
#> 5   2502     2049     2046     2051 0.00219 0.00221 0.0022              1
#> 6   4200     2048     2049     2047 0.00195 0.00196 0.00196             1
#> 7   4201     2050     2047     2049 0.00196 0.00195 0.00195             1
#> 8   4202     2046     2051     2048 0.00196 0.00196 0.00197             1
#> 9   4203     2049     2048     2050 0.00195 0.00197 0.00196             1
```

We can generate a vector of `acc_calibration` objects linked to their
corresponding `tag_id` like so:

``` r

cals_df <- cal_table |>
  mutate(cals = as_acc_calibration(cal_table)) |>
  select(tag_id, cals)

cals_df
#> # A tibble: 9 × 2
#>   tag_id cals     
#>    <dbl> <acc_cal>
#> 1   1000 <acc_cal>
#> 2   2000 <acc_cal>
#> 3   2500 <acc_cal>
#> 4   2501 <acc_cal>
#> 5   2502 <acc_cal>
#> 6   4200 <acc_cal>
#> 7   4201 <acc_cal>
#> 8   4202 <acc_cal>
#> 9   4203 <acc_cal>
```

Then, we can join these calibrations onto our `move2` track data,
matching by tag ID:

``` r

alb <- mt_set_track_data(
  alb,
  left_join(mt_track_data(alb), cals_df, by = "tag_id")
)
```

And transform by expanding our track-level calibrations to each of the
corresponding bursts in the event data:

``` r

transform_imu(alb$acceleration, mt_as_event_attribute(alb, cals)$cals)
#> <acceleration[54]>
#>  [1] <NA>                  (-4.89 -2.87) [m/s^2] (-3.16 -2.6) [m/s^2] 
#>  [4] (-4.91 -2.88) [m/s^2] (-4.84 -2.83) [m/s^2] (-7.17 -2.98) [m/s^2]
#>  [7] <NA>                  (-2.8 0.5) [m/s^2]    (-3.22 0.88) [m/s^2] 
#> [10] (-3.25 1.01) [m/s^2]  (-2.87 0.61) [m/s^2]  (-2.9 0.66) [m/s^2]  
#> [13] (-1.68 -0.1) [m/s^2]  <NA>                  (-1.81 -0.34) [m/s^2]
#> [16] (0.33 -0.55) [m/s^2]  (0.94 -0.5) [m/s^2]   (0.8 0.05) [m/s^2]   
#> [19] (-4.25 3.27) [m/s^2]  <NA>                  (-5.34 2.71) [m/s^2] 
#> [22] (-4.42 2.98) [m/s^2]  (-5.47 -1.15) [m/s^2] (-7.12 -2.11) [m/s^2]
#> [25] (-5.09 -2.03) [m/s^2] <NA>                  (-5.55 -2.57) [m/s^2]
#> [28] (-5.09 -2.66) [m/s^2] (-5.25 -2.4) [m/s^2]  (-5.1 -2.35) [m/s^2] 
#> [31] (-0.7 3.46) [m/s^2]   <NA>                  (-1.5 3.68) [m/s^2]  
#> [34] (1.56 0.67) [m/s^2]   (-0.99 0.51) [m/s^2]  (0.01 0.24) [m/s^2]  
#> [37] (-1.35 -3.51) [m/s^2] <NA>                  (-2.42 1.49) [m/s^2] 
#> [40] (-3.2 -2.51) [m/s^2]  (-2.28 -2.1) [m/s^2]  (-1.86 -1.7) [m/s^2] 
#> [43] (-2.17 -2.45) [m/s^2] <NA>                  (-7.26 -3.47) [m/s^2]
#> [46] (-2.07 -2.38) [m/s^2] (0.44 -1.81) [m/s^2]  (-8.3 -1.96) [m/s^2] 
#> [49] (-3.87 -2.46) [m/s^2] <NA>                  (-4 -2.29) [m/s^2]   
#> [52] (-3.91 -2.3) [m/s^2]  (-3.74 -2.29) [m/s^2] (-4.17 -2.24) [m/s^2]
#> # frequency: 5 [Hz]
```

Now, each burst has been transformed specifically with the detailed
calibration specifications for the tag that produced it.
