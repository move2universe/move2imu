# Compute per-burst dynamic body acceleration

Calculate vectorial dynamic body acceleration (VeDBA) or overall dynamic
body acceleration (ODBA) for each burst in an `acc` vector. Dynamic body
acceleration (DBA) is a widely used proxy for movement-related activity.

`vedba()` and `odba()` extract the dynamic component of the input
acceleration values (see details) and summarize these samples across
axes as follows:

- VeDBA: \\\sqrt{x^2 + y^2 + z^2}\\

- ODBA: \\\|x\| + \|y\| + \|z\|\\

These values are then averaged to give a single value per burst.

These functions are intended to provide burst-level DBA summaries. For
analyses that need per-sample dynamic and/or static acceleration values
(e.g., to compute activity metrics at intervals across a continuous
stream of data), use
[`dynamic_acc()`](https://move2universe.github.io/move2imu/reference/static_acc.md)
and
[`static_acc()`](https://move2universe.github.io/move2imu/reference/static_acc.md)
instead.

## Usage

``` r
vedba(x, smoothing_window)

odba(x, smoothing_window)
```

## Arguments

- x:

  An `acc` vector.

- smoothing_window:

  Length of the running mean used to estimate the static acceleration
  component. Either a
  [`units`](https://r-quantities.github.io/units/reference/units.html)
  object convertible to seconds, a
  [`difftime`](https://rdrr.io/r/base/difftime.html) object, or
  `"burst"` to use the mean of the entire burst as the static component.
  Bare numeric values are assumed to be in seconds. See details.

  This only affects how the static component is estimated; it does not
  split bursts into shorter intervals.

## Value

A vector of VeDBA or ODBA values with the same length and units as `x`.
Mixed units are converted to the units of the first non-missing burst in
`x`.

## Details

### Static and dynamic acceleration

Acceleration recorded on each axis is treated as the sum of a *static*
component (the share of gravity on that axis, which reflects the
orientation of the tag and thus the animal's posture) and a *dynamic*
component (acceleration typically caused by the animal's own movement).
For each sample, the static component on each axis is estimated as the
mean of the surrounding samples within a centered window of length
`smoothing_window` (or of the whole burst, if
`smoothing_window = "burst"`). Subtracting it from the recorded values
leaves the dynamic component for each sample.

### Choosing a window

The running mean separates the static from the dynamic component by
averaging over complete movement cycles (e.g. wingbeats or strides):
within a full cycle, the dynamic component averages to approximately
zero, leaving the slowly varying static component. The window therefore
defines what counts as "movement" (anything faster than the window) and
what counts as "posture" (anything slower).

- If the window is shorter than a movement cycle, part of the movement
  is treated as posture and DBA is **underestimated**.

- If the window is much longer than the timescale of posture changes,
  those changes are treated as movement and DBA is **overestimated**.

Shepard et al. (2008) recommend a window of at least 3 seconds for
animals with a stroke period up to 3 seconds, and, for slower strokes,
at least one (and preferably two) stroke cycles. See Shepard et al.
(2008) for more details and
[`vignette("metrics", package = "move2imu")`](https://move2universe.github.io/move2imu/articles/metrics.md)
for a worked example.

`smoothing_window = "burst"` uses the mean of each whole burst as the
static component, so the effective window is each burst's duration. This
can be useful for short bursts, where a running mean would leave few or
no samples, but is unlikely to be useful for long recordings of
continuously-sampled data.

Note that this means that bursts that differ in duration will have DBA
calculated with different window sizes, impacting comparability.
Further, bursts less than a second or two may be too short to reliably
separate posture from movement. This method can also count posture
changes as movement for long bursts, in which case a numeric window
should be provided.

### Applying the smoothing window

`smoothing_window` is converted to the nearest odd number of samples (so
that it can be centered on each sample) using each burst's sampling
frequency. An even number of samples is rounded up. Samples within half
a window of either end of a burst have no static estimate. Likewise, a
missing sample leaves the static component undefined for every sample
whose window includes it.

The components cannot be calculated (and return missing values) for
bursts that are shorter than `smoothing_window`, for windows of fewer
than 3 samples, and for bursts with fewer than 2 samples when
`smoothing_window = "burst"`.

## References

Shepard, E. L. C., Wilson, R. P., Halsey, L. G., Quintana, F., Gómez
Laich, A., Gleiss, A. C., Liebsch, N., Myers, A. E., & Norman, B.
(2008). Derivation of body motion via appropriate smoothing of
acceleration data. *Aquatic Biology*, 4, 235–241.
[doi:10.3354/ab00104](https://doi.org/10.3354/ab00104)

Wilson, R. P., Börger, L., Holton, M. D., Scantlebury, D. M.,
Gómez-Laich, A., Quintana, F., et al. (2020). Estimates for energy
expenditure in free-living animals using acceleration proxies: A
reappraisal. *Journal of Animal Ecology*, 89, 161–172.
[doi:10.1111/1365-2656.13040](https://doi.org/10.1111/1365-2656.13040)

## Examples

``` r
a <- as_acc(albatrosses())

a <- transform_imu(
  a,
  acc_calibration("eobs", 1000, units = "standard_free_fall")
)

# Static component estimated with a 3 second running mean
vedba(a, smoothing_window = units::set_units(3, "s"))
#> Units: [standard_free_fall]
#>  [1]          NA 0.068884904 0.072239477 0.064800946 0.058539038 0.072790297
#>  [7]          NA 0.073737447 0.132444286 0.102806719 0.068233055 0.060682279
#> [13] 0.236549955          NA 0.236350722 0.114085328 0.089494390 0.090803532
#> [19] 0.175975707          NA 0.083250335 0.222811565 0.040281270 0.138025808
#> [25] 0.053777846          NA 0.072020377 0.078039083 0.045069801 0.059042416
#> [31] 0.342381587          NA 0.034573195 0.011673462 0.009311533 0.010207175
#> [37] 0.011146326          NA 0.010442567 0.010344942 0.011322390 0.012764422
#> [43] 0.010587842          NA 0.033883944 0.012618563 0.010646915 0.018909796
#> [49] 0.210175793          NA 0.065469180 0.068091649 0.063682861 0.068620226

# Bare numbers are interpreted as seconds
odba(a, smoothing_window = 3)
#> Units: [standard_free_fall]
#>  [1]         NA 0.08569957 0.08908826 0.08379783 0.07602652 0.09255522
#>  [7]         NA 0.09552913 0.16497783 0.12600391 0.08605565 0.07663696
#> [13] 0.30164870         NA 0.29381087 0.15154043 0.11318478 0.11208913
#> [19] 0.21622304         NA 0.10492435 0.26833304 0.04968391 0.16658217
#> [25] 0.07013739         NA 0.09169435 0.09870261 0.05749043 0.07137783
#> [31] 0.45691826         NA 0.04424087 0.01499087 0.01163739 0.01301087
#> [37] 0.01414957         NA 0.01225565 0.01239652 0.01423957 0.01615696
#> [43] 0.01289348         NA 0.03944348 0.01613739 0.01348043 0.02341174
#> [49] 0.26081217         NA 0.08048739 0.08537870 0.08265522 0.08865391

# Burst mean is used as the static acceleration for each burst
vedba(a, smoothing_window = "burst")
#> Units: [standard_free_fall]
#>  [1]          NA 0.060040823 0.089470558 0.065863390 0.060055352 0.073439832
#>  [7]          NA 0.102606993 0.150583000 0.114556770 0.064734885 0.072931079
#> [13] 0.226952932          NA 0.236489533 0.102304382 0.091590361 0.109774790
#> [19] 0.186073196          NA 0.128011581 0.242227100 0.135420971 0.197301836
#> [25] 0.056576638          NA 0.081841667 0.072428486 0.047564711 0.061746720
#> [31] 0.275625486          NA 0.039653316 0.012121155 0.011892808 0.011549936
#> [37] 0.011671748          NA 0.009765369 0.011406487 0.011681197 0.012446482
#> [43] 0.010908120          NA 0.099959842 0.021278968 0.012032822 0.027617407
#> [49] 0.201488104          NA 0.060667552 0.066225558 0.065879783 0.068800494
```
