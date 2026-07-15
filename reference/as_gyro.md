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
  freq_tol = 0.01,
  gap_tol = 1e-06,
  merge_continuous = TRUE,
  drop = FALSE,
  ...
)
```

## Arguments

- x:

  A `move2` object containing gyroscope data. Typically this will be
  loaded from disk with
  [`move2::mt_read()`](https://bartk.gitlab.io/move2/reference/mt_read.html)
  or downloaded using
  [`move2::movebank_download_study()`](https://bartk.gitlab.io/move2/reference/movebank_download_study.html).

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

  For expanded-format data, the minimum allowable burst frequency in the
  output. Any burst whose derived frequency falls below this value is
  instead split into individual (length-1) bursts. Increase this value
  to avoid producing slow-frequency bursts. By default, all samples
  recorded at consistent intervals will be combined into bursts,
  regardless of their sampling frequency.

  Inputs with compatible
  [units](https://r-quantities.github.io/units/reference/units.html) are
  converted to Hz internally. If no units are specified, `min_freq` is
  assumed to be in Hz.

- freq_tol:

  Relative tolerance to use when detecting differences in sampling
  frequency when building or merging bursts. This determines how much
  two sampling frequencies may differ before they're treated as
  belonging to separate sampling regimes. Two frequencies belong to the
  same burst when the faster is at most `(1 + freq_tol)` times the
  slower. For example, `freq_tol = 0.01` keeps frequencies that are
  within 1% of each other in the same burst.

  Increase this value to prevent small deviations in sample timing from
  initiating the creation of new bursts. See details.

- gap_tol:

  Absolute tolerance (in seconds) to use when determining whether two
  bursts are adjacent in time and can be merged. Two bursts are adjacent
  when the gap between the first burst's end and the second burst's
  start is within `gap_tol`.

  For example, setting `gap_tol = 0.02` would allow a burst that starts
  up to 0.02 seconds after the end of the previous burst to be merged.
  See details.

- merge_continuous:

  Logical value indicating whether to merge adjacent bursts. Two
  adjacent bursts can be merged if the end of the first burst coincides
  with the start of the second burst (within `gap_tol`) and their
  frequencies agree (within `freq_tol`). This is useful for processing
  continuous data that have been stored in chunks split at regular
  intervals (e.g. e-obs data). See
  [`merge_imu()`](https://move2universe.github.io/move2imu/reference/merge_imu.md).

- drop:

  Logical indicating whether empty bursts should be dropped from the
  output. If `drop = FALSE`, then the length of the output will match
  the number of rows in the input data `x` and bursts will be stored at
  the index location corresponding to the start time of the burst.

## Value

An object of class `gyro` inheriting from class `imu`.

## Details

By default (`drop = FALSE`), the output vector will be the same length
as the input. This facilitates the use of an IMU burst vector as a
column in a data.frame. For expanded data formats, multiple rows of
input data will be represented in a single row in the output
(corresponding to the start timestamp of the burst).

### Input requirements

`as_*()` functions require that the input `move2` object be sorted by
track and strictly increasing in time. Duplicate timestamps within a
single track must be resolved before calling `as_*()`. See
[`move2::mt_is_track_id_cleaved()`](https://bartk.gitlab.io/move2/reference/assertions.html),
[`move2::mt_is_time_ordered()`](https://bartk.gitlab.io/move2/reference/assertions.html),
and
[`move2::mt_filter_unique()`](https://bartk.gitlab.io/move2/reference/mt_filter_unique.html)
for help diagnosing issues with data organization.

### Dealing with noise in recorded timestamps

Noise in the recorded timestamps of an input `move2` object can disrupt
the correct identification of the IMU bursts identified by `as_*()`.

- For data stored in expanded format, `as_*()` must derive the implied
  sampling frequency from the individual timestamps recorded in the
  data. Within each burst, all samples must be collected at a fixed
  frequency. However, timestamp errors may make it appear as if the
  sampling frequency has changed, artificially splitting a run of
  samples into multiple bursts.

- For data stored in compact format, sampling frequencies are recorded
  explicitly. However, when data are collected continuously, adjacent
  bursts need to be merged together. Here again, timestamp noise can
  prevent bursts from merging properly if gaps between bursts differ
  from the sampling period implied by the frequency of those two bursts.

You can fine-tune the burst parsing and merging process with the
`freq_tol` and `gap_tol` arguments.

- `freq_tol` determines how much sampling frequency noise is tolerated
  when identifying changes in sampling frequency over the course of a
  series of recorded samples. For example, at `freq_tol = 0.01`, a new
  burst is initiated only when two consecutive sampling frequencies
  differ by more than 1%.

  Thus, at low values of `freq_tol`, small deviations in the sampling
  frequency will trigger a new burst. Larger `freq_tol` values will
  smooth these inconsistencies, combining samples into single bursts.
  However, at high values, `freq_tol` may mask true changes in the
  sampling frequency, producing bursts with spurious sampling
  frequencies. For example, `freq_tol = 0.5` risks combining samples
  from a 30Hz signal with those from a 20Hz signal. Similarly, gradual
  timestamp drift within the `freq_tol` can produce misleading output
  frequencies for a burst.

- `freq_tol` also governs the similarity tolerance between two burst
  sampling frequencies when merging bursts (if
  `merge_continuous = TRUE`). Note that bursts that do not clear the
  `min_freq` threshold are automatically recorded in individual samples
  with `NA` frequency, meaning these cannot later be merged.

- `gap_tol` determines how much deviation in the time gap between bursts
  is tolerated when merging two bursts together, in seconds (if
  `merge_continuous = TRUE`). Two adjacent bursts can be merged when the
  gap between the two matches the sampling period (the reciprocal of the
  frequency) of each burst, and each burst has the same sampling
  frequency (within `freq_tol`). This implies that the two bursts
  represent one continuous stream of data. Small values of `gap_tol`
  require that the gap be a near-exact match to the period implied by
  the sampling frequency of the bursts. Larger values of `gap_tol` will
  ignore larger deviations in gap timing.

  Note that a burst's frequency is recalculated after merging using the
  number of samples and the recorded start and end of the burst. Thus,
  setting a large `gap_tol` may produce bursts that have non-standard
  frequencies, as the gap between the bursts (which deviates from the
  expected sampling frequency) will be incorporated into the samples of
  a single burst.

In general, it is best to keep the tolerance parameters as low as
possible while still accommodating the noise inherent in the timestamp
recordings in your data.

Because of floating-point timestamp noise, some values of `freq_tol` and
`gap_tol` may not always admit the frequencies or gaps that you expect.
To reliably allow frequencies and gaps within a given tolerance, you may
want to set the values slightly above your desired output tolerance.

## See also

[`movebank_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
for supported gyroscope column sets in Movebank.
