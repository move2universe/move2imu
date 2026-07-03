# Convert an object to an `acc` vector

Extract `acc` data from a `move2` or convert an object to an `acc`
vector.

For a `move2`, `acc` data are extracted from the object's
[`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md).

## Usage

``` r
as_acc(x, ...)

# Default S3 method
as_acc(x, ...)

# S3 method for class 'move2'
as_acc(
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

  A `move2` containing acceleration data as collected by EOBS, Ornitela,
  or similar tracking devices. Most of the time this will be either
  loaded from disk using
  [move2::mt_read](https://bartk.gitlab.io/move2/reference/mt_read.html)
  or downloaded using
  [move2::movebank_download_study](https://bartk.gitlab.io/move2/reference/movebank_download_study.html).

- ...:

  currently not used

- colset:

  An `imu_colset` object or list of `imu_colset` objects specifying the
  columns of `x` that contain acceleration data. By default, constructs
  bursts for all column sets that are detected in `x` that also contain
  data (see
  [`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)).

  Several common colsets are listed under
  [`movebank_acc_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md).
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

For data stored in expanded format, `as_*()` must derive the implied
sampling frequency from the individual timestamps recorded in the data.
Within each burst, all samples must be collected at a fixed frequency.
However, timestamps may also contain occasional imprecisions. These
deviations then violate the requirement that a burst have
consistently-sampled records, forcing the creation of a new, separate
burst at the point where the deviation occurs.

To avoid this behavior, you can set the `tolerance` parameter to ignore
a certain amount of noise in the recorded timestamps. Timestamps can
vary from the value implied by the local sampling frequency (which is
indicated by the gaps between adjacent sample timestamps) up to the
value of `tolerance` while still being considered as part of the same
burst. This prevents the partitioning of bursts at artificial boundaries
where timestamps contain small noise errors.

The `tolerance` also governs the largest amount that the end of one
burst and the start of the next burst can vary while still being
considered equal for the purposes of merging bursts (see the
`merge_continuous` argument.)

Note that increasing tolerance comes at the cost of reducing timestamp
precision; once bursts are constructed, recovering timestamps for
individual samples within a burst is only accurate to roughly the size
of `tolerance`. At large tolerance values, you may also mask true
frequency changes in the data.

## See also

[`movebank_acc_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
for supported acceleration column sets in Movebank.

## Examples

``` r
# Example compact-format data: acc bursts stored in strings in individual rows
alb <- albatrosses()

as_acc(alb)
#> <acceleration[54]>
#>  [1] <NA>              (1824.17 1913.83) (1904.3 1926.5)   (1823.27 1913.42)
#>  [5] (1826.7 1915.7)   (1719.07 1908.8)  <NA>              (1943.47 2028.05)
#>  [9] (1927.98 2013.72) (1926.7 2008.82)  (1940.97 2023.93) (1940.02 2021.87)
#> [13] (1969.07 2044.2)  <NA>              (1962.88 2033.35) (2062.08 2023.48)
#> [17] (2090.73 2025.83) (2083.92 2051.48) (1887.58 1927.13) <NA>             
#> [21] (1846.38 1948.13) (1881.15 1938.03) (1841.58 2093.23) (1778.92 2129.55)
#> [25] (1812.17 1952.13) <NA>              (1790.4 1927.4)   (1811.85 1923.43)
#> [29] (1804.47 1935.05) (1811.45 1937.57) (2012.53 2227.17) <NA>             
#> [33] (1970.52 2238.62) (2130.75 2082.43) (1997.18 2074.33) (2049.4 2060.38) 
#> [37] (1975.93 1868.22) <NA>              (1920.02 2128.58) (1879.35 1920.62)
#> [41] (1927.5 1941.72)  (1949.27 1962.57) (1936.87 1918.93) <NA>             
#> [45] (1672.52 1865.77) (1942.15 1922.33) (2072.85 1952.1)  (1617.97 1944.63)
#> [49] (1845.87 1921.13) <NA>              (1838.77 1929.6)  (1843.57 1929.52)
#> [53] (1852.25 1929.65) (1829.97 1932.28)
#> # frequency: 5 [Hz]

# Expanded-format data: bursts are constructed from samples stored across rows
g <- gulls()

head(as_acc(g))
#> <acceleration[6]>
#> [1] <NA>                    (-97.75 323.55 1963.95) <NA>                   
#> [4] <NA>                    <NA>                    <NA>                   
#> # frequency: 20 [Hz]

# Specify the columns to extract explicitly with a colset, e.g. to
# pull a single axis from the gulls data:
as_acc(g, colset = imu_colset(x = "acceleration_raw_x")) |>
  head()
#> <acceleration[6]>
#> [1] <NA>     (-97.75) <NA>     <NA>     <NA>     <NA>    
#> # frequency: 20 [Hz]

# Output is index-matched to the input move2, so the result can be
# easily attached:
g$a <- as_acc(g)

# To instead drop missing bursts, set `drop = TRUE`:
as_acc(g, drop = TRUE)
#> <acceleration[59]>
#>  [1] (-97.75 323.55 1963.95)  (-95 267.65 1914.25)     (7.1 301.85 1990.9)     
#>  [4] (77.65 372.95 1824.75)   (46.9 349.8 1989)        (-29.15 251.05 2046.6)  
#>  [7] (119.8 229.3 2016.6)     (142 214.65 2010.2)      (11.45 270.5 2002.95)   
#> [10] (0.4 162.35 1993)        (-12.1 139.65 1962.35)   (336 403.35 1916.3)     
#> [13] (-168.85 571.75 1854.15) (-280.5 543.2 1928.8)    (-186.1 570.55 1866.95) 
#> [16] (-113.85 541.65 1900.4)  (-221.35 564 1924.8)     (-202 515.6 1754.85)    
#> [19] (-191.1 498.15 1806.95)  (710.15 266.85 1854.85)  (-123.35 586.2 1935.45) 
#> [22] (-211.1 569.7 1885.2)    (-168.95 529.8 1907.7)   (511.8 268.9 1912.55)   
#> [25] (328.2 277.55 1982.5)    (-169.35 549.45 1857.1)  (-157.7 577.95 1910.3)  
#> [28] (353 374.6 2069.4)       (-77.75 526.6 2053.4)    (-213.55 510.35 1899.85)
#> [31] (548.8 370.9 1784.35)    (102.95 258.4 2014.15)   (-167.15 674.7 1961.2)  
#> [34] (160.3 311.8 2062.7)     (75.7 318.6 1945.35)     (64.65 295.65 1895.85)  
#> [37] (180.35 249.8 1932)      (-96.05 404.35 1908.9)   (376.3 149.5 1997.5)    
#> [40] (-26.5 580.45 1975.45)   (300.3 367.2 1938.7)     (145.15 261.4 1834.15)  
#> [43] (-303.65 644.45 1884.4)  (127.3 319.6 2049)       (126.15 426.2 1936.1)   
#> [46] (232.95 319.1 1879.4)    (101.5 287.65 1947.15)   (187.05 328.45 2010.8)  
#> [49] (125.6 258.8 2052.9)     (412.05 303.05 1866.4)   (219.25 281.75 1997.2)  
#> [52] (104.1 219.75 1918.6)    (67.7 163.4 1911.65)     (65.9 221.15 1943.15)   
#> [55] (81.7 310.9 1975.45)     (69.6 258.85 1897.65)    (147.1 407.4 2043.85)   
#> [58] (67 294.4 1886.05)       (19.1 378.15 1886.05)   
#> # frequency: 20 [Hz]
```
