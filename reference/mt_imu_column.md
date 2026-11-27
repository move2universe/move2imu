# Get or set names of columns that contain IMU data in a `move2`

Designate columns in a `move2` object that contain a given class of IMU
data. This allows you to flexibly store IMU data produced by
[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
or
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
in a column in a `move2` and access it programmatically, without needing
to hard-code column names.

- `mt_*_column()` returns the name of the column in the input `move2`
  that contains data from the indicated sensor.

- `mt_set_*_column()` sets the column that contains data from the
  indicated sensor.

## Usage

``` r
mt_acc_column(x)

mt_mag_column(x)

mt_gyro_column(x)

mt_set_acc_column(x, value)

mt_set_mag_column(x, value)

mt_set_gyro_column(x, value)
```

## Arguments

- x:

  A `move2` object with an IMU vector column (for instance, produced by
  [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
  [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
  or
  [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)).

- value:

  A string giving the name of the column in `x` that contains the
  extracted IMU data. The column must contain a vector of the
  corresponding sensor type (e.g. `acc`).

## Value

`mt_*_column()` returns a length-one character vector naming the
designated column.

`mt_set_*_column()` returns `x` with the corresponding attribute set to
`value`.

## Details

These functions behave similarly to move2's native helpers (e.g.
[`move2::mt_time_column()`](https://bartk.gitlab.io/move2/reference/column_name.html)).
They work by storing a character attribute in the input `move2` object
naming the relevant column of IMU data.

Save your data to RDS (e.g. with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html)) so the attributes
are kept when data are loaded again.

## Current limitations

The stored column name is an ordinary attribute of the `move2`, so it is
not always kept when the data are modified.

For instance, some common data manipulation operations drop these
attributes, like
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html),
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html),
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
grouped
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
and
[`move2::mt_stack()`](https://bartk.gitlab.io/move2/reference/mt_stack.html).
This behavior depends on how lower-level packages like sf and move2
handle these attributes, so many of these discrepancies may be resolved
in the future.

Further, renaming the column (e.g. with
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html))
does not update the attribute simultaneously, which can create a
mismatch between the stored column name and the name of the column in
the data themselves. Use `mt_set_*_column()` to update the column name
accordingly.

## See also

[`mt_acc()`](https://move2universe.github.io/move2imu/reference/mt_imu.md),
[`mt_mag()`](https://move2universe.github.io/move2imu/reference/mt_imu.md),
[`mt_gyro()`](https://move2universe.github.io/move2imu/reference/mt_imu.md)
to extract the column values.

[`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
[`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
to build IMU columns.

## Examples

``` r
alb <- albatrosses()
alb$acc <- as_acc(alb)
alb <- mt_set_acc_column(alb, "acc")

mt_acc_column(alb)
#> [1] "acc"

mt_acc(alb)[1:10]
#> <acceleration[10]>
#>  [1] <NA>              (1824.17 1913.83) (1904.3 1926.5)   (1823.27 1913.42)
#>  [5] (1826.7 1915.7)   (1719.07 1908.8)  <NA>              (1943.47 2028.05)
#>  [9] (1927.98 2013.72) (1926.7 2008.82) 
#> # frequency: 5 [Hz]
```
