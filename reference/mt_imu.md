# Access parsed IMU data stored in a `move2`

Return the values contained in the currently designated column storing
data for the indicated sensor.

To set a column that contains data from a given sensor, use the
corresponding setting function. For instance,
[`mt_set_acc_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md).

## Usage

``` r
mt_acc(x)

mt_mag(x)

mt_gyro(x)
```

## Arguments

- x:

  A `move2` object with an IMU vector column (for instance, produced by
  [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md),
  [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md),
  or
  [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)).

## Value

An object inheriting from class `imu` containing the data in the
indicated column of `x`.

## Details

The data contained in the designated IMU column for the given sensor
must be in the corresponding class. For instance, the `acc_column` set
by
[`mt_set_acc_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md)
must contain an `acc` vector. `mt_acc()` validates the data class before
returning.

The designated column can be lost or become out of date when the data
are modified. See the "Current limitations" section of
[`mt_acc_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md).

## See also

[`mt_acc_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md),
[`mt_mag_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md),
[`mt_gyro_column()`](https://move2universe.github.io/move2imu/reference/mt_imu_column.md)
to get or set the column that contains parsed data for a given sensor.

## Examples

``` r
alb <- albatrosses()
alb$acc <- as_acc(alb)
alb <- mt_set_acc_column(alb, "acc")

mt_acc(alb)
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
```
