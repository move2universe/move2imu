# Default e-obs tag configuration table

Returns a data.frame of known e-obs tag generations with their tag ID
ranges and default calibration parameters.

## Usage

``` r
eobs_default_specs()
```

## Value

A data.frame with columns `tag_gen`, `min_tag_id`, `max_tag_id`,
`sensitivity`, `orientation_x`, `orientation_y`, `orientation_z`,
`offset`, and `slope`.

## See also

[`acc_calibration()`](https://robe2037.github.io/move2imu/reference/acc_calibration.md)
to set up tag-specific calibration specifications and
[`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md)
to apply them to eobs acceleration values.

## Examples

``` r
eobs_default_specs()
#>   tag_gen min_tag_id max_tag_id sensitivity orientation_x orientation_y
#> 1       1          1       2241         low             1            -1
#> 2       1          1       2241        high             1            -1
#> 3       2       2242       4117         low             1             1
#> 4       3       4118        Inf         low             1             1
#>   orientation_z offset       slope
#> 1             1   2048 0.002700000
#> 2             1   2048 0.001000000
#> 3             1   2048 0.002200000
#> 4             1   2048 0.001953125
```
