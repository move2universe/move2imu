# Manage units in IMU burst data

Set, convert, or drop units in burst matrices of `acc`, `mag`, and
`gyro` vectors.

`set_imu_units()` attaches units to unitless bursts or converts between
compatible units. The target unit must be dimensionally compatible with
the IMU class:

- `acc`: acceleration units (e.g., `"m/s^2"`, `"standard_free_fall"`)

- `mag`: magnetic flux density units (e.g., `"tesla"`, `"uT"`,
  `"gauss"`)

- `gyro`: angular velocity units (e.g., `"rad/s"`, `"degree/s"`)

`drop_imu_units()` strips units from each burst, leaving the underlying
numeric values unchanged. Bursts that do not carry units are returned
as-is.

To transform raw values to physical units rather than simply attaching
or converting units, use
[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md).

## Usage

``` r
set_imu_units(x, value, ...)

drop_imu_units(x, ...)
```

## Arguments

- x:

  An IMU vector (`acc`, `mag`, or `gyro`)

- value:

  Character specifying the target units (e.g., `"m/s^2"`). For units in
  terms of gravitational acceleration, use `"standard_free_fall"`.

- ...:

  Unused.

## Value

The input vector with units attached to, converted on, or removed from
each burst matrix.

## See also

[`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
to transform raw IMU values

## Examples

``` r
a <- acc_example()

# Attach units to unitless bursts
set_imu_units(a, "m/s^2")
#> <acceleration[2]>
#> [1] (0.67 0.01 1) [m/s^2]  (0.08 -0.52 1) [m/s^2]
#> # frequency: 20 [Hz]

# Convert between units
a_ms2 <- set_imu_units(a, "m/s^2")
set_imu_units(a_ms2, "standard_free_fall")
#> <acceleration[2]>
#> [1] (0.07 0 0.1) [standard_free_fall]     (0.01 -0.05 0.1) [standard_free_fall]
#> # frequency: 20 [Hz]

# Units must be appropriate for the sensor type of the input
try(set_imu_units(a, "kg"))
#> Error in set_imu_units_(x, value, reference = "m/s^2", sensor = "acc") : 
#>   kg units not valid for <acc> vector.
#> ℹ Units must be convertible to m/s^2.

# Strip units back off
drop_imu_units(a_ms2)
#> <acceleration[2]>
#> [1] (0.67 0.01 1)  (0.08 -0.52 1)
#> # frequency: 20 [Hz]
```
