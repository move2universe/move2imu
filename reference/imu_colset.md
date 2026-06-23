# Specify IMU data columns present in a `move2` object

Define which columns in a `move2` object contain IMU data. Pass the
result as the `colset` argument of
[`as_acc()`](https://robe2037.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://robe2037.github.io/move2imu/reference/as_mag.md),
or
[`as_gyro()`](https://robe2037.github.io/move2imu/reference/as_gyro.md)
to convert those columns into an IMU vector.

`move2` objects store IMU data in two ways:

- **Expanded-format** columns store each IMU sample (possibly for
  multiple axes) in its own row.

- **Compact-format** columns store a burst of IMU samples as a
  space-delimited string. This string must be segmented into
  axis-specific values using an associated column that indicates the
  axes present in the burst. A further column provides the sampling
  frequency of the burst. All three of these columns must be present to
  form a valid compact-format column set.

## Usage

``` r
imu_colset(
  x = NULL,
  y = NULL,
  z = NULL,
  bursts = NULL,
  axes = NULL,
  frequency = NULL
)
```

## Arguments

- x, y, z:

  (Expanded-format) Column name(s) for the X, Y, and/or Z axes.

- bursts:

  (Compact-format) Column name containing the raw burst strings.

- axes:

  (Compact-format) Column name containing the axis labels for each
  burst.

- frequency:

  (Compact-format) Column name containing the sampling frequency for
  each burst.

## Value

An `imu_colset` object of type `"expanded"` or `"compact"`.

## See also

[`as_acc()`](https://robe2037.github.io/move2imu/reference/as_acc.md),
[`as_mag()`](https://robe2037.github.io/move2imu/reference/as_mag.md),
[`as_gyro()`](https://robe2037.github.io/move2imu/reference/as_gyro.md)
to extract IMU data from a move2 object.

[`active_acc_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md),
[`active_mag_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md),
[`active_gyro_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md)
to identify IMU colsets present in a move2 object.

[`movebank_acc_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md),
[`movebank_mag_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md),
[`movebank_gyro_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md)
to see column sets provided by Movebank.

## Examples

``` r
# Expanded-format: one or more axes
imu_colset(x = "my_x", y = "my_y", z = "my_z")
#> <imu_colset> [
#>   X = "my_x",
#>   Y = "my_y",
#>   Z = "my_z"
#> ]
imu_colset(x = "my_x", y = "my_y")
#> <imu_colset> [
#>   X = "my_x",
#>   Y = "my_y"
#> ]

# Compact-format: all three columns required
imu_colset(bursts = "my_raw", axes = "my_axes", frequency = "my_freq")
#> <imu_colset> [
#>   bursts = "my_raw",
#>   axes = "my_axes",
#>   frequency = "my_freq"
#> ]

# Use a colset to extract IMU data from those columns in a move2 object
as_acc(gulls(), colset = imu_colset(x = "acceleration_raw_x"))
#> <acceleration[1239]>
#>    [1] <NA>      (-97.75)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>    [8] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [15] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [22] <NA>      (-95)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [29] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [36] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [43] <NA>      (7.1)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [50] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [57] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [64] <NA>      (77.65)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [71] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [78] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [85] <NA>      (46.9)    <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [92] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [99] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [106] <NA>      (-29.15)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [113] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [120] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [127] <NA>      (119.8)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [134] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [141] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [148] <NA>      (142)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [155] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [162] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [169] <NA>      (11.45)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [176] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [183] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [190] <NA>      (0.4)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [197] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [204] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [211] <NA>      (-12.1)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [218] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [225] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [232] <NA>      (336)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [239] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [246] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [253] <NA>      (-168.85) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [260] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [267] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [274] <NA>      (-280.5)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [281] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [288] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [295] <NA>      (-186.1)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [302] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [309] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [316] <NA>      (-113.85) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [323] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [330] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [337] <NA>      (-221.35) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [344] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [351] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [358] <NA>      (-202)    <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [365] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [372] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [379] <NA>      (-191.1)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [386] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [393] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [400] <NA>      (710.15)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [407] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [414] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [421] <NA>      (-123.35) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [428] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [435] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [442] <NA>      (-211.1)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [449] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [456] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [463] <NA>      (-168.95) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [470] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [477] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [484] <NA>      (511.8)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [491] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [498] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [505] <NA>      (328.2)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [512] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [519] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [526] <NA>      (-169.35) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [533] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [540] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [547] <NA>      (-157.7)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [554] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [561] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [568] <NA>      (353)     <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [575] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [582] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [589] <NA>      (-77.75)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [596] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [603] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [610] <NA>      (-213.55) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [617] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [624] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [631] <NA>      (548.8)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [638] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [645] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [652] <NA>      (102.95)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [659] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [666] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [673] <NA>      (-167.15) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [680] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [687] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [694] <NA>      (160.3)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [701] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [708] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [715] <NA>      (75.7)    <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [722] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [729] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [736] <NA>      (64.65)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [743] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [750] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [757] <NA>      (180.35)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [764] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [771] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [778] <NA>      (-96.05)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [785] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [792] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [799] <NA>      (376.3)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [806] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [813] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [820] <NA>      (-26.5)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [827] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [834] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [841] <NA>      (300.3)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [848] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [855] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [862] <NA>      (145.15)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [869] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [876] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [883] <NA>      (-303.65) <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [890] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [897] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [904] <NA>      (127.3)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [911] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [918] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [925] <NA>      (126.15)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [932] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [939] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [946] <NA>      (232.95)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [953] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [960] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [967] <NA>      (101.5)   <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [974] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [981] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [988] <NA>      (187.05)  <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [995] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1002] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1009] <NA>      (125.6)   <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1016] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1023] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1030] <NA>      (412.05)  <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1037] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1044] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1051] <NA>      (219.25)  <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1058] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1065] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1072] <NA>      (104.1)   <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1079] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1086] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1093] <NA>      (67.7)    <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1100] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1107] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1114] <NA>      (65.9)    <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1121] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1128] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1135] <NA>      (81.7)    <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1142] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1149] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1156] <NA>      (69.6)    <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1163] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1170] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1177] <NA>      (147.1)   <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1184] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1191] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1198] <NA>      (67)      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1205] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1212] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1219] <NA>      (19.1)    <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1226] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1233] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> # frequency: 20 [Hz]
```
