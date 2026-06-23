# Package index

## IMU vectors

Build and explore IMU data vectors

- [`acc()`](https://robe2037.github.io/move2imu/reference/imu_constructors.md)
  [`mag()`](https://robe2037.github.io/move2imu/reference/imu_constructors.md)
  [`gyro()`](https://robe2037.github.io/move2imu/reference/imu_constructors.md)
  : Create an IMU vector
- [`is_acc()`](https://robe2037.github.io/move2imu/reference/imu-predicates.md)
  [`is_mag()`](https://robe2037.github.io/move2imu/reference/imu-predicates.md)
  [`is_gyro()`](https://robe2037.github.io/move2imu/reference/imu-predicates.md)
  : Check sensor type of an IMU vector
- [`n_axis()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  [`n_samples()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  [`burst_dur()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  [`burst_intervals()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  [`imu_units()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  [`is_uniform()`](https://robe2037.github.io/move2imu/reference/imu-properties.md)
  : Burst properties of an IMU vector
- [`bursts()`](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  [`` `bursts<-`() ``](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  [`freqs()`](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  [`` `freqs<-`() ``](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  [`starts()`](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  [`` `starts<-`() ``](https://robe2037.github.io/move2imu/reference/imu-fields.md)
  : Access and modify fields of an IMU vector
- [`summary(`*`<imu>`*`)`](https://robe2037.github.io/move2imu/reference/imu_summary.md)
  [`plot(`*`<imu_summary>`*`)`](https://robe2037.github.io/move2imu/reference/imu_summary.md)
  : Summarize and plot an IMU vector
- [`plot_time()`](https://robe2037.github.io/move2imu/reference/plot_time.md)
  : Plot bursts over time

## Extracting IMU data from move2 / Movebank

Identify IMU data in a `move2` object and extract into an IMU vector.

- [`as_acc()`](https://robe2037.github.io/move2imu/reference/as_acc.md)
  :

  Convert an object to an `acc` vector

- [`as_gyro()`](https://robe2037.github.io/move2imu/reference/as_gyro.md)
  :

  Convert an object to a `gyro` vector

- [`as_mag()`](https://robe2037.github.io/move2imu/reference/as_mag.md)
  :

  Convert an object to a `mag` vector

- [`imu_colset()`](https://robe2037.github.io/move2imu/reference/imu_colset.md)
  :

  Specify IMU data columns present in a `move2` object

- [`active_acc_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md)
  [`active_mag_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md)
  [`active_gyro_colsets()`](https://robe2037.github.io/move2imu/reference/active_colsets.md)
  :

  Identify IMU columns present in a `move2` object

- [`movebank_acc_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md)
  [`movebank_mag_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md)
  [`movebank_gyro_colsets()`](https://robe2037.github.io/move2imu/reference/movebank_colsets.md)
  : View standard Movebank IMU data column sets

- [`has_acc()`](https://robe2037.github.io/move2imu/reference/has_imu.md)
  [`has_mag()`](https://robe2037.github.io/move2imu/reference/has_imu.md)
  [`has_gyro()`](https://robe2037.github.io/move2imu/reference/has_imu.md)
  :

  Identify rows in a `move2` that contain IMU data

## Combining and splitting

Join continuous bursts together or split an IMU vector into pieces.

- [`merge_imu()`](https://robe2037.github.io/move2imu/reference/merge_imu.md)
  : Merge adjacent bursts in an IMU vector
- [`split_imu()`](https://robe2037.github.io/move2imu/reference/split_imu.md)
  : Split an IMU vector at regular intervals

## Calibration and units

Transform raw ADC counts to physical units, override per-axis
orientation, and look up tag manufacturer calibration defaults.

- [`acc_calibration()`](https://robe2037.github.io/move2imu/reference/acc_calibration.md)
  [`as_acc_calibration()`](https://robe2037.github.io/move2imu/reference/acc_calibration.md)
  : Create calibrations for raw acceleration values
- [`transform_imu()`](https://robe2037.github.io/move2imu/reference/transform_imu.md)
  : Apply a sensor calibration to an IMU vector
- [`eobs_default_specs()`](https://robe2037.github.io/move2imu/reference/eobs_default_specs.md)
  : Default e-obs tag configuration table
- [`set_imu_units()`](https://robe2037.github.io/move2imu/reference/set_imu_units.md)
  [`drop_imu_units()`](https://robe2037.github.io/move2imu/reference/set_imu_units.md)
  : Manage units in IMU burst data

## Burst computations

Get the dynamic body acceleration and/or peak frequency of a burst

- [`vedba()`](https://robe2037.github.io/move2imu/reference/dba.md)
  [`odba()`](https://robe2037.github.io/move2imu/reference/dba.md) :

  Calculate dynamic body acceleration (DBA) for an `acc` vector

- [`peak_frequency()`](https://robe2037.github.io/move2imu/reference/peak_frequency.md)
  : Calculate the peak frequency per axis for bursts

## Example data

Bundled datasets used throughout examples and vignettes.

- [`albatrosses()`](https://robe2037.github.io/move2imu/reference/example_data.md)
  [`gulls()`](https://robe2037.github.io/move2imu/reference/example_data.md)
  : move2imu example datasets

- [`acc_example()`](https://robe2037.github.io/move2imu/reference/acc_example.md)
  :

  Example `acc` vector
