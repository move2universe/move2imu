# Package index

## IMU vectors

Build and explore IMU data vectors

- [`acc()`](https://move2universe.github.io/move2imu/reference/imu_constructors.md)
  [`mag()`](https://move2universe.github.io/move2imu/reference/imu_constructors.md)
  [`gyro()`](https://move2universe.github.io/move2imu/reference/imu_constructors.md)
  : Create an IMU vector
- [`is_acc()`](https://move2universe.github.io/move2imu/reference/imu-predicates.md)
  [`is_mag()`](https://move2universe.github.io/move2imu/reference/imu-predicates.md)
  [`is_gyro()`](https://move2universe.github.io/move2imu/reference/imu-predicates.md)
  : Check sensor type of an IMU vector
- [`n_axis()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  [`n_samples()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  [`burst_dur()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  [`burst_intervals()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  [`imu_units()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  [`is_uniform()`](https://move2universe.github.io/move2imu/reference/imu-properties.md)
  : Burst properties of an IMU vector
- [`bursts()`](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  [`` `bursts<-`() ``](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  [`freqs()`](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  [`` `freqs<-`() ``](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  [`starts()`](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  [`` `starts<-`() ``](https://move2universe.github.io/move2imu/reference/imu-fields.md)
  : Access and modify fields of an IMU vector
- [`summary(`*`<imu>`*`)`](https://move2universe.github.io/move2imu/reference/imu_summary.md)
  : Summarize an IMU vector
- [`plot_time()`](https://move2universe.github.io/move2imu/reference/plot_time.md)
  : Plot bursts over time

## Extracting IMU data from move2 / Movebank

Identify IMU data in a `move2` object and extract into an IMU vector.

- [`as_acc()`](https://move2universe.github.io/move2imu/reference/as_acc.md)
  :

  Convert an object to an `acc` vector

- [`as_gyro()`](https://move2universe.github.io/move2imu/reference/as_gyro.md)
  :

  Convert an object to a `gyro` vector

- [`as_mag()`](https://move2universe.github.io/move2imu/reference/as_mag.md)
  :

  Convert an object to a `mag` vector

- [`imu_colset()`](https://move2universe.github.io/move2imu/reference/imu_colset.md)
  :

  Specify IMU data columns present in a `move2` object

- [`active_acc_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
  [`active_mag_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
  [`active_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/active_colsets.md)
  :

  Identify IMU columns present in a `move2` object

- [`movebank_acc_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
  [`movebank_mag_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
  [`movebank_gyro_colsets()`](https://move2universe.github.io/move2imu/reference/movebank_colsets.md)
  : View standard Movebank IMU data column sets

- [`has_acc()`](https://move2universe.github.io/move2imu/reference/has_imu.md)
  [`has_mag()`](https://move2universe.github.io/move2imu/reference/has_imu.md)
  [`has_gyro()`](https://move2universe.github.io/move2imu/reference/has_imu.md)
  :

  Identify rows in a `move2` that contain IMU data

## Combining and splitting

Join continuous bursts together or split an IMU vector into pieces.

- [`merge_imu()`](https://move2universe.github.io/move2imu/reference/merge_imu.md)
  : Merge adjacent bursts in an IMU vector
- [`split_imu()`](https://move2universe.github.io/move2imu/reference/split_imu.md)
  : Split an IMU vector at regular intervals

## Calibration and units

Transform raw ADC counts to physical units, override per-axis
orientation, and look up tag manufacturer calibration defaults.

- [`acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
  [`as_acc_calibration()`](https://move2universe.github.io/move2imu/reference/acc_calibration.md)
  : Create calibrations for raw acceleration values
- [`transform_imu()`](https://move2universe.github.io/move2imu/reference/transform_imu.md)
  : Apply a sensor calibration to an IMU vector
- [`eobs_default_specs()`](https://move2universe.github.io/move2imu/reference/eobs_default_specs.md)
  : Default e-obs tag configuration table
- [`set_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md)
  [`drop_imu_units()`](https://move2universe.github.io/move2imu/reference/set_imu_units.md)
  : Manage units in IMU burst data

## Example data

Bundled datasets used throughout examples and vignettes.

- [`albatrosses()`](https://move2universe.github.io/move2imu/reference/example_data.md)
  [`gulls()`](https://move2universe.github.io/move2imu/reference/example_data.md)
  : move2imu example datasets

- [`acc_example()`](https://move2universe.github.io/move2imu/reference/acc_example.md)
  :

  Example `acc` vector
