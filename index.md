# move2imu

move2imu aims to standardize the storage and analysis of biologging
inertial measurement unit (IMU) data, including accelerometer,
magnetometer, and gyroscope records. The package integrates with
[move2](https://bartk.gitlab.io/move2/), enabling standardized data
processing workflows and allowing IMU data to be analyzed alongside
other observations, including location records.

## Installation

move2imu does not yet exist on CRAN. Instead, you can install the
development version directly:

``` r

# install.packages("pak")
pak::pak("move2universe/move2imu")

# Or, if remotes is already installed:
remotes::install_github("move2universe/move2imu")
```

## Usage

``` r

library(move2imu)
library(move2)

# Extract acceleration data from gulls data
acceleration <- as_acc(gulls())

acceleration <- acceleration[!is.na(acceleration)]

head(acceleration)
#> <acceleration[6]>
#> [1] (-97.75 323.55 1963.95) (-95 267.65 1914.25)    (7.1 301.85 1990.9)    
#> [4] (77.65 372.95 1824.75)  (46.9 349.8 1989)       (-29.15 251.05 2046.6) 
#> # frequency: 20 [Hz]

# Compute values on acceleration bursts
vedba(acceleration)
#>  [1]  184.32546  217.03563  169.67963  159.70308  139.13675   93.78337
#>  [7]   70.52255   47.88465  103.18600  120.17322  180.73977  272.24915
#> [13] 1205.98655  906.47609 1297.89387 1370.41230 1010.59239 1155.86132
#> [19] 1207.07769  348.04025 1359.16413 1271.36056 1320.84767  292.35113
#> [25]  330.88228 1255.16058 1236.19487  162.21664 1570.35847 1167.40129
#> [31]  130.55415  439.56821 1286.00234  148.70816  103.99947  116.59106
#> [37]   76.41446  807.40152  115.69910  428.93748  250.49735  135.25885
#> [43]   94.72192  250.57432  104.08138  323.06970  142.13018  252.64078
#> [49]  161.30022  139.36594  203.29928  172.87414  188.60353  312.24183
#> [55]  225.97108  341.48917  147.20475  279.21028  309.20153
```

## Getting help + Contributing

We welcome feedback and contributions. If you encounter a bug or have
specific feature requests, please create an issue on
[GitHub](https://github.com/move2universe/move2imu).
