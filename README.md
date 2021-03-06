
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readrdi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paleolimbot/readrdi/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/readrdi/actions)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/readrdi/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/readrdi?branch=master)
<!-- badges: end -->

The goal of readrdi is to provide an non-lossy read function for
acoustic doppler profiles embedded within “RDI” files generated by units
manufactured by RD Instruments. The package has a focus on recovering
all possible information in data.frame form, partial extraction of
specific data types, and inspection of possibly corrupted files. For a
function that does more interpretation and includes more tools for ADCP
manipulation, use `oce::read.adp.rdi()`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/readrdi")
```

## Example

The function you probably want is `read_rdi()`.

``` r
library(readrdi)
ex_file <- system.file("extdata/19101018.rdi", package = "readrdi")
rdi <- read_rdi(ex_file)
str(rdi)
#> List of 8
#>  $ header         :'data.frame': 1 obs. of  3 variables:
#>   ..$ n_data_types: int 7
#>   ..$ data_offset :List of 1
#>   .. ..$ : int [1:7] 20 79 144 346 448 550 652
#>   ..$ data_type   :List of 1
#>   .. ..$ : int [1:7] 0 128 256 512 768 1024 1536
#>  $ fixed_leader   :'data.frame': 1 obs. of  32 variables:
#>   ..$ firmware_version       : chr "51.41"
#>   ..$ system_config          : int 51777
#>   ..$ real_sim_flag          : raw 00
#>   ..$ lag_length             : raw 0d
#>   ..$ n_beams                : raw 04
#>   ..$ n_cells                : raw 19
#>   ..$ pings_per_ensemble     : int 80
#>   ..$ cell_size              : num 4
#>   ..$ blank_after_transmit   : num 1.76
#>   ..$ profiling_mode         : raw 01
#>   ..$ low_corr_thresh        : raw 40
#>   ..$ n_code_reps            : raw 09
#>   ..$ pct_gd_min             : raw 00
#>   ..$ error_velocity_maximum : int 2000
#>   ..$ tpp_minutes            : raw 00
#>   ..$ tpp_seconds            : raw 03
#>   ..$ tpp_hundredths         : raw 00
#>   ..$ coord_transform        : raw 17
#>   ..$ heading_alignment      : num 0
#>   ..$ heading_bias           : num 0
#>   ..$ sensor_source          : raw 7d
#>   ..$ sensors_available      : raw 3d
#>   ..$ bin1_distance          : num 6.02
#>   ..$ transmit_pulse_length  : num 4.06
#>   ..$ wp_ref_layer_average   : int 1281
#>   ..$ false_target_threshold : raw 32
#>   ..$ transmit_lag_distance  : int 46
#>   ..$ cpu_board_serial_number: chr "3a.00.00.02.80.86.01.09"
#>   ..$ system_bandwidth       : int 1
#>   ..$ system_power           : raw ff
#>   ..$ serial_number          : int 9088
#>   ..$ beam_angle             : raw 14
#>  $ variable_leader:'data.frame': 1 obs. of  24 variables:
#>   ..$ ensemble_number     : int 605
#>   ..$ real_time_clock     : chr "19-10-10 18:00:03.08"
#>   ..$ ensemble_number_msb : raw 00
#>   ..$ bit_result          : int 0
#>   ..$ sound_speed         : int 1441
#>   ..$ transducer_depth    : num 61.3
#>   ..$ heading             : num 77.4
#>   ..$ pitch               : num -0.39
#>   ..$ roll                : num 0.37
#>   ..$ salinity            : int 33
#>   ..$ temperature         : num -1.32
#>   ..$ heading_std         : raw 24
#>   ..$ pitch_std           : num 0
#>   ..$ roll_std            : num 0
#>   ..$ transmit_current    : raw 90
#>   ..$ transmit_voltage    : raw 7a
#>   ..$ ambient_temperature : raw 7c
#>   ..$ pressure_plus       : raw 57
#>   ..$ pressure_minus      : raw 47
#>   ..$ attitude_temp       : raw 79
#>   ..$ attitude            : raw 83
#>   ..$ contamination_sensor: raw 9f
#>   ..$ pressure            : num 61.5
#>   ..$ pressure_std        : num 0.15
#>  $ velocity       :'data.frame': 1 obs. of  1 variable:
#>   ..$ velocity:List of 1
#>   .. ..$ : num [1:4, 1:25] -0.016 -0.001 -0.009 0.002 0.002 0.007 -0.007 -0.002 -0.023 0.046 ...
#>  $ correlation    :'data.frame': 1 obs. of  1 variable:
#>   ..$ correlation:List of 1
#>   .. ..$ : raw [1:4, 1:25] 7c 7d 7c 7f ...
#>  $ echo_intensity :'data.frame': 1 obs. of  1 variable:
#>   ..$ echo_intensity:List of 1
#>   .. ..$ : raw [1:4, 1:25] a9 a7 b7 af ...
#>  $ pct_good       :'data.frame': 1 obs. of  1 variable:
#>   ..$ pct_good:List of 1
#>   .. ..$ : raw [1:4, 1:25] 07 00 08 53 ...
#>  $ bottom_track   :'data.frame': 1 obs. of  5 variables:
#>   ..$ bottom_range      :List of 1
#>   .. ..$ : num [1:4] 61.8 60.8 61.1 61.3
#>   ..$ bottom_velocity   :List of 1
#>   .. ..$ : num [1:4] -0.357 -0.279 0.006 -0.001
#>   ..$ bottom_correlation:List of 1
#>   .. ..$ : int [1:4] 254 254 255 254
#>   ..$ bottom_amplitude  :List of 1
#>   .. ..$ : int [1:4] 78 79 82 76
#>   ..$ bottom_pct_good   :List of 1
#>   .. ..$ : int [1:4] 0 0 0 100
```

For more detailed inspection of potentially corrupted files, you can use
`rdi_index()` to locate ensembles. You can subset this index and pass it
to `read_rdi()` to read a subset of the data.

``` r
rdi_index(ex_file)
#>   offset size checksum checksum_calc
#> 1      0  739    10383         10383
```

You can use `rdi_detect_data_types()` to look for the data types. This
is mostly useful to subset and pass to `read_rdi()`.

``` r
(types <- rdi_detect_data_types(ex_file))
#>          header    fixed_leader variable_leader        velocity     correlation 
#>           32639               0             128             256             512 
#>  echo_intensity        pct_good    bottom_track 
#>             768            1024            1536
str(read_rdi(ex_file, types = types["velocity"]))
#> List of 1
#>  $ velocity:'data.frame':    1 obs. of  1 variable:
#>   ..$ velocity:List of 1
#>   .. ..$ : num [1:4, 1:25] -0.016 -0.001 -0.009 0.002 0.002 0.007 -0.007 -0.002 -0.023 0.046 ...
```
