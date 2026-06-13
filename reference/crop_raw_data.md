# Crop raw water/air temperature data from HOBO sensors

Crop raw water/air temperature data from HOBO sensors

## Usage

``` r
crop_raw_data(rawdata_loc, ldrtimes_fn, cropped_loc, nfiles = NULL)
```

## Arguments

- rawdata_loc:

  File path to the directory containing the raw data to be cropped. The
  names of the raw data files must be in the format
  sitename_medium_deployseason_deployyear.csv, e.g.
  NolanLower_air_sum_23.csv.

- ldrtimes_fn:

  Filename for the LDRTimes file, the lookup table of deployment and
  retrieval times. This file is assumed to be in the same folder as the
  raw data.

- cropped_loc:

  File path to the directory where cropped data files will be written
  and stored.

- nfiles:

  Number of files to process. Default value is `NULL`, in which case all
  files in the raw data directory `rawdata_loc` are processed. If
  `nfiles` is set to a positive integer such as 3, then only the first
  three files in `rawdata_loc` will be cropped.

## Examples

``` r

# the following two lines of code are only necessary for the example data;
# for your own data, simply set base_loc to the base file path for your
# computer, e.g. base_loc = "data/2022_summer/"
example_loc = fs::path_package("extdata", package = "dataQCtools")
base_loc = paste0(example_loc, "/data/2022_summer/")
rawdata_loc = paste0(base_loc, "1_raw_csv/")
cropped_loc = base_loc
#cropped_loc = paste0(base_loc, "2_cropped_csv/")
#croppedplots_loc = paste0(base_loc, "2_cropped_plots/")
ldrtimes_fn = "LDRTimes_summer22.xlsx"
# check that R can find your raw data files
# Get a list of the filenames of all the raw data files you will be clipping
csv_files = list.files(path = rawdata_loc, pattern = '*csv')
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the raw files you want to crop:
csv_files
#>  [1] "CedarLower_water_sum_22.csv"     "CedarUpper_water_sum_22.csv"    
#>  [3] "ChalaatLower_water_sum_22.csv"   "ChalaatUpper_water_sum_22.csv"  
#>  [5] "Cottonwood_water_sum_22.csv"     "CrippenSprings_water_sum_22.csv"
#>  [7] "EFIronMaiden_water_sum_22.csv"   "EFKalaloch_water_sum_22.csv"    
#>  [9] "EastPole_water_sum_22.csv"       "ElkLower_water_sum_22.csv"      
#> [11] "ElkUpper_water_sum_22.csv"       "Fletcher_water_sum_22.csv"      
#> [13] "GoodmanLower_water_sum_22.csv"   "GoodmanUpper_water_sum_22.csv"  

# read in LDRTimes lookup table and take a look at it
ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))
ldrtimes # or View(ldrtimes)
#> # A tibble: 65 × 9
#>    site          media latitude longitude unit_sn deploy_time        
#>    <chr>         <chr> <lgl>    <lgl>     <lgl>   <dttm>             
#>  1 Alder         water NA       NA        NA      2022-05-26 16:00:00
#>  2 AndersonLower water NA       NA        NA      2022-05-13 16:00:00
#>  3 AndersonUpper water NA       NA        NA      2022-05-27 16:00:00
#>  4 BradenLower   water NA       NA        NA      2022-05-11 16:00:00
#>  5 BradenUpper   water NA       NA        NA      2022-05-25 16:00:00
#>  6 CanyonSprings water NA       NA        NA      2022-05-19 16:00:00
#>  7 CedarLower    water NA       NA        NA      2022-05-20 16:00:00
#>  8 CedarUpper    water NA       NA        NA      2022-05-25 16:00:00
#>  9 ChalaatLower  water NA       NA        NA      2022-05-13 16:00:00
#> 10 ChalaatUpper  water NA       NA        NA      2022-05-11 16:00:00
#> # ℹ 55 more rows
#> # ℹ 3 more variables: retrieval_time <dttm>, deploy_season <chr>,
#> #   deploy_year <dbl>

# then crop the data!
crop_raw_data(rawdata_loc, ldrtimes_fn, cropped_loc)
#> Reading file 1 of 14: CedarLower_water_sum_22.csv
#> Reading file 2 of 14: CedarUpper_water_sum_22.csv
#> Reading file 3 of 14: ChalaatLower_water_sum_22.csv
#> Reading file 4 of 14: ChalaatUpper_water_sum_22.csv
#> Reading file 5 of 14: Cottonwood_water_sum_22.csv
#> Reading file 6 of 14: CrippenSprings_water_sum_22.csv
#> Reading file 7 of 14: EFIronMaiden_water_sum_22.csv
#> Reading file 8 of 14: EFKalaloch_water_sum_22.csv
#> Reading file 9 of 14: EastPole_water_sum_22.csv
#> Reading file 10 of 14: ElkLower_water_sum_22.csv
#> Reading file 11 of 14: ElkUpper_water_sum_22.csv
#> Reading file 12 of 14: Fletcher_water_sum_22.csv
#> Reading file 13 of 14: GoodmanLower_water_sum_22.csv
#> Reading file 14 of 14: GoodmanUpper_water_sum_22.csv
#> Done.
```
