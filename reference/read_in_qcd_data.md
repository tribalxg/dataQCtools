# Read in quality-controlled data for all sites and deployments for a given water year

This function is called by compute_7DADM_and_exceedance() to read in and
combine all the quality-controlled data into a single R data frame
object before computing 7DADM.

## Usage

``` r
read_in_qcd_data(fall_data_loc, sum_data_loc)
```

## Arguments

- fall_data_loc:

  File path to directory containing fall deployment data files; for
  water year 2021-2022, the fall deployment season is Fall 2021. Files
  must be csv format.

- sum_data_loc:

  File path to directory containing summer deployment data files; for
  water year 2021-2022, the summer deployment season is Summer 2022.
  Files must be csv format.

## Value

A single data frame object containing all the data from fall_data_loc
and sum_data_loc
