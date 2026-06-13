# Compute 7DADM

For each period of seven consecutive days, this function computes the
daily max water temperature for each day, then computes the average of
those daily max temperatures. This implementation assumes the data has
already been quality-controlled to remove or impute data during time
periods in which the logger was dewatered, buried, or nonfunctional.

## Usage

``` r
compute_7DADM(all_data)
```

## Arguments

- all_data:

  An R data frame object with the data to be used in computing 7DADM. In
  compute_7DADM_and_exceedance(), this data frame is the output of
  read_in_qcd_data().

## Value

A data frame with the following four columns:

- SiteName

- Date

- DailyMax, daily maximum temperature

- sevenDADM, the newly calculated 7DADM values
