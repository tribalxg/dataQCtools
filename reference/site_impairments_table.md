# Create a table of site impairments

Given quality-controlled data for a given water year, this function
identifies which sites are impaired and the dates of their exceedances.
A creek/site is impaired if its temperatures exceed water quality
standards (WQS) at least twice during the water year and there is a
period of at least seven days between two of those exceedances.

## Usage

``` r
site_impairments_table(wq_data)
```

## Arguments

- wq_data:

  Data set with exceedances, such as the output of
  compute_7DADM_and_exceedance().

## Value

A table such as the following:

|        |          |                     |
|--------|----------|---------------------|
| Site   | Impaired | Dates of impairment |
| Site 1 | No       |                     |
| Site 2 | Yes      | 3/17-3/19, 5/21     |
| Site 3 | Yes      | 10/20, 4/28-4/29    |
| Site 4 | No       |                     |
| Site 5 | No       |                     |
