# Match groups of sites to their corresponding water quality standards

Use group-WQS lookup table to determine what the WQS is for each group
over the course of the water year.

## Usage

``` r
match_group_to_WQS(wq_data, group_wqs)
```

## Arguments

- wq_data:

  Data frame

- group_wqs:

  Group-WQS lookup table as an R data.frame object

## Value

Data frame with water quality standards and exceedances
