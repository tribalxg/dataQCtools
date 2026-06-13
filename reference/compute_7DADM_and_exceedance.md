# Compute 7DADM and identify site exceedances

This function assumes the data come from a single water year with data
in two deployments, a fall deployment and a summer deployment. For
example, for the 2021-2022 water year as in the provided example data,
the two deployment seasons would be fall 2021 and summer 2022.

## Usage

``` r
compute_7DADM_and_exceedance(fall_data_loc, sum_data_loc, lookup_loc)
```

## Arguments

- fall_data_loc:

  File path to directory with quality-controlled fall data.

- sum_data_loc:

  File path to directory with quality-controlled summer data.

- lookup_loc:

  File path to directory with two lookup tables called site_group.csv
  and group_wqs.csv. Examples of these files are provided in the
  lookup_tables folder; for other data beyond the example data, these
  need to be edited to reflect the sites and water quality standards for
  the data of interest.

  Site-group lookup table:

  - Two sites are in the same group if they have the same WQS schedule
    as each other throughout the water year

  - There is one row for each site

  Example:

  |               |       |
  |---------------|-------|
  | Site          | Group |
  | AndersonLower | 1     |
  | AndersonUpper | 1     |
  | CedarLower    | 1     |
  | CedarUpper    | 2     |

  Group-WQS lookup table:

  - Provides details for the WQS schedule for each group

  - There is one row for each group-WQS combination from 01/01 to 12/31
    for example, if a group has three different WQS throughout the water
    year, there are three rows for that group

  - This can be generated using the write_group_wqs() function, or it
    can be written in a spreadsheet and exported to a csv file called
    group_wqs.csv

  Example:

  |       |          |        |     |
  |-------|----------|--------|-----|
  | Group | FromDate | ToDate | WQS |
  | 1     | 01/01    | 12/31  | 15  |
  | 2     | 01/01    | 02/14  | 12  |
  | 2     | 02/15    | 07/01  | 16  |
  | 2     | 07/02    | 12/31  | 12  |

## Value

A data frame with 7DADM and exceedances.

## Examples

``` r

#library(magrittr)

#fall_data_loc <- "data/2021_fall/4_final_data/"
#sum_data_loc <- "data/2022_summer/4_final_data/"
#lookup_loc <- "lookup_tables/"

#compute_7DADM_and_exceedance(fall_data_loc, sum_data_loc, lookup_loc)
```
