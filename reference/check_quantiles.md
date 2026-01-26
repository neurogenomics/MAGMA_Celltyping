# Check quantiles

Check whether every column in every matrix in every level of the
CellTypeDataset (`ctd`) has the expected number of quantile bins.

## Usage

``` r
check_quantiles(ctd, matrix_name, numberOfBins, metric = c("n", "max"))
```
