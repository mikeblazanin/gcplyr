# Extract parts of an object

A wrapper for `[` that handles `NA` indices better when using inside
[summarize](https://dplyr.tidyverse.org/reference/summarise.html)

## Usage

``` r
extr_val(x, i, allNA_NA = TRUE, na.rm = FALSE)
```

## Arguments

- x:

  object from which to extract element(s)

- i:

  index specifying element to extract.

- allNA_NA:

  `logical` indicating whether `NA` should be returned when
  `all(is.na(i)) == TRUE`.

- na.rm:

  a `logical` indicating whether missing values should be removed.

## Value

If `allNA_NA = FALSE` and `na.rm = FALSE`, identical to `x[i]`.

If `allNA_NA = FALSE` and `na.rm = TRUE`, identical to
`x[i[!is.na(i)]]`.

If `allNA_NA = TRUE`, identical to `x[i]` unless
`all(is.na(i)) == TRUE`, in which case returns `NA`

## Details

This function was created to better handle the common situation of
extracting values by index inside
[summarize](https://dplyr.tidyverse.org/reference/summarise.html) when
some indices may be `NA`. Often this arises when using a function to
find indices of interest might fail to find any, as can sometimes happen
when using
[which_max_gc](https://mikeblazanin.github.io/gcplyr/reference/WhichMinMaxGC.md)
or
[which_min_gc](https://mikeblazanin.github.io/gcplyr/reference/WhichMinMaxGC.md)
(wrappers for [which.max](https://rdrr.io/r/base/which.min.html) and
[which.min](https://rdrr.io/r/base/which.min.html), respectively),
[find_local_extrema](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
and its wrappers
[first_maxima](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
and
[first_minima](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md),
or
[find_threshold_crosses](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md)
and its wrappers
[first_below](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md)
and
[first_above](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md).
When all indices are `NA`, `[` returns a `vector` of all `NA` of length
`length(x)`. Since this typically raises an undesirable warning or error
from [summarize](https://dplyr.tidyverse.org/reference/summarise.html),
`extr_val` instead returns a single `NA` value.

An alternative (but largely superseded) use of this function is to
extract a value by index *after* `NA` values have been removed. This is
not applicable for any common functions with `gcplyr`, which all return
index values for the original `vector`.
