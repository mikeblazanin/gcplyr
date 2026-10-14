# Calculate doubling time equivalent of per-capita growth rate

Provided a `vector` of per-capita growth rates, this function returns
the `vector` of equivalent doubling times

## Usage

``` r
doubling_time(y, x_scale = 1)
```

## Arguments

- y:

  `vector` of per-capita derivative data to calculate the equivalent
  doubling time of

- x_scale:

  `numeric` to scale per-capita derivative values by

  Set `x_scale` to the ratio of the the units of `y` to the desired
  units. E.g. if `y` is in per-second, but the desired doubling time is
  in minutes, `x_scale = 60` (since there are 60 seconds in 1 minute).

## Value

A `vector` of values for the doubling time equivalent to the per-capita
growth rate supplied for `y`
