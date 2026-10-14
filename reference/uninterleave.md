# Uninterleave list

Takes a `list` that is actually interleaved elements from multiple
sources and uninterleaves them into the separate sources. For instance,
a `list` of blockmeasures that actually corresponds to two different
plates can be split into two `list`s, each of the blockmeasures
corresponding to a single plate. `uninterleave` assumes that the desired
sub-groups are perfectly interleaved in the input (e.g. items belong to
sub-groups `1,2,3,1,2,3,...`)

## Usage

``` r
uninterleave(interleaved_list, n)
```

## Arguments

- interleaved_list:

  A `list` of R objects

- n:

  How many output sub `list`s there should be (i.e. how many groups the
  interleaved `list` should be divided into)

## Value

A `list` of `list`s of R objects
