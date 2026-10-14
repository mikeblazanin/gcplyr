# Package index

## Reading files

Functions for reading files into R

- [`read_blocks()`](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md)
  : Read blocks
- [`read_wides()`](https://mikeblazanin.github.io/gcplyr/reference/read_wides.md)
  : Read wides
- [`read_tidys()`](https://mikeblazanin.github.io/gcplyr/reference/read_tidys.md)
  : Read tidy-shaped files

## Importing files

Wrapper functions that read and transform data

- [`import_blockmeasures()`](https://mikeblazanin.github.io/gcplyr/reference/import_blockmeasures.md)
  : Import blockmeasures
- [`import_blockdesigns()`](https://mikeblazanin.github.io/gcplyr/reference/import_blockdesigns.md)
  : Import blockdesigns

## Creating designs

Functions to facilitate generating designs

- [`make_design()`](https://mikeblazanin.github.io/gcplyr/reference/make_design.md)
  : Make design data.frame(s)
- [`make_designpattern()`](https://mikeblazanin.github.io/gcplyr/reference/make_designpattern.md)
  [`mdp()`](https://mikeblazanin.github.io/gcplyr/reference/make_designpattern.md)
  : Make design pattern
- [`write_blocks()`](https://mikeblazanin.github.io/gcplyr/reference/write_blocks.md)
  : Write block designs to csv

## Manipulating data in R

Functions that transform, manipulate, or reshape data

- [`trans_block_to_wide()`](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md)
  : Transform blocks to wides
- [`trans_wide_to_tidy()`](https://mikeblazanin.github.io/gcplyr/reference/trans_wide_to_tidy.md)
  : Pivot wide-shaped into tidy
- [`merge_dfs()`](https://mikeblazanin.github.io/gcplyr/reference/merge_dfs.md)
  : Collapse a list of data.frame's, or merge two data.frame's together
- [`paste_blocks()`](https://mikeblazanin.github.io/gcplyr/reference/paste_blocks.md)
  : Paste a list of blocks into a single block
- [`separate_tidy()`](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md)
  : Separate a column into multiple columns

## Processing growth curve data

Functions to smooth or calculate derivatives of growth curve data

- [`smooth_data()`](https://mikeblazanin.github.io/gcplyr/reference/smooth_data.md)
  : Smooth data
- [`moving_average()`](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md)
  [`moving_median()`](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md)
  : Moving window smoothing
- [`calc_deriv()`](https://mikeblazanin.github.io/gcplyr/reference/calc_deriv.md)
  : Calculate derivatives of vector of data
- [`gc_smooth.spline()`](https://mikeblazanin.github.io/gcplyr/reference/gc_smooth.spline.md)
  : Fit a Smoothing Spline
- [`train_smooth_data()`](https://mikeblazanin.github.io/gcplyr/reference/train_smooth_data.md)
  : Test efficacy of different smoothing parameters
- [`makemethod_train_smooth_data()`](https://mikeblazanin.github.io/gcplyr/reference/makemethod_train_smooth_data.md)
  : Create method argument for train of growth curve smoothers
- [`predict_interpolation()`](https://mikeblazanin.github.io/gcplyr/reference/predict_interpolation.md)
  : Predict data by linear interpolation from existing data

## Analyzing growth curve data

Functions to analyze growth curve data by summarizing into metrics

- [`find_local_extrema()`](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
  [`first_maxima()`](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
  [`first_minima()`](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
  : Find local extrema of a numeric vector
- [`find_threshold_crosses()`](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md)
  [`first_below()`](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md)
  [`first_above()`](https://mikeblazanin.github.io/gcplyr/reference/ThresholdFunctions.md)
  : Find point(s) when a numeric vector crosses some threshold
- [`auc()`](https://mikeblazanin.github.io/gcplyr/reference/auc.md) :
  Calculate area under the curve
- [`doubling_time()`](https://mikeblazanin.github.io/gcplyr/reference/doubling_time.md)
  : Calculate doubling time equivalent of per-capita growth rate
- [`lag_time()`](https://mikeblazanin.github.io/gcplyr/reference/lag_time.md)
  : Calculate lag time
- [`centroid()`](https://mikeblazanin.github.io/gcplyr/reference/CentroidFunctions.md)
  [`centroid_x()`](https://mikeblazanin.github.io/gcplyr/reference/CentroidFunctions.md)
  [`centroid_y()`](https://mikeblazanin.github.io/gcplyr/reference/CentroidFunctions.md)
  [`centroid_both()`](https://mikeblazanin.github.io/gcplyr/reference/CentroidFunctions.md)
  : Calculate centroid

## Utility functions

Various functions providing simple utilities

- [`uninterleave()`](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md)
  : Uninterleave list
- [`to_excel()`](https://mikeblazanin.github.io/gcplyr/reference/to_excel.md)
  : A function that converts numbers into base-26 Excel-style letters
- [`from_excel()`](https://mikeblazanin.github.io/gcplyr/reference/from_excel.md)
  : A function that converts base-26 Excel-style letters to numbers
- [`solve_linear()`](https://mikeblazanin.github.io/gcplyr/reference/solve_linear.md)
  : Return missing information about a line
- [`max_gc()`](https://mikeblazanin.github.io/gcplyr/reference/MinMaxGC.md)
  [`min_gc()`](https://mikeblazanin.github.io/gcplyr/reference/MinMaxGC.md)
  : Maxima and Minima
- [`which_min_gc()`](https://mikeblazanin.github.io/gcplyr/reference/WhichMinMaxGC.md)
  [`which_max_gc()`](https://mikeblazanin.github.io/gcplyr/reference/WhichMinMaxGC.md)
  : Where is the Min() or Max() or first TRUE or FALSE?
- [`extr_val()`](https://mikeblazanin.github.io/gcplyr/reference/extr_val.md)
  : Extract parts of an object
- [`print_df()`](https://mikeblazanin.github.io/gcplyr/reference/print_df.md)
  : Nicely print the contents of a data.frame

## Example data

Example wide-shaped data.frames of growth curves data

- [`example_widedata`](https://mikeblazanin.github.io/gcplyr/reference/example_widedata.md)
  : Example noisy growth curve data in wide format
- [`example_widedata_noiseless`](https://mikeblazanin.github.io/gcplyr/reference/example_widedata_noiseless.md)
  : Example growth curve data in wide format
- [`example_design_tidy`](https://mikeblazanin.github.io/gcplyr/reference/example_design_tidy.md)
  : Design for example growth curve data

## Example function

Function to generate files or objects seen in documentation examples

- [`make_example()`](https://mikeblazanin.github.io/gcplyr/reference/make_example.md)
  : Create R objects or files as seen in vignette examples

## Legacy functions

These functions have been deprecated

- [`first_peak()`](https://mikeblazanin.github.io/gcplyr/reference/first_peak.md)
  :

  Find the first local maxima of a `numeric` `vector`

- [`make_tidydesign()`](https://mikeblazanin.github.io/gcplyr/reference/make_tidydesign.md)
  : Make tidy design data.frame's

- [`block_tidydesign()`](https://mikeblazanin.github.io/gcplyr/reference/block_tidydesign.md)
  : Turn tidydesign into block format
