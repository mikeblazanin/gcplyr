# gcplyr 1.6.0.9000

stuff here

# gcplyr 1.6.0

* The behavior of smooth_data when return_fitobject = TRUE has changed. It now returns a list containing the output of each smoothing method directly (previously the returned objects were modified from the output of the smoothing method to always have a first element of 'fitted' values and a second element of 'residuals')
>>>>>>> master

# gcplyr 1.5.2

* Citation and load message now point to the bioRxiv preprint for the package: Blazanin, Michael. 2023. gcplyr: an R package for microbial growth curve data analysis. bioRxiv doi: 10.1101/2023.04.30.538883.

# gcplyr 1.5.1

* The "Dealing with noise" vignette was rewritten to be much more concise

* Other minor fixes for CRAN checks

# gcplyr 1.5.0

* Several fixes to bugs where calc_deriv had been returning incorrect values when trans_y = 'log' and either blank was specified or fitting was not used (i.e. window_width and window_width_n were both NULL)

* Bug fixes and better warning messages in lag_time, especially relating to negative and infinite y-values following log-transformation

* New function: solve_linear, which enables easy calculation of the slope between two points, or finding an additional point on a known line

* New functions: which_min_gc, which_max_gc, min_gc, max_gc, and extr_val. These serve as versions of the base functions which.min, which.max, min, max, and [] (respectively) that have better defaults and handling of edge cases (often related to NA values) for growth curve analyses with dplyr::summarize

* The auc function has two new arguments. 'blank' allows setting of a blank value, and 'neg.rm' gives users a choice in how to handle below-zero values

* Various documentation improvements in both the manual and vignettes

# gcplyr 1.4.1

* Bug fix for calc_deriv when all or nearly all values become NA after log-transformation

# gcplyr 1.4.0

* There are planned changes for smooth_data with return_fitobject = TRUE: in a forthcoming version (the first version released after Sep 1, 2023), return_fitobject = TRUE will return a list of the object created by the underlying method directly. The current behavior modifies the generated object so that 'fitted' is always the first element and 'residuals' is always the second element

* mdp() is a new function, it is simply a shorthand alias for make_designpattern()

* Column numbers and row numbers in read_* and import_* functions can now be specified using a mix of base-10 numbers and base-26 Excel-style letters

* separate_tidy now coerces strings of "NA" into NA values by default

* Bug fix in make_design where NA values were being returned as "NA" (a string) instead of NA (missing value indicator) when output_format = 'tidy' and there were multiple design columns

* Bug fix for make_design where pattern could not be just "1"

* Bug fix where an error arose in calc_deriv when trans_y = 'log' was used with y values at or below 0. calc_deriv now handles the resulting NA/NaN values and raises a warning

* Bug fixes in first_maxima and first_minima

* Added warning when using make_design and it's likely that setting a custom lookup_tbl_start was forgotten

* Improved documentation for make_designpattern

# gcplyr 1.3.0

* New function lag_time to calculate lag time

* New function doubling_time to convert per-capita growth rate into equivalent doubling time

* smooth_data and calc_deriv now pass a warning when they are used outside of dplyr::mutate or on ungrouped data

* make_design has better warning messages for common mistakes

* Major edits to most of the vignettes

* Various documentation improvements

# gcplyr 1.2.0

* The function first_peak has been renamed to first_maxima. first_peak will continue to function normally for a long time but with a warning to use first_maxima instead.

* New functions have been added that are shortcuts for common use cases: first_minima and first_above

* Bug fixes for find_threshold_crossings related to return_endpoints = TRUE

* Bug fixes for find_threshold_crossings and find_local_extrema when input values are NA

* Style and content improvements in vignettes 

* Help pages have been improved

# gcplyr 1.1.0

* The default behavior of `write_blocks` has changed: users are now required to specify the `file` argument to be either `NULL`, a file name, or a vector of file names. For the old default naming, specify `file = NULL`. This change is required for CRAN compatibility.

* Minor changes to citation

* Tweaked documentation for CRAN compatibility

# gcplyr 1.0.2

* Tweaks to CITATION for CRAN compatibility

# gcplyr 1.0.1

* Readability improvements to "Dealing with noise" vignette

* Minor tweaks to several vignettes to reduce build time for CRAN compatibility

# gcplyr 1.0.0

* There are only minor documentation tweaks for CRAN compatibility since 0.12.3. This is version 1.0.0 because it will be the first to be released on CRAN and denotes that gcplyr will be stable going forward

# gcplyr 0.12.3

* Tweaks for CRAN compatibility, largely minor documentation changes

# gcplyr 0.12.2

* Improved clarity in vignettes, especially in the calculating derivatives and analyzing data pages

* New vignette on dealing with noise in data

* Small tweaks to other documentation

# gcplyr 0.12.1

* Minor bug fixes

# gcplyr 0.12

* There is a new citation. Run citation("gcplyr") to see the new version

* smooth_data methods moving-average and moving-median now accept a new smoothing parameter: window_width

* find_local_extrema now has arguments window_width, window_width_n, and window_height (for naming consistency with smooth_data and calc_deriv arguments). Arguments width_limit, width_limit_n, and height_limit have been deprecated.

* calc_deriv can now calculate derivatives using a linear fit over multiple data points determined by arguments window_width and/or window_width_n. For per-capita derivatives, y-values can be fit as-supplied and divided by the mid-point y-value, or can be fit after log-transformation

* The gcplyr-workflow vignette was split into multiple smaller vignettes

* A new data.frame is included with gcplyr: example_data_noiseless. This data is the same as example_data but does not include any of the simulated noise present in example_data.

* Some small numerical changes to example_data values occurred on re-generation of example_data.

* Packages mgcv and readxl are now Suggests (previously they were Imports), with errors thrown when they are not installed but are required for gcplyr functionality

* find_local_extrema is now much faster

# gcplyr 0.11.2

* A new vignette section on running statistics 

* In the vignette, use of dplyr::mutate in the smoothing and derivatives sections

# gcplyr 0.11.1

* updates to vignette and README

# gcplyr 0.11

* This is the first public release of gcplyr as an Open Beta.

* Documentation should be complete, although there are planned additions to the vignette that have not been completed and included here.

* Functions and arguments should largely be stabilized, although some small changes may occur going forward following user feedback.

* Internal tests are mostly complete, although additional edge cases may be added as bugs are reported.
