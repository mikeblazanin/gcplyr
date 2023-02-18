# gcplyr 1.2.0.9000

* lag_time
* doubling_time
* analyze vignette rework
* other vignette conciseness
* various documentation improvements
* warnings for ungrouped data in smooth_data and calc_deriv
* better warnings in make_design

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
