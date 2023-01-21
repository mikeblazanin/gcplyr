# gcplyr 0.11.2.9000 (development)

* moving average now accepts window_width (and maintains use of window_width_n)

* moving median now accepts window_width (and maintains use of window_width_n)

* calc derivs now accepts window_width, window_width_n

* calc_derivs can now fit on log transformed y vals

* find_local_extrema efficiency upgrade

* find_local_extrema now uses window_width, window_width_n, and window_height arguments (width_limit, width_limit_n, and height_limit are deprecated)

*mgcv and readxl are now suggested, but not required, for use

*new citation format

*example_data_noiseless

*unavoidable changes to example_data

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
