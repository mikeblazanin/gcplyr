# gcplyr 0.12.1.9000

* Things here

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
