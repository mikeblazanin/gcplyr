##Name options:
#gcplyr
#growthcurvalyzer
#gcanalyzer
#gcer

#TODO:
#     FIXES:
#       fix reordering issues in smoothing
#       There's still a use of t() in make_tidydesign
#       Allow multiple startrow/startcol, etc from a single file in
#        read_blocks, read_wides
#     TESTS:
#       read_blocks and read_wides: change to use temp folder for files
#       smooth_data: algorithms other than moving-average
#       calc_deriv
#       get_window_limits
#       find_next_extrema
#       find_local_extrema
#     FEATURES:
#       vignette section on multiple plates
#       import_blockdesign (Alita would like)
#            if there's just one block, it's not an issue to use
#            the standard pipeline
#            If there's more than one block, we just have to paste them
#            together and then use the standard pipeline
#            And then in tidy format, have a function that will split
#            the column up by the separator
#            1. concat_blocks
#            2. split_tidy's
#       Make way to transform tidy designs to blocks and print them
#        (or save to file) (Cathy would like)
#        trans_tidy_to_wide, trans_wide_to_block
#       Add citation()
#       Allow pattern string to be vector in make_tidydesign
#       Compare to & integrate with other packages, eg growthcurver
#       Add plot option for blocks and wides so that users can check that
#             things worked (have it look like the plot in plate reader software)
#       Figure out how to call xlsx in remote building
#             (e.g. like that of devtools::check_win_devel(),
#             see: https://community.rstudio.com/t/error-regarding-manual-from-devtools-check-r-cmd-check/14227/2)
#       add capability for make_tidydesign to accept simple wellnumbers
#       Add functions to smooth_data: LOWESS, spline models
#       Improve efficiency of find_local_extrema

#General pipeline:
# 1.  import_blockmeasures OR
#       (read_blocks -> uninterleave (optional) -> widen_blocks)
#     import_widemeasures
# 2.  pivot_wide_longer
# 3.  make_tidydesign OR
#     import_blockdesign OR
#     use plater::read_plate with ONLY the design elements
# 4.  merge_tidydesign_tidymeasures
# 5.  [pre-process: smoothing, normalization]
# 6.  [process: derivs, peak-finding, curve fitting]