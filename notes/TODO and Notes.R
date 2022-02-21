##Name options:
#gcplyr
#growthcurvalyzer
#gcanalyzer
#gcer

#TODO: 
#       Ensure that metadata in read_blocks and read_wides can be specified
#             with excel-style letters
#       Figure out how to call xlsx in remote building
#             (e.g. like that of devtools::check_win_devel(),
#             see: https://community.rstudio.com/t/error-regarding-manual-from-devtools-check-r-cmd-check/14227/2)
#       Figure out what's wrong with my local Latex installation
#       Change header results so first row becomes header (not row before)
#       There's still a use of t() in make_tidydesign
#       Write tests for blocken_wides 
#       Finish import_blockdesign functions
#       Write shorthand functions with presets for find_local_extrema
#       Use letter-number naming for wells by default (bc it's what other
#        software uses and we should be consistent at least)
#       Accept letter-number naming for rows/cols when reading in data
#        (bc it's what users will see if they look in Excel for row/cols)
#       Compare setup to other packages for similar analyses, eg growthcurver
#       in growthcurver they keep the timestamps in a column named "time",
#         I should follow a similar convention
#         This is especially useful if there are multiple "variable" type
#           columns (e.g. Temperature) that are also output and that
#           the users want to keep (because rownames can only include
#           one thing)
#       add support for providing only startcol and startrow, with automatic
#         inference that the rest of the dataframe is data
#       Improve comments/documentation
#       
#       make_tidydesign calls t which calls as.matrix
#       
#       add capability for make_tidydesign to accept simple wellnumbers
#       
#Change smoothing to include other functions
#LOESS (which can simplify to a weighted moving average)
#LOWESS
#Spline models
#General additive model

#wide measures: dataframe with each column corresponding to a single well
#block measures: dataframe where rows and columns match literally to a plate

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