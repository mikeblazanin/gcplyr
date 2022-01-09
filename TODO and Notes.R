##Name options:
#gcplyr
#growthcurvalyzer
#gcanalyzer

#TODO:  
#       Write shorthand functions with presets for find_local_extrema
#       Use letter-number naming for wells by default (bc it's what other
#        software uses and we should be consistent at least)
#       Accept letter-number naming for rows/cols when reading in data
#        (bc it's what users will see if they look in Excel for row/cols)
#       actually test & check that functions work
#       Compare setup to other packages for similar analyses, eg growthcurver
#       Get this in package form before the quarantine ends!!!
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
#       widen_blocks calls t which calls as.matrix
#       make_layout also does
#       
#       add capability for make_tidydesign to accept simple wellnumbers
#       
#       'gcer' is available as a package name
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