## Resubmission
This is a resubmission. In this version (1.1.0) I have:

* Added a link to the github repo where documentation for the methods in this package is available to the description field of the DESCRIPTION file. There are not yet any published works describing these methods; when there are they will be added to the package DESCRIPTION.

* Added \value for all exported methods (only make_tidydesign was missing one)

* Changed the behavior of write_blocks so that it does not write by default in the user's home filespace. Users must now specify a file name or NULL for the `file` argument (in which case the file name will be inferred from other arguments). No other functions write to files.

* Checked all vignettes and tests, and believe they all only write files to tempdir() or tempfile(). No examples write to files.

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

* This is a new submission to CRAN

## Downstream dependencies
There are currently no downstream dependencies for this package
