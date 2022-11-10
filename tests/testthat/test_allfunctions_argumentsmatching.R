library(testthat)
library(gcplyr)

test_that("all functions included in gcplyr_function_subfunction_calls.csv", {
  #This file is a manually generated list of all functions in gcplyr
  # (run: getNamespaceExports("gcplyr")[order(getNamespaceExports("gcplyr"))])
  #And all sub-functions called by those functions
  myfile <- read.csv(test_path("allfunctions_argumentsmatching_files", 
            "gcplyr_function_subfunction_calls.csv"))
  
  #Here we validate that the current build has no functions that aren't
  # listed in this file. If there are, they must be manually added to
  # gcplyr_function_subfunction_calls.csv
  expect_equal(unique(myfile$gcplyr_func)[order(unique(myfile$gcplyr_func))], 
               unique(getNamespaceExports("gcplyr"))[
                 order(unique(getNamespaceExports("gcplyr")))])
})

test_that("all full arg matches have been checked", {
  #This file is a manually generated list of all functions in gcplyr
  #And all sub-functions called by those functions
  calls <- read.csv(test_path("allfunctions_argumentsmatching_files", 
                     "gcplyr_function_subfunction_calls.csv"))
  #This file contains a previously generated full_matches file
  # that was manually checked that all partial argument matches
  # would not cause problems
  full_matches_ref <-
    read.csv(test_path("allfunctions_argumentsmatching_files", 
                       "full_matches_ref.csv"))
  
  #Find all full name arg matches of funcs and subfuncs 
  #Loop through every func-subfunc combo
  full_matches <- NULL
  for (i in 1:nrow(calls)) {
    if(!is.na(calls$other_func[i])) {
      #Get the gcplyr super-func
      func1 <- get(calls$gcplyr_func[i])
      #Load library containing sub-func
      library(package = strsplit(calls$other_func[i], "::")[[1]][1], 
              character.only = TRUE)
      #Get sub-func
      func2 <- get(strsplit(calls$other_func[i], "::")[[1]][2])
      
      #Save full matches of argument names to df
      if (is.null(full_matches)) {
        full_matches <- 
          data.frame(
            "gcplyr_func" = calls$gcplyr_func[i],
            "other_func" = calls$other_func[i],
            "matches" = paste(names(formals(func1))[
              names(formals(func1)) %in% names(formals(func2)) &
                names(formals(func1)) != "..."],
              collapse = ", "))
      } else {
        full_matches <- 
          rbind(full_matches,
                data.frame(
                  "gcplyr_func" = calls$gcplyr_func[i],
                  "other_func" = calls$other_func[i],
                  "matches" = paste(names(formals(func1))[
                    names(formals(func1)) %in% names(formals(func2)) &
                      names(formals(func1)) != "..."],
                    collapse = ", ")))
      }
    }
  }
  
  if(F){write.csv(file = "full_matches.csv", x=full_matches, row.names=FALSE)}
  
  #Check that all full name matches have been checked in ref file
  if(all(is.na(full_matches_ref$checked) | full_matches_ref$checked == "x")) {
    full_matches_ref <- full_matches_ref[, 1:3]
    full_matches_ref <- full_matches_ref[
      order(full_matches_ref$gcplyr_func, full_matches_ref$other_func), ]
    row.names(full_matches_ref) <- 1:nrow(full_matches_ref)
    full_matches <- full_matches[
      order(full_matches$gcplyr_func, full_matches$other_func), ]
    row.names(full_matches) <- 1:nrow(full_matches)
    expect_equal(full_matches, full_matches_ref)
  } else {stop("not all full name matches have been manually checked")}
})

test_that("all partial arg matches have been checked", {
  #This file is a manually generated list of all functions in gcplyr
  #And all sub-functions called by those functions
  calls <- read.csv(test_path("allfunctions_argumentsmatching_files", 
                              "gcplyr_function_subfunction_calls.csv"))
  #This file contains a previously generated part_matches file
  # that was manually checked that all partial argument matches
  # would not cause problems
  part_matches_ref <-
    read.csv(test_path("allfunctions_argumentsmatching_files", 
                       "part_matches_ref.csv"))
  
  #Find all partial name arg matches of funcs and subfuncs 
  part_matches <- NULL
  for (i in 1:nrow(calls)) {
    if(!is.na(calls$other_func[i])) {
      #Get the gcplyr super-func
      func1 <- get(calls$gcplyr_func[i])
      #Load library containing sub-func
      library(package = strsplit(calls$other_func[i], "::")[[1]][1], 
              character.only = TRUE)
      #Get sub-func
      func2 <- get(strsplit(calls$other_func[i], "::")[[1]][2])
      
      #Save partial matches of argument names to df
      for (func1_arg in names(formals(func1))) {
        if(func1_arg != "...") {
          charmatching <- charmatch(names(formals(func2)), func1_arg)
          charmatching[names(formals(func2)) == func1_arg] <- NA
          if(any(!is.na(charmatching) & charmatching == 1)) {
            if(is.null(part_matches)) {
              part_matches <- data.frame(
                "gcplyr_func" = calls$gcplyr_func[i],
                "other_func" = calls$other_func[i],
                "gcplyr_arg" = func1_arg,
                "other_args" = paste(
                  names(formals(func2))[which(charmatching == 1)], collapse = ","))
            } else {
              part_matches <- rbind(
                part_matches,
                data.frame(
                  "gcplyr_func" = calls$gcplyr_func[i],
                  "other_func" = calls$other_func[i],
                  "gcplyr_arg" = func1_arg,
                  "other_args" = 
                    paste(names(formals(func2))[which(charmatching == 1)], 
                          collapse = ",")))
            }
          }
        }
      }
    }
  }
  
  if(F){write.csv(file = "part_matches.csv", x=part_matches, row.names=FALSE)}
  
  #Check that all partial name matches have been checked in ref file
  if(all(is.na(part_matches_ref$checked) | part_matches_ref$checked == "x")) {
    part_matches_ref <- part_matches_ref[, 1:4]
    part_matches_ref <- part_matches_ref[
      order(part_matches_ref$gcplyr_func, part_matches_ref$other_func), ]
    row.names(part_matches_ref) <- 1:nrow(part_matches_ref)
    part_matches <- part_matches[
      order(part_matches$gcplyr_func, part_matches$other_func), ]
    row.names(part_matches) <- 1:nrow(part_matches)
    expect_equal(part_matches, part_matches_ref)
  } else {stop("not all partial name matches have been manually checked")}
})