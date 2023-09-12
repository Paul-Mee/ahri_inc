test_func <- function(testfile) {
  function(year="2020", report='progress') {
    message(sprintf('Running tests on %s data release...', year))
    testthat::test_package('ahri', filter=paste0(testfile, year), reporter=report)
  }
}

#' @title test_ahri_dta
#' 
#' @description  Tests to see if you are using the correct default .dta datasets. The
#' tests will generate summary values of selected variables from the datasets and check if they match
#' expected values defined in the test. 
#' 
#' @param year The release year that you want to perform the tests on. Currently, tests
#' include 2019 and 2020 release years only.
#' @param report How to display results, can be "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_dta <- test_func('ahri_dta_')

#' @title test_ahri_set
#' 
#' @description  Tests to see if your version of the \code{ahri} functions will produce
#' the correct results.  The tests will generate summary values of selected variables in
#' several datasets and check if they match expected values defined in the test. 
#' 
#' @param year The release year that you want to perform the tests on. Currently, tests
#' include 2019 and 2020 release years only.  
#' @param report How to display results, can be
#' "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_set <- test_func('ahri_set_')

#' @title test_ahri_hiv
#' 
#' @description  Tests to see if the range of HIV incidence functions produce
#' the correct results.  The tests will generate summary values of selected variables in
#' several datasets and check if they match expected values defined in the test. 
#' 
#' @param year The release year that you want to perform the tests on. Currently, tests
#' include 2019 and 2020 release years only.  
#' @param report How to display results, can be
#' "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_hiv <- test_func('ahri_hiv_')

#' @title check_var
#' 
#' @description   Check if the variable name exists
#' 
#' @param dat A dataset 
#' @param var The variable
#' @keywords internal
check_var <- function(dat, var) {
  if (!(var %in% names(dat)))
    stop(sprintf("dat must have a variable named %s",  var))
}
