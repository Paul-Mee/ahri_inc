#' @title Set arguments and parameters to control functions and data
#' transformations.
#' @description  A function that collects and stores various arguments that can be used as
#' inputs to other functions. The \code{\link{setArgs}} function comes with default parameter
#' values for the arguments. The user is asked to check these values and understand how
#' these will affect the data. 
#' @param Years numeric vector of years.
#' @param Age list of ages as in \code{list(Fem=c(15, 49), Mal=c(15, 54)}.
#' @param AgeCat numeric vector of age categories, e.g. \code{seq(15, 55, by=5)}.
#' @param AgeBy integer value for size age categories.
#' @param nSim number of simulations to perform.
#' @param aname String to name an object or set a filename.
#' @param imputeMethod select either \code{\link{imputeRandomPoint}} or
#' \code{\link{imputeMidPoint}}.
#' @param mcores number of cores to use for parallel package (used in
#' \code{\link{MIdata}}). This is not available for Windows users apparently.
#' @param MoreArgs a list, which adds more arguments if needed.
#' @param setFun a function used by \code{\link{setData}} by which the data can be further subset.
#' @param addVars a function used by \code{\link{setData}} by which additional variables
#' can be added. 
#' @return list
#' @export
#' @examples
#' # Check default parameter values
#' Args = setArgs()
#' Args 
#' # Set specific age and year ranges 
#' Args <- setArgs(Years=c(2008:2018), Age=list(Mal=c(15, 54), Fem=c(15, 49)))
#' setHIV(Args)
#' # Keep all ages
#' ArgsEpi <- setArgs(Age=list(All=c(0, 100)), AgeCat=seq(0, 100, 20))
#' setEpisodes(ArgsEpi)

setArgs <- function( 
  Years = c(2000:2025),
  Age = list(All = c(0, 100)),
  AgeCat = NULL,
  AgeBy = 5,
  nSim = 1, 
  imputeMethod = imputeRandomPoint,
  aname = 'filename',
  mcores = 1,
  setFun = identity,
  addVars = identity,
  MoreArgs = NULL) {
  #
  if (is.null(AgeCat)) {
    AgeCat=seq(min(unlist(Age)),
      max(unlist(Age)) + AgeBy, AgeBy)
  }
  Sex <- ifelse(
    setequal(names(Age), c("Mal", "Fem")), 
    "All", names(Age))
  FemCode = switch(Sex,
    Mal = 0, Fem = 1, All = c(0, 1))
  as.list(environment())
}

