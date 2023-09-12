#' @title Set the path and filenames to the standard AHRI datasets. Required. 
#' @description  Set file paths to the default AHRI datasets, which must be placed into a
#' single folder. The names of the default .dta datasets must be exactly the same as
#' described below. The function also sets the names for the .Rda datasets used in other
#' functions.  You should change these names only if you know what you are doing (assume
#' that you don't). The \code{ahri} package will never overwrite the default. dta
#' datasets. You must assign the \code{setFiles} function to a name called
#' \code{getFiles}. See the examples below. 
#' @param folder The path (as a string) to  the folder of default .dta files. If
#' \code{folder=""}, then the default is to call a dialogue box for you to point and click
#' to the folder path.  
#' @param hivfile Reads the file RD05-99 ACDIS HIV All.dta 
#' @param epifile Reads the file SurveillanceEpisodesExtended.dta 
#' @param wghfile Reads the file RD03-99 ACDIS WGH ALL.dta 
#' @param mghfile Reads the file RD04-99 ACDIS MGH ALL.dta
#' @param bsifile Reads the file RD01-03 ACDIS BoundedStructures.dta 
#' @param hiv_rda Writes the file ACDIS_HIV_All.Rda 
#' @param epi_rda Writes the file SurveillanceEpisodesExtended.Rda 
#' @param wgh_rda Writes the file ACDIS_WGH_ALL.Rda
#' @param mgh_rda Writes the file ACDIS_MGH_ALL.Rda 
#' @param bsc_rda Writes the file ACDIS_BoundedStructures.Rda
#' @return function
#' @export
#' @examples 
#' \donttest{
#' # You must assign the setfiles function to the getFiles name 
#' getFiles <- setFiles(folder="Path/to/my/datafolder")
#' # If for some reason your HIV surveillance .dta file is named differently 
#' getFiles <-setFiles(folder="Path/to/my/datafolder", hivfile="RD09-01 PIP HIV All.dta")
#' # print out the default file paths and names
#' setFiles()
#' # print out your file paths and names
#' getFiles <-setFiles(folder="Path/to/my/datafolder", hivfile="RD09-01 PIP HIV All.dta")
#' getFiles()
#' }
setFiles <- function(
  folder = "",
  hivfile = "RD05-99 ACDIS HIV All.dta",
  epifile = "SurveillanceEpisodesYrAge.dta",
  wghfile = "RD03-99 ACDIS WGH ALL.dta",
  mghfile = "RD04-99 ACDIS MGH ALL.dta",
  bsifile = "RD01-03 ACDIS BoundedStructures.dta",

  hiv_rda = "ACDIS_HIV_All.Rda",
  epi_rda = "SurveillanceEpisodes.Rda",
  wgh_rda = "ACDIS_WGH_ALL.Rda",
  mgh_rda = "ACDIS_MGH_ALL.Rda",
  bsc_rda = "ACDIS_BoundedStructures.Rda") {

  if (folder == "") 
    folder <- utils::choose.dir(
      caption = "Select a folder which contains the standard AHRI .dta files") 
  flist <- as.list(environment())
  flist <- lapply(flist, function(x) file.path(folder, x))
  function() 
    return(flist[setdiff(names(flist), "folder")])
}

#' @title check_getFiles
#' @description  Warns user that they did not set getFiles, see \code{\link{setFiles}}.
#' @keywords internal
check_getFiles <- function() {
  if (!exists("getFiles", envir = globalenv()))
    message("The  getFiles function doesn't exist, you need to set it. Type `?setFiles` for help.")
}
