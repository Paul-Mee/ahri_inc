#' @title readHealthData
#' 
#' @description  Reads the Women (WGH) or Men (MGH) General Health Dataset. 
#' 
#' @param Female Either a value of 0 to read in Men or 1 to read in Women.
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#' @keywords internal
readHealthData <- function(Female=1, write_rda=TRUE) {
  warning("This function is deprecated. Use readMGH() or readWGH() instead.")
  # check_getFiles()
  # inFile <- ifelse(Female==0, getFiles()$mghfile, getFiles()$wghfile)
  # outFile <- ifelse(Female==0, getFiles()$mgh_rda, getFiles()$wgh_rda)
  # dat <- haven::read_dta(inFile) %>%
  #   rename(IIntID=IIntId, BSIntID=ResidenceBSIntId, Age=AgeAtVisit)
  # dat <- mutate(dat, 
  #   Year = as.integer(format(dat$VisitDate, "%Y")),
  #   IIntID = as.integer(IIntID),
  #   Female = Female)
  # if (write_rda) saveRDS(dat, file=outFile) 
  # dat
}

#' @title  Reads the standard Men (MGH) General Health .dta dataset into R. 
#' 
#' @description  Reads the Men (MGH) General Health Dataset. 
#' 
#' @param inFile Filepath to .dta dataset, default is \code{getFiles()$mghfile}. 
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' readMGH(write_rda = FALSE)
readMGH <- function(inFile = NULL, write_rda=TRUE) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile <- getFiles()$mghfile
  }
  dat <- haven::read_dta(inFile) %>%
    rename(IIntID=.data$IIntId, 
     BSIntID=.data$ResidenceBSIntId, Age=.data$AgeAtVisit)
  dat <- mutate(dat, 
    Year = as.integer(format(.data$VisitDate, "%Y")),
    IIntID = as.integer(.data$IIntID), Female = 0)
  if (write_rda) {
    check_getFiles()
    saveRDS(dat, file = getFiles()$mgh_rda) 
  }
  dat
}


#' @title Reads the standard Women's  General Health (WGH) .dta dataset into R.
#' 
#' @description  Reads the Women's  General Health Dataset (WGH).
#' 
#' @param inFile Filepath to .dta dataset, default is \code{getFiles()$wghfile}. 
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' readWGH(write_rda = FALSE)
readWGH <- function(inFile = NULL, write_rda=TRUE) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile <- getFiles()$wghfile
  }
  dat <- haven::read_dta(inFile) %>%
    rename(IIntID=.data$IIntId, BSIntID=.data$ResidenceBSIntId, Age=.data$AgeAtVisit)
  dat <- mutate(dat, 
    Year = as.integer(format(.data$VisitDate, "%Y")),
    IIntID = as.integer(.data$IIntID), Female = 1)
  if (write_rda) {
    check_getFiles()
    saveRDS(dat, file = getFiles()$wgh_rda) 
  }
  dat
}

#' @title Loads the Men's general health (MGH) dataframe into memory. 
#' 
#' @description  Loads in the Men's general health data. 
#' 
#' @param inFile Filepath to .rda dataset, default is \code{getFiles()$mgh_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#' @return  data.frame
#'
#' @export 
getMGH <- function(inFile=NULL) {
  if(is.null(inFile)) {
    check_getFiles()
    inFile <- getFiles()$mgh_rda
  }
  readRDS(inFile)
}

#' @title Loads the Women's general health (WGH) dataframe into memory.  
#' 
#' @description  Reads in Women's general health data. 
#' 
#' @param inFile Filepath to .rda dataset, default is \code{getFiles()$wgh_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#' 
#' @return data.frame
#'
#' @export 
getWGH <- function(inFile=NULL) {
  if(is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$wgh_rda
  }
  readRDS(inFile)
}


#' @title Gets circumcision data from the MGH dataset.
#' 
#' @description  Gets circumcision data from the MGH dataset with the year of
#' circumcision and an indicator of ever circumcised.
#' @param dat A dataset from \code{\link{getMGH}}.
#' @export
#' @examples
#' getCircumcisionData(dat = getMGH())
getCircumcisionData <- function(dat=getMGH()) {
  dat <- dplyr::filter(dat, .data$IsCircumcised %in% c(1, 2))
  dat <- dplyr::mutate(dat, IsCircumcised=as.numeric(.data$IsCircumcised==1))
  dat <- dplyr::filter(dat, .data$IsCircumcised==1)
  dat <- group_by(dat, .data$IIntID) %>% 
    summarize(YearCircum = min(.data$Year)) %>% 
      mutate(EverCircum=1)
  dat
}


#' @title  Adds the ever circumcised variable to an existing dataset. 
#' 
#' @description  Adds the ever circumcised variable to an existing dataset. 
#' 
#' @param dat An existing dataset.
#' @param cdat Circumcision dataset from \code{\link{getCircumcisionData}}. 
#' 
#' @return data.frame
#'
#' @export 
addCircumVar <- function(dat, cdat=NULL) {
  if (is.null(cdat)) cdat <- getCircumcisionData()
  cdat <- rename(cdat, Year = YearCircum)
  dat <- left_join(dat, cdat, by=c("IIntID", "Year"))
  dat <- group_by(dat, .data$IIntID) %>% mutate( 
    EverCircum=zoo::na.locf(.data$EverCircum, na.rm=FALSE))
  dat$EverCircum[is.na(dat$EverCircum)] <- 0
  dat
}


#' @title Get variable for ever used condom status from Men and Women's health datasets.
#' 
#' @description  Gets the ever used condom variable from Men and Women's health datasets.

getCondomUseVar <- function() {
  stop("This function is deprecated and no longer maintained.")
  # wdat <- getWGH() %>% 
  #   select(IIntID, Female,  Year, EverUsedCondom = MRPEverUsedCondoms )
  # mdat <- getMGH() %>% 
  #   select(IIntID, Female, Year, EverUsedCondom = MRPEverUsedCondoms )
  # dat <- rbind(mdat, wdat)
  # dat <- filter(dat, EverUsedCondom %in% c(1:3))
  # dat$EverUsedCondom <- as.character(haven::as_factor(dat$EverUsedCondom)) 
  # dat <- distinct(dat, IIntID, Year, .keep_all=TRUE)
  # dat
}


