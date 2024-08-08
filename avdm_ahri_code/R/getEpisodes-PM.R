#' @title Read in the standard Surveillance .dta dataset.
#' @description  Reads in the Surveillance Episodes dataset. Note, this function drops
#' participants with missing values for \code{Sex} (there are a handful of these).
#' @param inFile File path to the .dta dataset, default is set to \code{\link{setFiles}}.
#' @param dropTasP default is to drop TasP areas.
#' @param addVars A regular expression string representing the variables to be added. 
#' @param write_rda Default is to write the .Rda file.
#' @return data.frame
#' @import dplyr
#' @export 
#' @examples
#' \donttest{
#' readEpisodes(addVars="CurrentlyEmployed|UnEmployment")
#' readEpisodes(dropTasP=FALSE, addVars="Employ")
#' }
readEpisodes <- function(
  inFile=NULL,
  dropTasP=TRUE, addVars=" ",
  write_rda=TRUE) {
  #
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$epifile
  }
  message(sprintf("ahri: Reading %s, this may take a while...", inFile))
  dat <- haven::read_dta(inFile)
  # Variable names changed from releases
  # if ("CalendarYear" %in% names(dat)) {
  #   message("ahri: Renaming CalendarYear to Year")
  #   names(dat)[names(dat) == "CalendarYear"] <- "Year"
  # } 
  # if ("ARTStartedDate" %in% names(dat)) { 
  #   message("ahri: Renaming ARTStartedDate to EarliestARTInitDate")
  #   names(dat)[names(dat)=="ARTStartedDate"] <- "EarliestARTInitDate"
  # }
  # if ("IIntId" %in% names(dat)) {
  #   message("ahri: Renaming IIntId to IndividualId")
  #   names(dat)[names(dat)=="IIntId"] <- "IndividualId"
  # }
  dat <- dplyr::select(dat,
    IIntID=IIntId, BSIntID=LocationId, 
    Female=Sex, 
    #Age, 
    DoB, DoD,
    #Year, 
    ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    InMigration, OutMigration,
    Resident, 
    # Not present in file
    #  AssetIndex=ModerntAssetIdx,
    # OnART, EarliestARTInitDate, 
    matches(addVars))
  dat <- filter(dat, Female %in% c(1,2))
  dat <- mutate(dat,
    IIntID=as.integer(IIntID),
    BSIntID=as.integer(BSIntID),
    #Year=as.integer(Year),
    Female=as.integer(ifelse(Female==2, 1, 0)))
  if (dropTasP==TRUE) dat <- dropTasPData(dat)
  dat <- arrange(dat, IIntID, ObservationStart)
  if (write_rda) {
    check_getFiles()
    saveRDS(dat, file = getFiles()$epi_rda)
  }
  dat
}

#' @title Load the standard Surveillance Episodes dataframe into memory. 
#' @description  Loads Episodes .Rda into memory, see \code{\link{readEpisodes}}.
#' @param inFile File path to the dataset, default is set to \code{getFiles()$epi_rda}.
#' @return data.frame
#' @export 
getEpisodes <- function(inFile=NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$epi_rda
  }
  readRDS(inFile)
}


#' @title Subset the Surveillance Episodes dataframe by user arguments. 
#' @description  Set the Episodes data according to Arguments.
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param dat A dataset generated from \code{\link{readEpisodes}}, which exists in the
#' global environment. If NULL, it reads in the corresponding .Rda file (see
#' \code{\link{setFiles}}.  
#' @return data.frame
#' @export 
#' @examples
#' Args <- setArgs(Years=c(2005:2010))
#' setEpisodes(Args)
#' \donttest{
#' epidat <- readEpisodes(write_rda=FALSE)
#' setEpisodes(Args, epidat)
#' }
setEpisodes <- function(Args=setArgs(), dat=NULL) {
  if (is.null(dat)) dat <- getEpisodes()
  setData(dat, Args)
}


#' @title Makes a variable of the proportion of time that a participant spent
#' as a resident in the PIP surveillance area by year.
#' @description Makes a variable of the proportion of time that a participant spent as a
#' resident in the PIP surveillance arear by year. 
#' @param Args requires Args, see \code{\link{setArgs}}. 
#' @return  data.frame
#' @import dplyr
#' @export 
#' @examples
#' dat <- setEpisodes(setArgs()) 
#' adat <- makePropRes(setArgs())
#' dat <- dplyr::left_join(dat, adat, by=c("IIntID", "Year"))
#' dplyr::select(dat, IIntID, Year, ExpDays, Resident, PropRes)

makePropRes <- function(Args) {
  dat <- setEpisodes(Args) 
  ddat <- dplyr::select(dat, IIntID, Year,
    Resident, ExpDays) 
  ddat <- distinct(ddat, IIntID, Year)
  gdat <- filter(dat, Resident==1)
  gdat <- group_by(gdat, IIntID, Year) %>% 
    summarize(DaysIn = sum(ExpDays)) %>% ungroup()
  adat <- left_join(ddat, gdat, by=c("IIntID", "Year"))
  adat$DaysIn[is.na(adat$DaysIn)] <- 0
  adat <- mutate(adat, PropRes=round(DaysIn/366, 3)) %>% 
    dplyr::select(IIntID, Year, PropRes)
  # adat <- filter(adat, .adata$PropRes>=Prop)
  adat
}
