#' @title  Read in the standard Bounded Structures .dta dataset.
#' 
#' @description  Read in Bounded Structures data.
#' 
#' @param inFile filepath to data. 
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' getBSData() 
#' # Same as above
#' getBSData(inFile = getFiles()$bsifile) 
getBSData <- function(inFile = NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile = getFiles()$bsifile
  }
  dat <- haven::read_dta(inFile) %>% rename(BSIntID=.data$BSIntId)
  dat <- dplyr::mutate(dat, BSIntID = as.integer(.data$BSIntID))
  dat$IsUrbanOrRural <- as.character(haven::as_factor(dat$IsUrbanOrRural))
  dat$LocalArea <- as.character(haven::as_factor(dat$LocalArea))
  dat$PIPSA <- as.character(haven::as_factor(dat$PIPSA))
  dat$Isigodi <- as.character(haven::as_factor(dat$Isigodi))
  return(dat)
}

#' @title Drop TasP (Northern Areas) from a dataframe.
#' 
#' @description  Function to drop obervations from TasP areas.
#' 
#' @param dat A dataset, which will be merged with the Bounded Structures
#' dataset, to determine if observations come from the TasP (northern) areas. 
#' If an observation cannot be linked to an area, it is kept.
#' @param bsdat The Bounded Structures dataset with BSIntID and PIPSA variables.
#' @return data.frame
#'
#' @export 
#' @examples
#' # This is what the dropTasP argument does when it is TRUE.
#' hiv <- readHIVData(dropTasP = FALSE)
#' hiv <- dropTasPData(hiv)

dropTasPData <- function(dat, bsdat = NULL) {
  if (is.null(bsdat)) {
    check_getFiles()
    bsdat <- getBSData() %>% dplyr::select(.data$BSIntID, .data$PIPSA)
  }
  dat <- left_join(dat, bsdat, by="BSIntID")
  dat <- filter(dat, .data$PIPSA %in% c("Southern PIPSA", NA)) 
  return(dat)
}

#' @title Gets the BSIntID that IIntID spent most time in a surveillance year. 
#' 
#' @description Gets the BSIntID that IIntID spent most time in a surveillance year. 
#' 
#' @param dat The default is to call \code{\link{getEpisodes}}.
#' @param minDays Value of 1:366 min days spent in DSA to qualify as being a resident in
#' that year. 
#'
#' @return data.frame
#'
#' @export 
#'
#' @examples 
#' edat <- getEpisodes()
#' bs_max <- getBSMax(edat)

getBSMax <- function(dat = getEpisodes(), minDays=0) {

  dat <- dplyr::filter(dat, .data$Resident==1)

  # Identify max expdays per episode
  dat <- dplyr::group_by(dat, .data$IIntID, .data$Year) %>% dplyr::mutate(
    MaxDays = max(.data$ExpDays, na.rm=TRUE))
  dat <- dplyr::ungroup(dat)
  
  dat <- dplyr::filter(dat, .data$MaxDays==.data$ExpDays)

  dat <- dplyr::group_by(dat, .data$IIntID, .data$Year) %>% 
    dplyr::filter(row_number()==1)
  dat <- dplyr::ungroup(dat)

  maxBS <- dplyr::filter(dat, .data$MaxDays >= minDays) %>% 
    dplyr::select(.data$IIntID, .data$Year, .data$BSIntID )

  maxBS
}


#' @title makeMigrVars
#' 
#' @description Make migration variables: the cumulative time spent out the PIP
#' study area and the number of migration events (in and out) by year. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#' @keywords internal 

makeMigrVars <- function(Args) {
  stop("This function is deprecated and no longer maintained")
  # dem <- setEpisodes(Args) %>% 
  #   select(.data$IIntID, .data$BSIntID, .data$Year, .data$Age,
  #     .data$ExpDays, .data$Resident, matches("Migration"))
  # adat <- distinct(dem, .data$IIntID, .data$Year)
  # mdat <- filter(dem, .data$Resident==1)
  # mdat <- group_by(mdat, .data$IIntID, .data$Year) %>% 
  #   summarize(DaysIn=sum(.data$ExpDays)) %>% ungroup()
  # adat <- left_join(adat, mdat, by=c("IIntID", "Year"))
  # adat$DaysIn[is.na(adat$DaysIn)] <- 0
  # adat <- mutate(adat, 
  #   DaysOut = 366 - .data$DaysIn, DayFull = 366)
  # cumtime <- group_by(adat, .data$IIntID) %>% 
  #   mutate(CumDaysOut = cumsum(.data$DaysOut),
  #   CumDays = cumsum(.data$DayFull),
  #   CumTimeOut = round(.data$CumDaysOut/.data$CumDays, 2)) %>% 
  #   ungroup()
  # cumtime <- select(cumtime,.data$IIntID, .data$Year, .data$CumTimeOut)
  # # Count external migr events
  # inmigr <- select(dem, .data$IIntID, .data$Year, .data$InMigration) %>% filter(.data$InMigration==1)
  # inmigr <- group_by(inmigr, .data$IIntID, .data$Year) %>% summarize(InMigr=n()) %>% ungroup()
  # outmigr <- select(dem, .data$IIntID, .data$Year, .data$OutMigration) %>% filter(.data$OutMigration==1)
  # outmigr <- group_by(outmigr, .data$IIntID, .data$Year) %>% summarize(OutMigr=n()) %>% ungroup()
  # migr <- left_join(adat, inmigr, by=c("IIntID", "Year")) %>% select(-.data$DayFull)
  # migr <- left_join(migr, outmigr, by=c("IIntID", "Year"))
  # migr$InMigr[is.na(migr$InMigr)] <- 0
  # migr$OutMigr[is.na(migr$OutMigr)] <- 0
  # migr <- mutate(migr, MigrCount=.data$InMigr+.data$OutMigr) 
  # dat <- left_join(migr, cumtime, by=c("IIntID", "Year"))
  # dat
}




#' @title addMigrVars
#' 
#' @description Adds variables from \code{\link{makeMigrVars}} to an
#' existing dataset. 
#' 
#' @param dat Existing dataset to merge variables into. 
#' @param mdat Dataset of migration variables \code{\link{makeMigrVars}}.
#' @param carry Whether to carry missing values forward and backward after merging. Default
#' is TRUE.
#' 
#' @return data.frame
#' @keywords internal 
#' @examples
#' \donttest{
#' Args <- setArgs()
#' epi <- setEpisodes(Args) 
#' mdat <- makeMigrVars(Args)
#' epi2 <- addMigrVars(epi, mdat)
#' }

addMigrVars <- function(dat, mdat, carry=TRUE) {
  stop("This function is deprecated and no longer maintained")
  # dat <- left_join(dat, mdat, by=c("IIntID", "Year"))
  # dat <- arrange(dat, .data$IIntID, .data$Year)
  # if (carry) {
  # dat <- mutate(dat, 
  #   MigrCount = zoo::na.locf(.data$MigrCount, na.rm=FALSE), 
  #   MigrCount = zoo::na.locf(.data$MigrCount, na.rm=FALSE, fromLast=TRUE),
  #   CumTimeOut = zoo::na.locf(.data$CumTimeOut, na.rm=FALSE), 
  #   CumTimeOut = zoo::na.locf(.data$CumTimeOut, na.rm=FALSE, fromLast=TRUE))
  # }
  # dat <- mutate(dat, CumTimeOut = round(.data$CumTimeOut*100))
  # dat
}


#' @title addHIVPrevBS
#' 
#' @description  add the geospatical HIV prevalence surrounding BS
#' 
#' @param inFile The filepath to the dataset with HIV prevalence variables.
#' @param dat A dataset to add HIV prevalence variables to. 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param Type "Males", "Females", or "All". Add only the Males, Females or All.
#' 
#' @return data.frame
#' @keywords internal 

addHIVPrevBS <- function(inFile, dat, Args, Type="All") {
  stop("This function is deprecated and no longer maintained")
  # prev <- tibble::as_tibble(read.csv(inFile))
  # prev[] <- lapply(prev[], function(x) as.numeric(as.character(x)))

  # # Reshape to long
  # prev <- select(prev, BSIntID, starts_with(Type))
  # long <- tidyr::gather(prev, Year, HIVPrev, starts_with(Type)) 
  # long <- suppressWarnings(mutate(long, 
  #   Year=as.integer(gsub("[^[:digit:]]", "", Year)),
  #   HIVPrev = as.numeric(HIVPrev)*100))

  # dat <- left_join(dat, long, by=c("BSIntID", "Year"))
  # dat <- arrange(dat, BSIntID, Year) 
  # dat <- group_by(dat, BSIntID) %>% mutate(
  #     HIVPrev=zoo::na.locf(HIVPrev, na.rm=FALSE),
  #     HIVPrev=zoo::na.locf(HIVPrev, na.rm=FALSE, fromLast=TRUE))
  # dat$HIVPrev[is.na(dat$HIVPrev)]  <- 
  #   runif(sum(is.na(dat$HIVPrev)), 0, 50)
  # dat
}

