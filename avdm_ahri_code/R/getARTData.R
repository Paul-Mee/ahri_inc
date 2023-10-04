#' @title  Get ART initiation dates from Surveillance Episodes dataset. 
#' 
#' @description  Get ART initiation dates
#' 
#' @param dat A dataset which has a variable called \code{EarliestARTInitDate}. If
#' NULL, the function calls \code{\link{getEpisodes}} with that variable name.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' @examples
#' getARTDates()
getARTDates <- function(dat=getEpisodes()) {
  dat <- dplyr::filter(dat, !is.na(.data$EarliestARTInitDate))
  dat <- dplyr::distinct(dat, .data$IIntID, .data$EarliestARTInitDate, .keep_all=TRUE) %>% 
    dplyr::select(.data$IIntID, DateOfInitiation=.data$EarliestARTInitDate)
  dat <- dplyr::mutate(dat,
    YearOfInitiation = as.integer(format(.data$DateOfInitiation, "%Y")),
    MonthART = as.integer(format(.data$DateOfInitiation, "%m")))
  dat
}


#' @title  Creates an Ever on ART variable.
#' 
#' @description  Creates an Ever on ART variable.
#' 
#' @param dat A dataset from \code{\link{getEpisodes}}.
#' 
#' @return data.frame
#' @import dplyr
#' @export 
#' @examples
#' getEverART()

getEverART <- function(dat=getEpisodes()) {
  early_pos  <- getDatesMax(dat=getHIV(), "HIVPositive", "EarliestHIVPos")
  dat <- left_join(dat, early_pos, by = "IIntID")
  dat <- filter(dat, !is.na(.data$EarliestHIVPos))
  dat <- select(dat, .data$IIntID,.data$Year, .data$Age, .data$Female,
    DateOfInitiation=.data$EarliestARTInitDate, .data$EarliestHIVPos, .data$OnART)
  dat <- mutate(dat, YearPos = as.integer(format(.data$EarliestHIVPos, "%Y")),
    YearOfInitiation = as.integer(format(.data$DateOfInitiation, "%Y")))
  dat <- filter(dat, !(.data$Year < .data$YearPos))
  dat <- mutate(dat,
    EverART = as.integer(!(.data$Year < .data$YearOfInitiation | is.na(.data$YearOfInitiation))))
  dat
}

#' @title calcARTCov
#' @description  Calculate ART coverage for AHRI data. (This is a very crude measure of
#' ART coverage. More work needed on an appropriate measure.)
#' @param dat A dataset from \code{\link{getEverART}}.
#' @param Args requires Args, see \code{\link{setArgs}}
#' @param Formula A formula using R formula language to get ART estimates by group,
#' default is by year \code{"EvertART ~ Year"}
#' @return data.frame
#' @keywords internal
#' @examples
#' \donttest{
#' calcARTCov(Formula = "EverART ~ Year + Female + AgeCat")
#' }
calcARTCov <- function(dat=getEverART(), Args=setArgs(), 
  Formula = "EverART ~ Year") {
  stop("This function is deprecated and no longer maintained")
  # dat <- setData(dat, Args)
  # calcTrendYear(Formula = Formula, dat)
}


#' @title readARTCov
#' 
#' @description  Read in community ART coverage data from Source path
#' 
#' @param Female Read female data (Female = 1) or male data (Female = 0)
#' 
#' @return data.frame
#' @keywords internal

readARTCov <- function(Female=1) {
  stop("This function is deprecated and no longer maintained")
  # sex <- ifelse(Female==1, "fem_art", "mal_art")
  # art <- suppressMessages(read_csv(
  #   unlist(getFiles()[sex]), na=c("", "-")))
  # art <- tidyr::gather(art, Year, ARTCov, -BSIntID)
  # art <- dplyr::filter(art, !is.na(.data$ARTCov))
  # art <- suppressWarnings(mutate(art, 
  #   Year=as.integer(gsub("[MF]_ART_|All_ART_", "", .data$Year)),
  #   ARTCov = as.numeric(.data$ARTCov)*100))
  # art
}


#' @title addARTCov
#' 
#' @description  Add community ART coverage. 
#' 
#' @param dat Dataset to add ART vars to. 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param oppSex Make opposite-sex HIV prevalence. Default is FALSE.
#' 
#' @return data.frame
#' @keywords internal
addARTCov <- function(dat, Args, oppSex=FALSE) {
  stop("This function is deprecated and no longer maintained")
  # Sex <- ifelse(oppSex, as.numeric(!Args$Fem), Args$Fem)
  # art <- readARTCov(Sex)
  # dat <- left_join(dat, art, by=c("BSIntID", "Year"))
  # dat
}


