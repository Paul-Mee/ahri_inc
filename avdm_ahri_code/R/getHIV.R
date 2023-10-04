#' @title Reads the standard HIV .dta file into R.
#' 
#' @description  Reads in AHRI data from standard HIV .dta file
#' 
#' @param inFile File path to .dta, default is set by \code{\link{setFiles}}.
#' @param dropTasP Default is to drop TasP surveillance areas from the data. 
#' @param addVars A regular expression string representing the variables to be added. 
#' @param drop15Less Default is to drop all observations with Age < 15 years. Only
#' participants >=15 years eligible for HIV testing.
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 
#' @examples
#' # Writes .Rda to file
#' \donttest{
#' readHIVData(write_rda = TRUE)
#' }
#' # Drop TasP areas, dont write to file
#' readHIVData(dropTasP = FALSE, write_rda = TRUE)
#' # Saves to global environment 
#' hdat <- readHIVData(write_rda=FALSE)
#' # Add variables
#' hdat <- readHIVData(addVars="HIVRefused|WhereLastTested", write_rda=FALSE)

readHIVData <- function(inFile=NULL, 
  dropTasP=TRUE, addVars=" ", drop15Less=TRUE, write_rda=TRUE) {
  #
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$hivfile
  }
  #
  hiv <- haven::read_dta(inFile) %>% 
    select(IIntID = .data$IIntId, 
      BSIntID = .data$ResidencyBSIntId, .data$VisitDate, 
      .data$HIVResult, Female = .data$Sex, Age = .data$AgeAtVisit,
      matches(addVars))
  hiv <- haven::zap_labels(hiv)
  if (dropTasP) hiv <- dropTasPData(hiv)
  hiv <- filter(hiv, .data$Female %in% c(1,2))
  # Only deal with valid test results
  hiv <- filter(hiv, .data$HIVResult %in% c(0,1))
  if (drop15Less) hiv <- filter(hiv, .data$Age %in% c(15:100))
  hiv <- mutate(hiv,
    IIntID = as.integer(.data$IIntID),
    BSIntID = as.integer(.data$BSIntID),
    Female = as.integer(ifelse(.data$Female==2, 1, 0)),
    Year = as.integer(format(.data$VisitDate, "%Y")),
    HIVNegative = ifelse(.data$HIVResult==0, .data$VisitDate, NA), 
    HIVPositive = ifelse(.data$HIVResult==1, .data$VisitDate, NA))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  hiv <- arrange(hiv, .data$IIntID, .data$VisitDate)
  if (write_rda) {
    check_getFiles()
    saveRDS(hiv, file = getFiles()$hiv_rda)
  }
  return(hiv)
}

#' @title Loads the standard HIV dataset into R.
#' @description  Load in the .rda version of the HIV surveillance dataset. 
#' @param inFile File path to .rda dataset, default is \code{getFiles()$hiv_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#' @return data.frame
#' @export 

getHIV <- function(inFile=NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$hiv_rda
  }
  readRDS(file=inFile)
}

#' @title Subsets the HIV dataset according to user arguments.
#' @description set HIV data by arguments.
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param dat A dataset generated from \code{\link{readHIVData}}, which exists in the
#' global environment. If NULL, it reads in the .Rda file from
#' \code{getFiles()$hiv_rda}.  
#' @return data.frame
#' @export 
#' @examples
#' Args <- setArgs() 
#' set_hiv <- setHIV(Args)
#' # Pass in existing data as an argument
#' hdat <- readHIVData(write_rda=FALSE)
#' hdat1 <- setHIV(Args, dat=hdat)
setHIV <- function(Args=setArgs(), dat=NULL) {
  if (is.null(dat)) dat <- getHIV()
  setData(dat, Args)
}

#' @title Creates a dataset for HIV repeat-testers.
#' @description  Get all repeat testers from HIV surveillance.
#' @param dat dataset from \code{\link{getHIV}}. 
#' @param onlyRT Drops IDs who are not repeat-testers.
#' @return data.frame
#' @import dplyr
#' @export
#' @examples 
#' rtdat <- getRTData(dat=getHIV())

getRTData <- function(dat=NULL, onlyRT=TRUE) {
  if (is.null(dat)) dat <- getHIV()
  early_neg <- getDatesMin(dat, "HIVNegative", "early_neg")
  early_pos <- getDatesMin(dat, "HIVPositive", "early_pos")
  late_neg <- getDatesMax(dat, "HIVNegative", "late_neg")
  late_pos <- getDatesMax(dat, "HIVPositive", "late_pos")
  dat <- distinct(dat, .data$IIntID, .data$Female)
  dat <- suppressMessages(Reduce(left_join, 
    list(dat, early_neg, late_neg, early_pos, late_pos)))

  rtdat <- mutate(dat, late_neg_after = ifelse(
    (.data$late_neg > .data$early_pos) &
      is.finite(.data$early_pos) & is.finite(.data$late_neg), 1, 0)) 
  # I just drop these individuals, irreconcilable
  rtdat <- filter(rtdat, .data$late_neg_after==0) %>% 
    select(-c(.data$late_neg_after, .data$late_pos))
  if (onlyRT) {
    # Drop any indiv that dont have a first neg date.
    rtdat <- filter(rtdat, !(is.na(.data$early_neg) & is.na(.data$late_neg)))
    # Must have two tests, if early neg date is equal to late neg date and missing pos date then drop
    rtdat <- filter(rtdat, !(.data$early_neg==.data$late_neg & is.na(.data$early_pos)))
  }
  rtdat <- mutate(rtdat, sero_event = ifelse(is.finite(.data$early_pos), 1, 0))
  rtdat <- rename(rtdat, obs_start = .data$early_neg)
  rtdat
}

#' @title getDates
#' 
#' @description Function to get earliest/latest test dates
#' 
#' @param  f a function, either \code{min} or \code{max}.
#' 
#' @return data.frame
#' @keywords internal
getDates <- function(f) {
  function(dat, Var, Name) {
    dat <- data.frame(dat[!is.na(dat[, Var, drop=TRUE]), c("IIntID", Var)])
    dates <- tapply(dat[, Var], dat[, "IIntID"], f)
    out <- data.frame(as.integer(names(dates)), dates)
    colnames(out) <- c("IIntID", Name)
    out[, Name] <- as.Date(out[, Name], origin="1970-01-01")
    tibble::as_tibble(out)
  }
}

#' @title Function to get earliest HIV test dates.
#' 
#' @description Function to get earliest test dates
#' 
#' @param  dat a dataset.
#' @param  Var a variable name.
#' @param  Name new variable name.
#' 
#' @return data.frame
#' @export

getDatesMin <- getDates(min)

#' @title Function to get latest HIV test dates
#' 
#' @description Function to get latest test dates
#' 
#' @param  dat a dataset.
#' @param  Var a variable name.
#' @param  Name new variable name.
#' 
#' @return data.frame
#' @export
getDatesMax <- getDates(max)



