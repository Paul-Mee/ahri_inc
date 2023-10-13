#' @title  Function for splitting data into episodes. 
#' @description  Function for splitting data into episodes. 
#' @param  dat Dataset must have variables called obs_start, obs_end and event.
#' @param  years The years by which to split the data. Default is c(2003:2020).
#' @return data.frame
#' @importFrom survival survSplit Surv
#' @export 

splitData2 <- function(
  dat, years=NULL) {
  if (is.null(years))
    years <- c(2003:2023)
  edat <- survival::survSplit(Surv(
    time=as.integer(obs_start), 
    time2=as.integer(obs_end), 
    event=event) ~ . , 
    data=dat,
    start="obs_start",
    end="obs_end",
    cut=getYearDates(years))
  vars <- c("obs_start", "obs_end")
  edat[vars] <- lapply(edat[vars], 
    function(x) as.Date(x, origin="1970-01-01"))
  edat <- mutate(edat, 
    Year=as.integer(format(.data$obs_start, "%Y")))
  tibble::as_tibble(edat)
}


#' @title Get the numeric version of date for 01JanXXXX.
#' 
#' @description  gets numeric dates for 01JanXXXX.
#' 
#' @param  Years The years by which to split the data, for example, `c(2005:2019)`.
#' 
#' @return vector
#'
#' @export 

getYearDates <- function(Years) {
  sapply(Years, function(x)
    as.numeric(as.Date(paste0(x, "-01-01"))))
}

#' @title Split data at the imputed seroconversion date.
#' 
#' @description Split data at the imputed seroconversion date.
#' 
#' @param dat a dataset
#' @param splitYears Vector from \code{\link{getYearDates}}.
#' 
#' @return data.frame 
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(Years=c(2008:2020))
#' hiv <- setHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' splitAtSeroDate(sdat, splitYears=Args$Years)

splitAtSeroDate <- function(
  dat=NULL,  splitYears=NULL) {
  dat <- dplyr::rename(dat, event = .data$sero_event)
  dat <- dplyr::mutate(dat, obs_end=ifelse(.data$event==1, .data$sero_date, .data$late_neg))
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(.data$obs_end - .data$obs_start))
  if(any(edat$Time>366)) stop("Days in Year > 366")
  edat <- rename(edat, sero_event = .data$event)
  tibble::as_tibble(edat)
}

#' @title Split data at the earliest HIV-positive date. 
#' 
#' @description Split data at the earliest HIV-positive date. 
#' 
#' @param dat a dataset
#' @param splitYears Vector from \code{\link{getYearDates}}.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(Years=c(2008:2019))
#' hiv <- setHIV(Args)
#' rtdat <- getRTData(hiv)
#' splitAtEarlyPos(rtdat, splitYears=Args$Years)

splitAtEarlyPos <- function(
  dat=NULL,  splitYears=NULL) {
  dat <- mutate(dat, obs_end=ifelse(.data$sero_event==1, .data$early_pos, .data$late_neg))
  dat <- rename(dat, event = .data$sero_event)
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(.data$obs_end - .data$obs_start))
  if(any(edat$Time > 366)) stop("Days in Year > 366")
  edat <- rename(edat, sero_event = .data$event)
  tibble::as_tibble(edat)
}

