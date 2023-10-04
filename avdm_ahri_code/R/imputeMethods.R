#' @title  Impute a mid-point seroconversion date within the censored interval.
#' 
#' @description  Impute a mid-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData}}. 
#'
#' @return data.frame
#'
#' @export

imputeMidPoint <- function(dat) {
  dat$sero_date <-  
    (as.numeric(dat$late_neg) + as.numeric(dat$early_pos))/2
  dat$sero_date <- as.Date(dat$sero_date, origin='1970-01-01') 
  tibble::as_tibble(dat)
}

#' @title  Impute random seroconversion date(s) within the censored interval.
#' 
#' @description  Impute random seroconversion date(s) within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData}}. 
#'
#' @return data.frame
#'
#' @export

imputeRandomPoint <- function(dat) {
  idat <- split(dat, as.factor(dat$IIntID))
  Fun1 <- function(idat) {
    if (is.na(idat$early_pos)) {
      with(idat, cbind(IIntID, sero_date=NA))
    } else {
      if (idat$early_pos - idat$late_neg <= 1) {
        rpoint <- idat$early_pos 
      } else {
        rpoint <- sample((idat$late_neg + 1):idat$early_pos, 1)
        with(idat, cbind(IIntID, sero_date=rpoint)) 
      }
    }
  }
  sdat <- lapply(idat, Fun1)
  sdat <- data.frame(do.call("rbind", sdat ))
  dat <- merge(dat, sdat, by="IIntID", all.x=TRUE)
  dat$sero_date <- as.Date(dat$sero_date, origin = "1970-01-01")
  tibble::as_tibble(dat)
}

#' @title  Impute a end-point seroconversion date within the censored interval.
#' 
#' @description  Impute a end-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData}}. 
#'
#' @return data.frame
#'
#' @export

imputeEndPoint <- function(dat) {
  dat$sero_date <- dat$early_pos
  tibble::as_tibble(dat)
}

