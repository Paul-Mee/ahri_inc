#' @title  Read the results from the unireg model into dataframe.
#' @description  Read the results from the \code{\link{uniReg}} model into .Rdata
#' format. 
#' @param File File path to \code{\link{uniReg}} output.txt.
#' @export
#' @examples
#' # install.packages(system.file("IntCens_0.2.tar.gz", package = "ahri"), repos = NULL, type = 'source')
#' library(IntCens)
#' data(gdat_demo)
#' home <- file.path('/home/alain/Seafile/AHRI_Data')
#' write.table(gdat_demo, file=file.path(home, "intcens/input_data.txt"),
#'   row.names=FALSE, quote=FALSE)
#' unireg(input = file.path(home, "intcens/input_data.txt"),
#'   output = file.path(home, "intcens/res_dat.txt"),
#'   model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
#'   subject_id = "IIntID", convergence_threshold=0.01, r = 1.0)
#' ureg_res <- readUniRegResults(
#'   File=file.path(home, "intcens/res_dat.txt"))
readUniRegResults <- function(File=NULL) {

  out <- readLines(File)
  out <- gsub("\\t", " ", out)
  out <- out[out!=""]

  # get lines of surv data
  time_ln <- grep("^Time[[:space:]]+Estimate", out)
  surv_dat <- out[(time_ln+1):length(out)] 
  surv_dat <- strsplit(surv_dat, "[[:space:]]+")
  surv_dat <- data.frame(do.call(rbind, surv_dat), stringsAsFactors=FALSE)
  surv_dat[] <- lapply(surv_dat[],  as.numeric)
  colnames(surv_dat) <-  c("Time", "Estimate")

  # Now get estimates into R object
  Cov_ln <- grep("^Covariate", out)
  Cov_ln2 <- grep("[Co]?[vV]ariance", out)
  emat <- out[(Cov_ln+1):(Cov_ln2-1)] 
  emat <- strsplit(emat, "[[:space:]]+")
  emat <- data.frame(do.call(rbind, emat), stringsAsFactors=FALSE)
  emat[, -1] <- lapply(emat[, -1], as.numeric)
  colnames(emat) <-  strsplit(out[Cov_ln], "[[:space:]]+")[[1]]

  # Covariance estimates
  ofset <- ifelse(grepl("^Var", out[Cov_ln2]), 1, 2) # needed
  cmat <- out[(Cov_ln2+ofset):(time_ln-2)] 
  cmat <- gsub("^\\s+|:", "", cmat)
  cmat <- strsplit(cmat, "[[:space:]]+")
  cmat <- data.frame(do.call(rbind, cmat), stringsAsFactors=FALSE)
  cmat[,-1] <- lapply(cmat[,-1], as.numeric)
  nm <- cmat[,1]; cmat <- cmat[,-1, drop=FALSE]
  colnames(cmat) <-  rownames(cmat) <- nm

  list(sdat=surv_dat, edat=emat, cdat=cmat)
}


#' @title  Uses the G-imputation method to impute the HIV sero date times.
#' 
#' @description  Uses the G-imputation method to impute the infection times.
#' 
#' @param dat A dataset of the covaraiate values by individual and time period. \code{dat} must have an ID variable
#' called IIntID, a start date \code{obs_statr}, a latest-negative date \code{late_neg}
#' and earliest-positive date \code{early_pos}. 
#' @param Results Results from \code{\link{readUniRegResults}}.
#' @param nSim The number of imputations to perform.
#' @param start_date If null, start_date is the first obs_start date of ID, else it is
#' same start_date for everyone. Must be a string in the following YYYY-MM-DD format: e.g. "2005-01-23".
#' @param tscale Scalar by which to scale time.  The default is 1, which means
#' that the time-scale is in days. The tscale used must correspond with the time scale of your Time
#' variable. So if Time is in days, \code{tscale=1} else if Time is in months \code{tscale
#' = 30.44}. 
#' @param trans_back If \code{tscale != 1}, transform the infection times back into days.
#' Default is TRUE. 
#' @param mcores The number of cores to use for parallel processing using \code{mclapply}.
#' @return data.frame
#' @importFrom stats knots stepfun runif
#' @export 
#' @examples
#' # See full examples at https://github.com/vando026/ahri/wiki/6-G-Imputation
#' # install.packages(system.file("IntCens_0.2.tar.gz", package = "ahri"), repos = NULL, type = 'source')
#' library(IntCens)
#' data(gdat_demo)
#' home <- file.path('/home/alain/Seafile/AHRI_Data')
#' write.table(gdat_demo, file=file.path(home, "intcens/input_data.txt"),
#'   row.names=FALSE, quote=FALSE)
#' unireg(input = file.path(home, "intcens/input_data.txt"),
#'   output = file.path(home, "intcens/res_dat.txt"),
#'   model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
#'   subject_id = "IIntID", convergence_threshold=0.01, r = 1.0)
#' ureg_res <- readUniRegResults(
#'   File=file.path(home, "intcens/res_dat.txt"))
#' Args <- setArgs(
#'   Years=c(2005:2018), nSim=50,
#'   Age=list(Mal=c(15, 54)))
#' gdates <- gImpute(gdat_demo, ureg_res, nSim=Args$nSim)

gImpute <- function(dat, Results, nSim=1,
  start_date=NULL, tscale=1, trans_back=TRUE,
  mcores=1) {

  # G = function(x)  return(x)
  G <- function(x)  return(log(1 + x))

  # simulate from the multivariate normal distribution of the 
  # regression parameter estimates
  betaMeans <- Results$edat[, "Estimate"]
  betaCovariance  <- Results$cdat
  regParamsSim <- mvtnorm::rmvnorm(n=nSim,
    mean = betaMeans, sigma = as.matrix(betaCovariance))

  # step function for the baseline hazard
  survTime <- Results$sdat[, "Time"]
  Estimate <- Results$sdat[, "Estimate"]
  baselineHazard  <-  stepfun(x=c(0, survTime),
    y=c(0, Estimate, max(Estimate)), right=FALSE)

  # Work only with HIV+
  dat <- data.frame(dat[!is.na(dat$early_pos), ])
  allIDs <- sort(unique(dat$IIntID))

  doFunc <- function(oneID, dat) {
    oneIDdata <- dat[dat$IIntID==oneID, ]
    stopifnot(nrow(oneIDdata)>0)
    start_time <- ifelse(is.null(start_date), 
      as.character(oneIDdata$obs_start[1]), start_date)
    if (oneIDdata$late_neg[1] < start_time) {
      ID <- oneID[1]
      message(sprintf("Warning: ID %s latest negative date (late_neg) is before observation start (obs_start).", ID))
    }
    leftTime <- round(as.integer(
      difftime(oneIDdata$late_neg[1], start_time, units='days'))/tscale)
    rightTime <- round(as.integer(
      difftime(oneIDdata$early_pos[1], start_time, units='days'))/tscale)

    #vector of random seroconversion times
    SeroTimes = rep(NA, nSim)

    # Get all the knots in censor interval
    jumpTimesIndicesSample = which((knots(baselineHazard)>=leftTime) &
      (knots(baselineHazard)<=rightTime))
    jumpTimesIndices = which((knots(baselineHazard)>=0) &
      (knots(baselineHazard)<=rightTime))
    variableNames <- Results$edat[, "Covariate"]
    if(length(jumpTimesIndicesSample)<1) {
      message(sprintf("=====Issue for %s ", oneID))
      message("  No infection times sampled. Suggested: check time scale in your data.")
    } else if(length(jumpTimesIndicesSample)>=1) {
      covariateValues = matrix(data=NA,
        nrow=length(jumpTimesIndices),
        ncol=length(variableNames))
    # Here we assign the covariate value to each knot on ID time
      for(i in seq_len(length(variableNames))) {
        oneVariable = variableNames[i]
        Z = stepfun(x = oneIDdata$Time,
          y = c(oneIDdata[,oneVariable],max(oneIDdata[,oneVariable])),
          right=FALSE)
        covariateValues[,i] = Z(knots(baselineHazard)[jumpTimesIndices])
      }
      xbase <- knots(baselineHazard)[jumpTimesIndices]
      Lambda <- c(0, diff(baselineHazard(xbase)))
      AllSeroTimes = c(knots(baselineHazard)[jumpTimesIndicesSample],rightTime)
      for(asim in seq_len(nSim))
        {
          M = cumsum(exp(as.vector(covariateValues %*% matrix(data=regParamsSim[asim,],ncol=1)))*Lambda)
          valF = 1 - exp(-G(M))
          distF = stepfun(x=xbase, y=c(valF,max(valF)), right=FALSE)
          seroDist <- c(0, diff(distF(AllSeroTimes)))
          mysum = sum(seroDist)
          if(0==mysum) {
            SeroTimes[asim] <- as.integer(runif(1, leftTime+1, rightTime))
          } else {
            seroDist = seroDist/mysum
            SeroTimes[asim] = sample(x=AllSeroTimes,size=1,prob=seroDist)
          }
          if(any(SeroTimes[asim]<leftTime))
            stop('Random seroconversion time smaller than allowed\n')
          if(any(SeroTimes[asim]>rightTime))
            stop('Random seroconversion time larger than allowed\n')
        }
    }
    names(SeroTimes) <- paste0("g", seq(nSim))
    if (!trans_back) tscale = 1
    c(IIntID=oneID, 
      start_date=as.Date(start_time), 
      obs_start=oneIDdata$obs_start[1], 
      late_neg=floor(leftTime*tscale),
      early_pos=ceiling(rightTime*tscale),
      round(SeroTimes*tscale)) 
  }
  out <- parallel::mclapply(allIDs, 
    function(i) doFunc(i, dat), mc.cores=mcores)
  data.frame(do.call("rbind", out))
}

#' @title uniReg
#' 
#' @description  Wrapper for Intcens executable by Zeng et al 2016. See
#' http://dlin.web.unc.edu/software/intcens/ to download the intcens program
#' for R.
#' 
#' @param xpath The path to the unireg executable.
#' @param InFile txt file to be input
#' @param OutFile txt file to be output
#' @param Model equation to be given
#' @param ID name of subject ID
#' @param iter Number of iterations
#' @param cthresh Threshold for convergence
#' @param r Threshold for convergence
#' @param inf Value for infinite, default is "Inf"
#' @param printout Print results to screen
#' @param ign_stout For Linux systems
#' @keywords internal

uniReg <- function(xpath, InFile, OutFile, Model, ID=NULL, inf="Inf",
  iter=5000, cthresh=0.0001, r=1.0, printout=FALSE, ign_stout=TRUE) {
  stop('This function is deprecated. Use the unireg function from the IntCens package')
    # InFile <- paste("--in", InFile)
    # OutFile <- paste("--out", OutFile)
    # Model <- paste("--model", shQuote(Model))
    # ID <- ifelse(is.null(ID), "", paste("--subject_id", ID))
    # Sep <- paste("--sep", shQuote(" "))
    # inf <- paste("--inf_char", inf)
    # R <- paste("--r", r)
    # iter <- paste("--max_itr", iter)
    # cthresh <- paste("--convergence_threshold", cthresh)
    # if (Sys.getenv("OS") == "Windows_NT") {
    #   system(command=paste(xpath, InFile, OutFile, Model, 
    #     ID, Sep, iter, R, inf, cthresh, collapse=" "),
    #     show.output.on.console=printout)
    # } else {
    #   system(command=paste(xpath, InFile, OutFile, Model, 
    #     ID, Sep, iter, R, inf, cthresh, collapse=" "),
    #     ignore.stdout=ign_stout)
    # }
}

#' @title runUniReg
#' 
#' @description  Helper function to run \code{\link{uniReg}}.  
#' @param  Vars Variables to feed into uniReg model.
#' @param  Name Name of input txt file.
#' 
#' @return list
#'
#' @keywords internal 
runUniReg <- function(Vars, Name) {
  unireg(
    input=file.path(derived, paste0(Name,".txt")), 
    output=file.path(derived, paste0(Name, "_out.txt")), 
    model = paste0("(Time, sero_event) = ", Vars), 
    subject_id ="IIntID", r = 1.0, convergence_threshold=0.01)
}


#' @title uniRegOne
#' 
#' @description  Run gModel on each variable and make table
#' 
#' @param Vars Vector of RHS character varnames. 
#' @param Name File name of the gModel results from \code{\link{readUniRegResults}}. 
#' @export
#' @keywords internal
#' @examples 
#' \donttest{uniRegOne(c("Age0", "Age2"), Name="icens_mal")}
uniRegOne <- function(Vars, Name) {
  dat = list()
  for(vari in Vars) {
    message(sprintf("Running UniReg on %s", vari))
    runUniReg(vari, Name)
    res <- readUniRegResults(File=
      file.path(derived, paste0(Name,"_out.txt")))
    dat[[vari]] <- res$edat
  }
  do.call(rbind, dat)
}

#' @title Add theimputed infection times to an existing dataframe.
#' 
#' @description  Adds the ith column of imputed infection times from \code{\link{gImpute}} to an
#' existing dataset.
#' 
#' @param rtdat An existing dataset generated from \code{\link{getRTData}}. 
#' @param gdat The imputed dateset from \code{\link{gImpute}}.
#' @param i The ith imputed infection time in gdat.
#' 
#' @return data.frame
#' @export 
#' @examples
#' # See full examples at https://github.com/vando026/ahri/wiki/6-G-Imputation
#' # install.packages(system.file("IntCens_0.2.tar.gz", package = "ahri"), repos = NULL, type = 'source')
#' library(IntCens)
#' data(gdat_demo)
#' data(rtdat_demo)
#' home <- file.path('/home/alain/Seafile/AHRI_Data')
#' write.table(gdat_demo, file=file.path(home, "intcens/input_data.txt"),
#'   row.names=FALSE, quote=FALSE)
#' unireg(input = file.path(home, "intcens/input_data.txt"),
#'   output = file.path(home, "intcens/res_dat.txt"),
#'   model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
#'   subject_id = "IIntID", convergence_threshold=0.01, r = 1.0)
#' ureg_res <- readUniRegResults(
#'   File=file.path(home, "intcens/res_dat.txt"))
#' Args <- setArgs(
#'   Years=c(2005:2018), nSim=50,
#'   Age=list(Mal=c(15, 54)))
#' gdates <- gImpute(gdat_demo, ureg_res, nSim=Args$nSim)
#' gdat_ex <- getGImpute(rtdat_demo, gdates, 1)
#' gdat_ex[gdat_ex$sero_event==1, ]
getGImpute <- function(rtdat, gdat, i) {
  gdat <- gdat[, c("IIntID", "start_date", paste0("g", i))]
  names(gdat) <- c("IIntID", "start_date", "sero_days")
  gdat <- mutate(gdat,
    sero_date =  as.Date(.data$start_date + .data$sero_days, origin="1970-01-01"))
  left_join(rtdat, gdat, by="IIntID") %>% 
    select(-c(.data$start_date, .data$sero_days))
}


