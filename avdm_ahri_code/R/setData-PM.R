#' @title Set age according to user arguments. 
#' @description  sets age according to arguments
#' @param dat must have a variable named AgeAtVisit  
#' @param Args requires Args, see \code{\link{setArgs}}
#' @return data.frame
#' @export 

setAge <- function(dat, Args) {
  if ("All" %in% names(Args$Age)) {
    dat <- filter(dat, !(Age < Args$Age[["All"]][1]) &
      !(Age > Args$Age[["All"]][2]))
  } 
  if ("Mal" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==0 & Age < Args$Age[["Mal"]][1]) &
      !(Female==0 & Age > Args$Age[["Mal"]][2]))
  } 
  if ("Fem" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==1 & Age < Args$Age[["Fem"]][1]) &
      !(Female==1 & Age > Args$Age[["Fem"]][2]))
  }
  dat
}

#' @title Gets the birthdates form the Surveillance Episodes dataframe. 
#' @description  Gets birth dates from \code{\link{getEpisodes}} data
#' @param dat default value is \code{Null}, which calls \code{\link{getEpisodes}}
#' @param addVars String for a regular expression to select additional vars
#' @return data.frame
#' @import dplyr
#' @export 
#' @examples
#' getBirthDate(addVars="Female")

getBirthDate <- function(dat = NULL, addVars=" ") {
  if (is.null(dat)) 
    dat <- getEpisodes() 
  dat <- dplyr::select(dat, IIntID, DateOfBirth=DoB, contains(addVars))
  dat <- group_by(dat, IIntID) %>% slice(1)
  dat <- ungroup(dat)
  dat <- filter(dat, !is.na(DateOfBirth))
  dat <- filter(dat, as.numeric(format(DateOfBirth, "%Y")) > 1910)
  dat
}


#' @title  Function for making Age and AgeCat variables
#' 
#' @description  Function for making Age and AgeCat variables
#' 
#' @param dat A dataset 
#' @param time2 The name of a date variable that is used to calculate the Age variable using the
#' \code{\link{getBirthDate}} function. Age is calculated as (time2 - birthdate)/365.35.
#' @param age_cut Vector of ages to make age categories, default is to use values from
#' \code{AgeCat} in \code{link{setArgs}}.
#' @param birthdate Dataset of birthdates, if NULL it uses \code{\link{getBirthDate}}.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' hiv <- makeAgeVars(dat=getHIV(), age_cut=c(15, 20, 25, 30, 40, 50))

makeAgeVars <- function(dat, time2=NULL, age_cut=NULL, birthdate=NULL){
  if (!is.null(time2)) {
    if (is.null(birthdate)) birthdate <- getBirthDate()
    dat <- data.frame(left_join(dat, birthdate, by="IIntID"))
    dat$Age <- floor(as.numeric(difftime(
      dat[,time2], dat[,"DateOfBirth"], units='weeks'))/52.25)
    dat <- dplyr::select(dat, -(DateOfBirth))
  }
  if(!is.null(age_cut)) {
    dat <- mutate(dat, AgeCat = cut(Age, breaks=age_cut,
      include.lower=TRUE, right=FALSE, labels=NULL))
    dat$AgeCat <- droplevels(dat$AgeCat)
  }
  tibble::as_tibble(dat) 
}


#' @title Sets data according to a list of arguments. 
#' @description  Sets data according to a list of arguments. 
#' @param dat A dataset to subset. 
#' @param Args Takes a list of arguments from \code{\link{setArgs}}. 
#' @param time2 The name of a date variable that is used to calculate the Age variable using the
#' \code{\link{getBirthDate}} function. Age is calculated as (time2 - birthdate)/365.35.
#' If time2=NULL, the default, then \code{\link{setData}} will use an existing Age
#' variable in \code{dat}. 
#' @param age_cut Vector of ages to make age categories from \code{link{setArgs}}.
#' @param birthdate Takes the dataset generated from \code{\link{getBirthDate}}. 
#' @return data.frame
#' @import dplyr
#' @export
#' @examples
#' hiv <- getHIV()
#' Args <- setArgs(Age=list(All=c(15, 25)))
#' # This will use the existing Age variable to subset 
#' adat <- setData(hiv, Args)
#' # This will create a new Age variable using the birthdat and subset by age
#' adat1 <- setData(hiv, Args, time2="VisitDate", birthdate=getBirthDate())
#' # Note that there will be some discrepancy in the number of observations between adat and
#' adat1

setData <- function(dat, Args=setArgs(), time2=NULL,
    age_cut=NULL, birthdate=NULL) {
  if (is.null(age_cut)) age_cut <- Args$AgeCat
  dat <- makeAgeVars(dat, time2=time2,
    age_cut=age_cut, birthdate=birthdate)
  # Filter by Age limits
  dat <- setAge(dat, Args)
  check_var(dat, "Female")
  check_var(dat, "Year")
  dat <- filter(dat, Female %in% Args$FemCode)
  dat <- filter(dat, Year %in% Args$Years)
  # further actions to take place
  dat <- Args$addVars(dat)
  dat <- Args$setFun(dat)
  tibble::as_tibble(dat)
}

