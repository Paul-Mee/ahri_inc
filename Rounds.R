library(ahri)

getFiles = setFiles("HIV-INCIDENCE/2023_Data")
setFiles(
  folder = "HIV-INCIDENCE/2023_Data",
  hivfile = "RD05-99 ACDIS HIV All.dta",
)

hiv_all = readHIVData(dropTasP = TRUE, addVars = "DSRound")

hiv_all$DSRound = as.factor(hiv_all$DSRound)

hiv_all = hiv_all[!(hiv_all$DSRound %in% c(3,7,8)), ]

hiv_all$DSRound = droplevels(hiv_all$DSRound)
hiv_all = hiv_all[hiv_all$PIPSA == "Southern PIPSA" | is.na(hiv_all$PIPSA),]




#rounds = hiv_all %>% group_by(DSRound) %>% dplyr::summarise(earliest_visit_date =min(VisitDate), latest_visit_date = max(VisitDate), m_year =mean.Date(as.Date(c(min(VisitDate), max(VisitDate)), format=c("%Y-%m-%d"))), Round_Year = substr(m_year, 1,4))
rounds = hiv_all %>% group_by(DSRound) %>% dplyr::summarise(earliest_visit_date =min(VisitDate), latest_visit_date = max(VisitDate), m_year =mean.Date(as.Date(VisitDate), format=c("%Y-%m-%d")), Round_Year = substr(m_year, 1,4))

##remove missing last row
rounds = rounds[complete.cases(rounds),]

rounds$mid_year = rounds$m_year
rounds$mid_year[rounds$Round_Year == 2004]<-mean.Date(rounds$m_year[rounds$Round_Year==2004])

rounds$Round_Year = substr(rounds$mid_year, 1,4)

rounds$final_mid_year = rounds$mid_year
rounds$final_mid_year[rounds$Round_Year == 2003] <-unique(rounds$mid_year[rounds$Round_Year == 2004])

##change 2003  to 2004 
rounds$Round_Year = substr(rounds$final_mid_year, 1,4)



##Assign roundyear to hiv_data

nrounds = rounds[, c(1,5,7)]



hiv = merge(hiv_all, nrounds, by = "DSRound")


writexl::write_xlsx(rounds, "nrounds.xlsx")


##hiv prevalence

prev = hiv %>% group_by(BSIntID, Round_Year) %>% dplyr::summarise(Tot = n(), pos = sum(HIVResult), neg = Tot-pos,HIVprev = sum(HIVResult)/n())


