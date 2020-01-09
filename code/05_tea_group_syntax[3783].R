library(tidyverse)
library(dplyr)

#loading R dataset
#load("C:/Users/trbeverly/AppData/Local/Temp/Temp1_pfi_pu_pert_rdata.zip/pfi_pu_pert.rdata")

#variables necessary for analysis

pinv <- select(pfi_pu_pert, c("basmid", "scpubpri", "sneighbrx", "sefuturex", "fssportx", "fsvol",
                              "fsmtng", "fsmtng", "fsatcnfn", "fsfundrs", "fscommte", "fscounslr",
                              "fsfreq", "fcschool", "fcteachr", "fcstds", "fcorder", "fcsupprt", "FOSTORY2X",
                              "focrafts","fogames", "fobuildx", "fosport", "forespon", "fohistx",                               "folibrayx", "fobookstx", "foconcrtx", "fomuseumx", "fozoox", "fogroupx",
                              "fosprtevx", "chispan", "camind", "casian", "cpaci", "cwhite",
                              "chisprm", "csex", "hhtotalxx", "AGE2015", "HHPARN16X", "HHPARN16_BRD",
                              "numsibsx", "pargradex", "raceethn", "allgradex", "grade", "S16CHART", "S16PBPV",
                              "hhtotalxx", "relation", "hwelftan", "hwelfst", "hwic",
                              "hfoodst", "hmedicaid", "hchip", "HSECN8","RACEETH2", "ttlhhinc", "ownrnthb", "hvintcom"))

#filtering by students enrolled in public and private students 
pi <- pinv %>% 
  filter(pinv$scpubpri != "-1")

# recoding family and school involvement variables

# Step 1. create a dataset for dichotomous variables that need to be recoded
recodedvars <- select(pi, c("basmid", "fssportx", "fsvol", "fsmtng", "fsmtng", "fsatcnfn", "fsfundrs",
                            "fscommte", "fscounslr", "FOSTORY2X","focrafts","fogames", "fobuildx", "fosport", 
                            "forespon", "fohistx", "folibrayx", "fobookstx", "foconcrtx", "fomuseumx", 
                            "fozoox", "fogroupx", "fosprtevx", "chispan", "camind", "casian", "cpaci",
                            "cwhite"))

#Step 2 recode variables using reshape2 command
fs = reshape2::dcast(
  dplyr::mutate(
    reshape2::melt(recodedvars,id.var="basmid"),
    value=plyr::mapvalues(
      value, c("1","2"),c(1,0))
  ),basmid~variable)

apply(fs[-1], 2, table)


# creating new variables for family involvement in school activities and outside activities

fs$FS <- rowSums(fs[,2:8], na.rm = TRUE)
fs$FA <- rowSums(fs[,9:22], na.rm = TRUE)
pi$SSatis <- rowSums(pi[,13:17], na.rm = TRUE)
range(pi$SSatis)
mean(pi$SS)
range


# Create new datasets 
parinv <- select(fs, c("basmid", "chispan", "camind", "casian", "cpaci",
                       "cwhite", "FS", "FA"))
explanatory <- select(pi, c("basmid", "allgradex", "hhtotalxx", "AGE2015", "pargradex",
                          "ttlhhinc", "fsfreq", "scpubpri", "sneighbrx", "sefuturex", "SSatis", "RACEETH2"))

# merging two datasets for complete dataset
parental_involvement <- merge(parinv, explanatory, by="basmid")

apply(parental_involvement[-1], 2, table)
mean(parental_involvement$AGE2015)
mean(parental_involvement$fsfreq)

hist(parental_involvement$AGE2015)
hist(parental_involvement$fsfreq)
range(parental_involvement$AGE2015)
range(parental_involvement$fsfreq)
t.income_by_FS <- xtabs(parental_involvement$ttlhhinc, parental_involvement$FS)
t.income_by_FS

table(parental_involvement$ttlhhinc, parental_involvement$fsfreq)
table(parental_involvement$ttlhhinc, parental_involvement$FA)

incfa <- table(parental_involvement$ttlhhinc, parental_involvement$FA)

prop.table(incfa,1)

install.packages("gmodels")
library(gmodels)

CrossTable(parental_involvement$ttlhhinc, parental_involvement$FS)

table(parental_involvement$F)




