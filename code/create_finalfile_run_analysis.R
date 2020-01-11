library(tidyverse)
library(ggplot2)
library(psych)
library(ltm)
library(dplyr)
library(GPArotation)
library(ggplot2)
library(survey)
library(cluster)
library(gmodels)
#load("C:/Users/trbeverly/AppData/Local/Temp/Temp1_pfi_pu_pert_rdata.zip/pfi_pu_pert.rdata")


newdf <- dplyr::select(pfi_pu_pert, c("basmid", "scpubpri", "sneighbrx", "sefuturex", "fssportx", "fsvol",
                                       "fsmtng", "fsptmtng", "fsatcnfn", "fsfundrs", "fscommte", "fscounslr",
                                       "fsfreq", "fcschool", "fcteachr", "fcstds", "fcorder", "fcsupprt", "FOSTORY2X",
                                       "focrafts","fogames", "fobuildx", "fosport", "forespon", "fohistx",
                                       "folibrayx", "fobookstx", "foconcrtx", "fomuseumx", "fozoox", "fogroupx",
                                       "fosprtevx", "chispan", "camind", "casian", "cpaci", "cwhite",
                                       "chisprm", "csex", "hhtotalxx", "AGE2015", "HHPARN16X", "HHPARN16_BRD",
                                       "numsibsx", "pargradex", "raceethn", "allgradex", "grade", "S16CHART", "S16PBPV",
                                       "hhtotalxx", "relation", "hwelftan", "hwelfst", "hwic", 
                                       "hfoodst", "hmedicaid", "hchip", "HSECN8","RACEETH2", "ttlhhinc", "ownrnthb", "hvintcom"))

#filtering by students enrolled in public and private students 
newdf <- newdf %>% 
  filter(newdf$scpubpri != "-1")

# Step 1. create a dataset for dichotomous variables that need to be recoded
recodedvars <- dplyr::select(newdf, c("basmid", "fssportx", "fsvol", "fsmtng", "fsptmtng", "fsatcnfn", "fsfundrs",
                            "fscommte", "fscounslr", "FOSTORY2X","focrafts","fogames", "fobuildx", "fosport", 
                            "forespon", "fohistx", "folibrayx", "fobookstx", "foconcrtx", "fomuseumx", 
                            "fozoox", "fogroupx", "fosprtevx", "chispan", "camind", "casian", "cpaci",
                            "cwhite", "sneighbrx"))

explanatory <- dplyr::select(newdf, c("basmid", "allgradex", "hhtotalxx", "AGE2015", "pargradex",
                            "ttlhhinc", "fsfreq", "scpubpri", "sefuturex", "RACEETH2",
                            "fcschool", "fcteachr", "fcstds", "fcorder", "fcsupprt"))

#Step 2 recode variables using reshape2 command
recodedvars = reshape2::dcast(
  dplyr::mutate(
    reshape2::melt(recodedvars,id.var="basmid"),
    value=plyr::mapvalues(
      value, c("1","2"),c(1,0))
  ),basmid~variable)


parental_involvement <- merge(recodedvars, explanatory, by="basmid")

# dimension reduction for instument development
fa.final<- psych::fa(parental_involvement[,c(2:23, 39:43)], nfactors=4, rotate= "oblimin", fm="ml", scores = "regression")
plot(fa.final)
fa.final

parental_involvement$FS <- rowSums(parental_involvement[,2:8], na.rm = TRUE)
parental_involvement$FA <- rowSums(parental_involvement[, c(10:14, 21)], na.rm = TRUE)
parental_involvement$SSatis <- rowSums(parental_involvement[,39:43 ], na.rm = TRUE)

length(parental_involvement[,c(2:8, 10:14, 21, 39:43)])

#creating new dataset for GLMs
parinv <- parental_involvement[,c(1, 24:38, 44:46)]

#recoding the public school variable
apply(parinv[-1], 2, table)
parinv$public <- ifelse(parinv$scpubpri==4, 1, 0)

#saving the final dataset
save(parinv, file = "parental_involvement.Rdata")

cordata <- parinv[, c(9:13,17:19)]
head(cordata, 3)

#iclust(cordata, nclusters = 2)

#res <- cor(cordata)
#round(res, 2)

hist(as.matrix(cordata$hhtotalxx))
heatmap(parinv[, c(9:13, 17:20)])
heatmap(as.matrix(cordata))

heatmap(cordata, scale = "row")

ggplot(parinv, AGE2015) +
  geom_histogram() + theme_bw()

ggplot(parinv, aes(AGE2015, fill=..count..)) +
  geom_histogram(binwidth = 4) + theme_bw()
  
colnames(parinv)

#some descriptive statistics
# 2-Way Cross Tabulation

ctabs <- CrossTable(parinv$FS, parinv$AGE2015, digits = 2, , prop.r = TRUE, prop.c = TRUE)
ctabs <- CrossTable(parinv$FS, parinv$RACEETH2, digits = 2, , prop.r = TRUE, prop.c = TRUE)

tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  
  res
}


do.call(rbind,lapply(newdf[5:12],tblFun))
table(parinv$sneighbrx)
table(parinv$allgradex)
table(parinv$hhtotalxx)
table(parinv$sefuturex)

#create regression models

fit <-lm(FS ~ AGE2015 + hhtotalxx + AGE2015 + pargradex + ttlhhinc + FA + SSatis + public + sneighbrx + sefuturex, data=parinv) 
summary(fit)
plot(fit)

fit2 <-lm(FS ~ AGE2015 +  AGE2015 + pargradex + ttlhhinc + FA + SSatis + public + sefuturex, data=parinv) 
summary(fit2)
plot(fit2)

posfit <- glm(FS ~ AGE2015 + hhtotalxx + AGE2015 + pargradex + ttlhhinc + FA + SSatis + public + sneighbrx + sefuturex, data=parinv, family = poisson())
summary(posfit)
exp(coef(posfit))
plot(posfit)                                            

posfit2 <- glm(FS ~ AGE2015 + hhtotalxx + AGE2015 + pargradex + FA + SSatis + public +  sefuturex, data=parinv, family = poisson())
summary(posfit2)
exp(coef(posfit2))
plot(posfit2) 

#cluster analysis
#agn1 <- agnes(parental_involvement, metric = "euclidean", stand = FALSE) 
#agn1 
#plot(agn1)


describe(parinv[2:19])


