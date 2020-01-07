#/************************************************************************
#*** You may need to edit this code.                                  ***
#***                                                                  ***
#*** Please check all SETWD statements before running this code.      ***
#***                                                                  ***
#*** You may have selected variables that contain missing data or     ***
#*** valid skips. You may wish to recode one or both of these special ***
#*** values. You need to consult the Variable Description to see if   ***
#*** these special codes apply to your extracted variables. You can   ***
#*** recode these special values to missing using the "car" package   ***
#*** and the following sample code:                                   ***
#***                                                                  ***
#*** {variable name} <- recode({variable name},"c({value}) = NA")     ***
#***                                                                  ***
#*** Replace {variable name} above with the name of the variable you  ***
#*** wish to recode. Replace {value} with the special value you wish  ***
#*** to recode to missing.                                            ***
#***                                                                  ***
#*** It is important to retain full sample weights, replicate         ***
#*** weights, and identification numbers as appropriate.              ***
#************************************************************************/

# Change working directory
setwd("C:/EDAT/NHES")

# Load R Data File
load("nhes_16_pfi_v1_0.rdata")

# Create vector of selected variables
keepvars <- c(
   "GRADE",
   "SCPUBPRI",
   "FSSPORTX",
   "FSVOL",
   "FSMTNG",
   "FSPTMTNG",
   "FSATCNFN",
   "FSFUNDRS",
   "FSCOMMTE",
   "FSCOUNSLR",
   "FSFREQ",
   "FOSTORY2X",
   "FOCRAFTS",
   "FOGAMES",
   "FOBUILDX",
   "FOSPORT",
   "FORESPON",
   "FOHISTX",
   "FOLIBRAYX",
   "FOBOOKSTX",
   "FOCONCRTX",
   "FOMUSEUMX",
   "FOZOOX",
   "FOGROUPX",
   "FOSPRTEVX",
   "HHTOTALXX",
   "HWELFTAN",
   "HWELFST",
   "HWIC",
   "HFOODST",
   "HMEDICAID",
   "HCHIP",
   "HSECN8",
   "TTLHHINC",
   "OWNRNTHB",
   "RACEETHN",
   "RACEETH2",
   "INTACC"
)

# Create new object containing only selected variables
nhes_16_pfi_v1_0_200106194103 <- nhes_16_pfi_v1_0[keepvars]

# Save dataset
save(nhes_16_pfi_v1_0_200106194103, file="nhes_16_pfi_v1_0_200106194103.rdata")

# Set the working dataset
attach(nhes_16_pfi_v1_0_200106194103)

# Display frequencies for the categorical variables
table(GRADE)
table(SCPUBPRI)
table(FSSPORTX)
table(FSVOL)
table(FSMTNG)
table(FSPTMTNG)
table(FSATCNFN)
table(FSFUNDRS)
table(FSCOMMTE)
table(FSCOUNSLR)
table(FSFREQ)
table(FOSTORY2X)
table(FOCRAFTS)
table(FOGAMES)
table(FOBUILDX)
table(FOSPORT)
table(FORESPON)
table(FOHISTX)
table(FOLIBRAYX)
table(FOBOOKSTX)
table(FOCONCRTX)
table(FOMUSEUMX)
table(FOZOOX)
table(FOGROUPX)
table(FOSPRTEVX)
table(HHTOTALXX)
table(HWELFTAN)
table(HWELFST)
table(HWIC)
table(HFOODST)
table(HMEDICAID)
table(HCHIP)
table(HSECN8)
table(TTLHHINC)
table(OWNRNTHB)
table(RACEETHN)
table(RACEETH2)
table(INTACC)
