####### Program for getting most recent data from HMD
############# For Serena Vigezzi et al
############# 24/10/2019
library(HMDHFDplus)
library(data.table)

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- "user"
# set your password
pw <- "password"

# get all the lifetables available from HMD
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDL <- data.table(HMDL)

# save the data
save(HMDL,file="Data/HMD_Data.RData")
