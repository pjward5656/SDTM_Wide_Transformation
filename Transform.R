#Program that converts QS SDTM data into wide format

library(tidyverse)
library(haven)

#Read in the QS data and metadata
qs<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/QSmd.sas7bdat")
meta<-read_sas("J:/BACPAC/SC/Review/BP0001/200706/qsmd_metadata.sas7bdat")


#Lets write this as normal code first, then convert into a generalizable function
wide<-qs %>% 
  #Remove derived variables
  filter(QSDRVFL!="Y") %>% 
  #Selecting the variables we need
  select(USUBJID, VISIT, QSDTC, QSTESTCD, QSSTRESN) %>% 
  pivot_wider(id_cols=c(USUBJID, VISIT, QSDTC),
              names_from=QSTESTCD,
              values_from=QSSTRESN)

#As a function
transform_wide<-function(x){
  x %>% 
    filter(QSDRVFL!="Y") %>%
    select(USUBJID, VISIT, QSDTC, QSTESTCD, QSSTRESN) %>% 
    pivot_wider(id_cols=c(USUBJID, VISIT, QSDTC),
                names_from=QSTESTCD,
                values_from=QSSTRESN)
}

#Example use
wide_new<-qs %>% 
  transform_wide()

#Export result
write_csv(wide_new, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/qs_sim_wide.csv")
