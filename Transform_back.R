#Program that converts QS wide data into SDTM format

library(tidyverse)
library(haven)
library(lubridate)

#Read in the wide QS data and metadata, as well as the original QS data for comparison purposes
meta<-read_sas("J:/BACPAC/SC/Review/BP0001/200706/qsmd_metadata.sas7bdat")
wide<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/qs_sim_wide.csv", col_types=cols(.default = "c"))
qs<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/QSmd.sas7bdat")

#First step is making it long
long<-wide %>% 
  pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC, -QSDY),
               names_to="QSTESTCD",
               values_to="QSSTRESC")

#Create a metadata file with variables needed for merging from QSTESTCD
qstestcd_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSSEQ=TESTCODE_ORDER,
         QSCAT=CATEGORY,
         QSSCAT=SUBCATEGORY,
         QSTEST=TEST_VALUE,
         QSEVLNT=EVALUATION_INTERVAL) %>% 
  distinct()

#Create a metadata file with variables needed for merging from QSTESTCD and QSTRESC
qsstresc_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSSTRESC=STRESC_TERM,
         QSSTRESN=STRESN_TERM,
         QSDRVFL=DERIVED_FLAG) %>% 
  distinct()

#Create vector of column names of the QS data
order<-colnames(qs)
df_order<-as.data.frame(order)

#Now, we have to add all the associated information for all of the questions
qs_<-long %>% 
  mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
         VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
         DOMAIN="QS") %>% 
  #Need to merge in QSSEQ, QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
  left_join(qstestcd_lu) %>% 
  arrange(USUBJID, VISIT, QSSEQ) %>% 
  #Need to merge in QSSTRESN and QSDRVFL based upon QSTESTCD and QSSTRESC
  left_join(qsstresc_lu) %>% 
  select(order)

#Export our lookup tables and write this as a function
#write_csv(qstestcd_lu, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testcode_lu.csv")
#write_csv(qsstresc_lu, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testresult_lu.csv")
#write_csv(df_order, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/column_order.csv")

transform_sdtm<-function(x){
  #This will need to be modified to read in the lookup tables from the location the repo is on your machine
  qstestcd_lu<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testcode_lu.csv")
  qsstresc_lu<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testresult_lu.csv")
  df_order<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/column_order.csv")
  order<-df_order$order
  
  x %>% 
    pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC, -QSDY),
                 names_to="QSTESTCD",
                 values_to="QSSTRESC") %>% 
    mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
           VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
           DOMAIN="QS") %>% 
    #Need to merge in QSSEQ, QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
    left_join(qstestcd_lu) %>% 
    arrange(USUBJID, VISIT, QSSEQ) %>% 
    #Need to merge in QSSTRESN and QSDRVFL based upon QSTESTCD and QSSTRESC
    left_join(qsstresc_lu) %>% 
    select(order)
}

#Example using function
qs_transform<-wide %>% 
  transform_sdtm()




