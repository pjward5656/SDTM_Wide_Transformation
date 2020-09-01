#Program that converts QS wide data into SDTM format

library(tidyverse)
library(haven)
library(lubridate)

#Read in the wide QS data and metadata
meta<-read_sas("J:/BACPAC/SC/Review/BP0001/200706/qsmd_metadata.sas7bdat")
wide<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/qs_sim_wide.csv", col_types=cols(.default = "c"))

#First step is making it long
long<-wide %>% 
  pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC),
               names_to="QSTESTCD",
               values_to="QSTRESC")

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
qstresc_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSTRESC=STRESC_TERM,
         QSTRESN=STRESN_TERM,
         QSDRVFL=DERIVED_FLAG) %>% 
  distinct()

#Now, we have to add all the associated information for all of the questions
qs_<-long %>% 
  mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
         VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
         DOMAIN="QS") %>% 
  #Need to merge in QSSEQ, QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
  left_join(qstestcd_lu) %>% 
  arrange(USUBJID, VISIT, QSSEQ) %>% 
  #Need to merge in QSTRESN and QSDRVFL based upon QSTESTCD and QSTRESC
  left_join(qstresc_lu)
