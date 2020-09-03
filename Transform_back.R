#Program that converts QS wide data into SDTM format

library(tidyverse)
library(haven)
library(lubridate)

#Read in the wide QS data and metadata, as well as the original QS data for comparison purposes. Also need the DM data for QSDY calculation
meta<-read_sas("J:/BACPAC/SC/Review/BP0001/200706/qsmd_metadata.sas7bdat")
wide<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/qs_sim_wide.csv", col_types=cols(.default = "c"))
qs<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/QSmd.sas7bdat")
dm<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/DM.sas7bdat")

#First, need to create the derived variables while the data is still in wide format
#Identify derived variables to create
#Create a df of all the derived variables that need to be created
derived<-meta %>% 
  filter(DERIVED_FLAG=="Y")
vars<-derived$TESTCODE_VALUE

#So, the derived variables are derived from QSTRESN, not QSTRESC. This is problematic based on how I created the wide dataset.
#With how I have it setup currently, I would need to transpose the wide data to long, merge the QSTRESN values into the data,
#transpose back to wide, then calculate the derived variables, and then transpose to long once more and add the rest of the
#information for the QS dataset. I could also derive them in long format for less transposing, but this would involve 
#calculations across rows (as opposed to just across columns) which would make the code a bit more complex.
#The other option is to create the initial wide file as values with QSTRESN rather than QSTRESC, in which case the process
#for transforming this wide file to SDTM would be calculating the derived variables, transposing to long and adding in the
#additional QS information/fields. This is simpler but it depends on what format of a wide file we'd typically receive.


#Then, make the data long
long<-wide %>% 
  pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC),
               names_to="QSTESTCD",
               values_to="QSSTRESC")

#Create a metadata file with variables needed for merging from QSTESTCD
qstestcd_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSCAT=CATEGORY,
         QSSCAT=SUBCATEGORY,
         QSTEST=TEST_VALUE,
         QSEVLNT=EVALUATION_INTERVAL) %>% 
  distinct()

#Create a metadata file with variables needed for merging from QSTESTCD and QSTRESC
qsstresc_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSSTRESC=STRESC_TERM,
         QSSTRESN=STRESN_TERM) %>% 
  distinct()

#Create a df with the reference start day to calculate QSDY
start_date<-dm %>% 
  select(USUBJID, RFSTDTC)

#Create vector of column names of the QS data
order<-colnames(qs)
df_order<-as.data.frame(order)

#Now, we have to add all the associated information for all of the questions




#I need to re-think the below.
qs_<-long %>% 
  mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
         VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
         DOMAIN="QS") %>% 
  #Need to merge in QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
  left_join(qstestcd_lu) %>% 
  arrange(USUBJID, VISIT) %>% 
  #Need to merge in QSSTRESN based upon QSTESTCD and QSSTRESC
  left_join(qsstresc_lu) %>% 
  #Calculate the study day
  left_join(start_date) %>% 
  #Calculate derived variables
  #Arrange by needed variables and create the QSSEQ variable
  arrange(STUDYID, USUBJID, QSCAT, QSSCAT, VISITNUM, QSTESTCD) %>% 
  group_by(USUBJID) %>% 
  mutate(QSSEQ=1:n()) %>% 
  ungroup() %>% 
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
    #Need to merge in QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
    left_join(qstestcd_lu) %>% 
    arrange(USUBJID, VISIT) %>% 
    #Need to merge in QSSTRESN and QSDRVFL based upon QSTESTCD and QSSTRESC
    left_join(qsstresc_lu) %>% 
    #Arrange by needed variables and create the QSSEQ variable
    arrange(STUDYID, USUBJID, QSCAT, QSSCAT, VISITNUM, QSTESTCD) %>% 
    group_by(USUBJID) %>% 
    mutate(QSSEQ=1:n()) %>% 
    ungroup() %>% 
    select(order)
}

#Example using function
qs_transform<-wide %>% 
  transform_sdtm()

#Need to compare the two datasets and make sure there aren't differences




