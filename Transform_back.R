#Program that converts QS wide data into SDTM format

library(tidyverse)
library(haven)
library(lubridate)

#Read in the wide QS data and metadata, as well as the original QS data for comparison purposes. Also need the DM data for QSDY calculation
meta<-read_sas("J:/BACPAC/SC/Review/BP0001/200706/qsmd_metadata.sas7bdat")
wide<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/qs_sim_wide.csv")
qs<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/QSmd.sas7bdat")
dm<-read_sas("J:/BACPAC/SC/SASdata/Simulated/200706/DM.sas7bdat")

#First, need to create the derived variables while the data is still in wide format
#Identify derived variables to create
#Create a df of all the derived variables that need to be created
derived<-meta %>% 
  filter(DERIVED_FLAG=="Y")
vars<-derived$TESTCODE_VALUE

#Read in the T-score lookup
library(readxl)
t_score<-read_xlsx("J:/BACPAC/Statistics/Data_Standards/Standard_Specifications/DERIVEDSCORES_MinimumDataset.xlsx",
                   sheet="RAW_to_TSCORE")

#Add the derived variables to the wide dataset
wide_derived<-wide %>% 
  mutate(GAD2RAW=GAD01 + GAD02,
         PRANX4AR=EDANX01 + EDANX40 + EDANX41 + EDANX53,
         PRDEP4AR=EDDEP04 + EDDEP06 + EDDEP29 + EDDEP41,
         PRPI4AR=PAININ9 + PAININ22 + PAININ31 + PAININ34,
         PRPF6BR=PFA11 + PFA21 + PFA23 + PFA53 + PFC12 + PFB1,
         PRSD6AR=SLEEP109 + SLEEP116 + SLEEP20 + SLEEP44 + SLEEP108 + SLEEP72,
         PCSRAW=PCS4 + PCS5 + PCS6 + PCS10 + PCS11 + PCS13,
         PHQ2RAW=PHQ01 + PHQ02,
         WDSPPNUM=PNAREA1 + PNAREA2 + PNAREA3 + PNAREA4 + PNAREA5 + PNAREA6 + PNAREA7,
         PEGSCORE=(PEG1 + PEG2 + PEG3)/3) %>% 
  #Now all of the T-scores
  left_join(t_score[t_score$MEASURE=="PRMANX", c("RAWSCORE", "TSCORE")], by=c("PRANX4AR"="RAWSCORE")) %>% 
  rename(PRANX4AT=TSCORE) %>% 
  left_join(t_score[t_score$MEASURE=="PRMDEP", c("RAWSCORE", "TSCORE")], by=c("PRDEP4AR"="RAWSCORE")) %>% 
  rename(PRDEP4AT=TSCORE) %>% 
  left_join(t_score[t_score$MEASURE=="PRMPI", c("RAWSCORE", "TSCORE")], by=c("PRPI4AR"="RAWSCORE")) %>% 
  rename(PRPI4AT=TSCORE) %>% 
  left_join(t_score[t_score$MEASURE=="PRMPHF", c("RAWSCORE", "TSCORE")], by=c("PRPF6BR"="RAWSCORE")) %>% 
  rename(PRPF6BT=TSCORE) %>% 
  left_join(t_score[t_score$MEASURE=="PRMSD", c("RAWSCORE", "TSCORE")], by=c("PRSD6AR"="RAWSCORE")) %>% 
  rename(PRSD6AT=TSCORE)

  
#Then, make the data long
long<-wide_derived %>% 
  pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC),
               names_to="QSTESTCD",
               values_to="QSSTRESN")

#Create a metadata file with variables needed for merging from QSTESTCD
qstestcd_lu<-meta %>% 
  select(QSTESTCD=TESTCODE_VALUE,
         QSCAT=CATEGORY,
         QSSCAT=SUBCATEGORY,
         QSTEST=TEST_VALUE,
         QSEVLNT=EVALUATION_INTERVAL,
         QSDRVFL=DERIVED_FLAG) %>% 
  distinct()

#Create a metadata file with variables needed for merging from QSTESTCD and QSTRESN
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
qs_<-long %>% 
  mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
         VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
         DOMAIN="QS") %>% 
  #Need to merge in QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
  left_join(qstestcd_lu) %>% 
  #Round and convert QSSTRESN to character to merge the other information in
  mutate_at(vars(QSSTRESN), list(~as.character(round(., digits=2)))) %>% 
  #Need to merge in QSSTRESC based upon QSTESTCD and QSSTRESN
  left_join(qsstresc_lu) %>% 
  #Calculate the study day
  left_join(start_date) %>% 
  mutate(QSDY=ymd(QSDTC)-ymd(RFSTDTC) + 1) %>% 
  #Arrange by needed variables and create the QSSEQ variable
  arrange(STUDYID, USUBJID, QSCAT, QSSCAT, VISITNUM, QSTESTCD) %>% 
  group_by(USUBJID) %>% 
  mutate(QSSEQ=1:n()) %>% 
  ungroup() %>% 
  select(order) %>% 
  #Convert QSSTRESN to double
  mutate_at(vars(QSSTRESN), as.double) %>% 
  #If QSSTRESC is NA, replace it with QSSTRESN value
  mutate_at(vars(QSSTRESC), list(~case_when(is.na(.) ~ as.character(QSSTRESN),
                                            TRUE ~ .)))

#Export our lookup tables and write this as a function
#write_csv(qstestcd_lu, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testcode_lu.csv")
#write_csv(qsstresc_lu, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testresult_lu.csv")
#write_csv(df_order, "J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/column_order.csv")

transform_sdtm<-function(x){
  #This will need to be modified to read in the lookup tables from the location the repo is on your machine
  qstestcd_lu<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testcode_lu.csv")
  qsstresc_lu<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/testresult_lu.csv", col_types=cols(.default = "c"))
  df_order<-read_csv("J:/BACPAC/Users/WardPatrick/SDTM_Wide_Transformation/Lookups/column_order.csv")
  order<-df_order$order
  wide_derived<-x %>% 
    mutate(GAD2RAW=GAD01 + GAD02,
           PRANX4AR=EDANX01 + EDANX40 + EDANX41 + EDANX53,
           PRDEP4AR=EDDEP04 + EDDEP06 + EDDEP29 + EDDEP41,
           PRPI4AR=PAININ9 + PAININ22 + PAININ31 + PAININ34,
           PRPF6BR=PFA11 + PFA21 + PFA23 + PFA53 + PFC12 + PFB1,
           PRSD6AR=SLEEP109 + SLEEP116 + SLEEP20 + SLEEP44 + SLEEP108 + SLEEP72,
           PCSRAW=PCS4 + PCS5 + PCS6 + PCS10 + PCS11 + PCS13,
           PHQ2RAW=PHQ01 + PHQ02,
           WDSPPNUM=PNAREA1 + PNAREA2 + PNAREA3 + PNAREA4 + PNAREA5 + PNAREA6 + PNAREA7,
           PEGSCORE=(PEG1 + PEG2 + PEG3)/3) %>% 
    #Now all of the T-scores
    left_join(t_score[t_score$MEASURE=="PRMANX", c("RAWSCORE", "TSCORE")], by=c("PRANX4AR"="RAWSCORE")) %>% 
    rename(PRANX4AT=TSCORE) %>% 
    left_join(t_score[t_score$MEASURE=="PRMDEP", c("RAWSCORE", "TSCORE")], by=c("PRDEP4AR"="RAWSCORE")) %>% 
    rename(PRDEP4AT=TSCORE) %>% 
    left_join(t_score[t_score$MEASURE=="PRMPI", c("RAWSCORE", "TSCORE")], by=c("PRPI4AR"="RAWSCORE")) %>% 
    rename(PRPI4AT=TSCORE) %>% 
    left_join(t_score[t_score$MEASURE=="PRMPHF", c("RAWSCORE", "TSCORE")], by=c("PRPF6BR"="RAWSCORE")) %>% 
    rename(PRPF6BT=TSCORE) %>% 
    left_join(t_score[t_score$MEASURE=="PRMSD", c("RAWSCORE", "TSCORE")], by=c("PRSD6AR"="RAWSCORE")) %>% 
    rename(PRSD6AT=TSCORE)
  
  wide_derived %>% 
    pivot_longer(cols=c(-USUBJID, -VISIT, -QSDTC),
                 names_to="QSTESTCD",
                 values_to="QSSTRESN") %>% 
    mutate(STUDYID=str_split(USUBJID, "-", simplify=TRUE)[1],
           VISITNUM=str_split(VISIT, " ", simplify=TRUE)[,2],
           DOMAIN="QS") %>% 
    #Need to merge in QSCAT, QSSCAT, QSTEST, and QSEVLNT based on QSTESTCD
    left_join(qstestcd_lu) %>% 
    #Round and convert QSSTRESN to character to merge the other information in
    mutate_at(vars(QSSTRESN), list(~as.character(round(., digits=2)))) %>% 
    #Need to merge in QSSTRESC based upon QSTESTCD and QSSTRESN
    left_join(qsstresc_lu) %>% 
    #Calculate the study day
    left_join(start_date) %>% 
    mutate(QSDY=ymd(QSDTC)-ymd(RFSTDTC) + 1) %>% 
    #Arrange by needed variables and create the QSSEQ variable
    arrange(STUDYID, USUBJID, QSCAT, QSSCAT, VISITNUM, QSTESTCD) %>% 
    group_by(USUBJID) %>% 
    mutate(QSSEQ=1:n()) %>% 
    ungroup() %>% 
    select(order) %>% 
    #Convert QSSTRESN to double
    mutate_at(vars(QSSTRESN), as.double) %>% 
    #If QSSTRESC is NA, replace it with QSSTRESN value
    mutate_at(vars(QSSTRESC), list(~case_when(is.na(.) ~ as.character(QSSTRESN),
                                              TRUE ~ .)))
  
}

#Example using function
qs_transform<-wide %>% 
  transform_sdtm()

















