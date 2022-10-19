library(tidyverse)
library(dplyr)
library(tableone)
library(lubridate)
library(noacsr)
library(glm2)
library(ggplot2)

setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")
source("functions.R")

# 1. Import the data
#
# Read the dataset and codebook from github
#  data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-full-dataset-v1.csv")
# # # data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-limited-dataset-v1.csv")
#  codebook <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/codebook-titco-I-v1.csv")


# #för jag vill kunna köra offline
# save(data, file = "data.RData")
# save(codebook, file = "codebook.RData")

load("data.RData")
load("codebook.RData")


outcome=data %>% 
  select(doi, toi, doa, toa, doar, toar,  dom_1, tom_1,dom_2, tom_2,dodd, todd,tran,spo2_o2_1) %>% 
  # Combine date and time to make a datetime object
  mutate(injury_time = as_datetime(paste0(doi, toi))) %>%
  mutate(arrival_time = as_datetime(paste0(doa, toa))) %>%
  mutate(admission_time = as_datetime(paste0(doa, toa))) %>% 
  mutate(firstsurvey_time = as_datetime(paste0(dom_1, tom_1))) %>%
  mutate(secondsurvey_time = as_datetime(paste0(dom_2, tom_2))) %>%
  mutate(time_of_death = as_datetime(paste0(dodd, todd))) %>% 
  # Calculate difference in hours between arrival and death
  mutate(time_to_death = round(as.numeric(time_of_death - arrival_time, units = "hours"),0))%>%
  
  #här börjar min påbyggnad
  mutate(time_to_first = round(as.numeric(firstsurvey_time-arrival_time , units = "hours"),2))%>%
  mutate(time_between_surveys = round(as.numeric(secondsurvey_time - firstsurvey_time, units = "hours"),2)) %>%
#vi tar dessa åt sidan, för vi vill bara göra detta på de som inte är transfererade, eftersom de snurrat förbi ett annat sjukhus blir tiden från injury till arrival missvisande
  # approach 1, blev kortare
# filter(tran %in% c("No"))%>%
#   mutate(injury_time_direct = as_datetime(paste0(doi, toi)))  %>%
#   mutate(arrival_time_direct = as_datetime(paste0(doar, toar))) %>% 
#   mutate(firstsurvey_time_direct = as_datetime(paste0(dom_1, tom_1)))
mutate(stillalive_6h = case_when(time_to_death > 6 ~ 1,
                                 time_to_death < 6 ~ 0)) %>%
  #återkom fatta hur du filtrerar på detta 
  mutate(delay2 = round(as.numeric(arrival_time-injury_time , units = "hours"),0))%>% 
  mutate(delay23 = round(as.numeric(firstsurvey_time-injury_time , units = "hours"),0))%>%
  mutate(delay2ifdirect = case_when(str_detect(tran, "No") & delay2<72 ~ delay2)) # %>%

#addera kolumnerna
data=data.frame( data, outcome)

#lite rensning
# Updates to the dataset
data <- data %>%
  # Convert age to numeric
  mutate(age = parse_number(age)) %>%
  #applicera överenskomna filtreringar
#  filter(data$stillalive==1)%>%
  #... återkom
  
  mutate(rts_sbp1 = case_when(sbp_1 ==0 & hr_1 <1 & sbp_1 !='NA'  ~ 0,
                                 sbp_1 < 49 & sbp_1 !='NA' ~ 1,
                                 sbp_1 < 75 & sbp_1 !='NA' ~ 2,
                              sbp_1 < 89 & sbp_1 !='NA' ~ 3,
                              sbp_1 < 300 & sbp_1 !='NA' ~4)) %>% 
  
  mutate(rts_sbp2 = case_when(sbp_2 ==0 & hr_2<1  ~ 0,
                              sbp_2 < 49  & sbp_2 !='NA' ~ 1,
                              sbp_2 < 75  & sbp_2 !='NA' ~ 2,
                              sbp_2 < 89   & sbp_2 !='NA' ~ 3,
                              sbp_2 < 300 & sbp_2 !='NA' ~ 4)) %>% 
  
  mutate(rts_rr1 = case_when(rr_1 ==0   ~ 0,
                              rr_1 < 5   ~ 1,
                              rr_1 < 9   ~ 2,
                              rr_1 > 29   ~ 3)) %>% 
  
  mutate(rts_rr2 = case_when(rr_2 ==0   ~ 0,
                             rr_2 < 5   ~ 1,
                             rr_2 < 9   ~ 2,
                             rr_2 > 29   ~ 3)) %>% 
  
  #får in transfer 
  mutate(ambulancefromthescene = case_when(str_detect(tran, "No") & str_detect(mot, "Ambulance") ~ 1)) %>%                         

  mutate(tranclass= case_when(ambulancefromthescene==1   ~ 1,
                              str_detect(mot, "Police")  ~ 2,
                              ambulancefromthescene==0   ~ 3,
                              str_detect(mot, "Carried by man")   ~ 3,
                              str_detect(mot, "Other")   ~ 3,
                              str_detect(mot, "Private car")   ~ 3,
                              str_detect(mot, "Taxi, motor rickshaw")   ~ 3    )) %>%

  mutate(bl_rec= case_when(ubr_1>1   ~ "Yes",
                           ubr_1<1.5   ~ "No")) %>%
  
  mutate(sc_hi= case_when(sc>3   ~ "Yes",
                          sc<3.000001   ~ "No")) %>%
  
  # logical or för paste ||
  
  mutate(gcs_v_1_class= case_when(str_detect(gcs_v_1, "1")    ~ 1,
                                  str_detect(gcs_v_1, "1v")   ~ 2,
                                  str_detect(gcs_v_1, "2")    ~ 3,
                                  str_detect(gcs_v_1, "3" )   ~ 4,
                                  str_detect(gcs_v_1, "4")    ~ 4, 
                                  str_detect(gcs_v_1, "5" )   ~ 4,
                                  str_detect(gcs_v_1, "6" )   ~ 4)) %>%
  
  mutate(gcs_m_1_class= case_when(str_detect(gcs_m_1, "1")    ~ 1,
                                  str_detect(gcs_m_1, "2")    ~ 3,
                                  str_detect(gcs_m_1, "3" )   ~ 4,
                                  str_detect(gcs_m_1, "4")    ~ 4, 
                                  str_detect(gcs_m_1, "5" )   ~ 4,
                                  str_detect(gcs_m_1, "6" )   ~ 4)) %>%
  
  mutate(gcs_e_1_class= case_when(str_detect(gcs_e_1, "1")    ~ 1,
                                  str_detect(gcs_e_1, "1v")   ~ 2,
                                  str_detect(gcs_e_1, "2")    ~ 3,
                                  str_detect(gcs_e_1, "3" )   ~ 4,
                                  str_detect(gcs_e_1, "4")    ~ 4, 
                                  str_detect(gcs_e_1, "5" )   ~ 4,
                                  str_detect(gcs_e_1, "6" )   ~ 4)) %>%
  
  #second survey. du ,åste pricka allt så du kan få NA
  mutate(gcs_v_2_class= case_when(str_detect(gcs_v_2, "1")    ~ 1,
                                  str_detect(gcs_v_2, "1v")   ~ 2,
                                  str_detect(gcs_v_2, "2")    ~ 3,
                                  str_detect(gcs_v_2, "3" )   ~ 4,
                                  str_detect(gcs_v_2, "4")    ~ 4, 
                                  str_detect(gcs_v_2, "5" )   ~ 4,
                                  str_detect(gcs_v_2, "6" )   ~ 4)) %>%
  
  mutate(gcs_m_2_class= case_when(str_detect(gcs_m_2, "1")    ~ 1,
                                  str_detect(gcs_m_2, "2")    ~ 3,
                                  str_detect(gcs_m_2, "3" )   ~ 4,
                                  str_detect(gcs_m_2, "4")    ~ 4, 
                                  str_detect(gcs_m_2, "5" )   ~ 4,
                                  str_detect(gcs_m_2, "6" )   ~ 4)) %>%
  
  mutate(gcs_e_2_class= case_when(str_detect(gcs_e_2, "1")    ~ 1,
                                  str_detect(gcs_e_2, "1v")   ~ 2,
                                  str_detect(gcs_e_2, "2")    ~ 3,
                                  str_detect(gcs_e_2, "3" )   ~ 4,
                                  str_detect(gcs_e_2, "4")    ~ 4, 
                                  str_detect(gcs_e_2, "5" )   ~ 4,
                                  str_detect(gcs_e_2, "6" )   ~ 4)) %>%
  
# Add id column to the dataset
#  mutate(id = row_number()) %>% #jag byter ut, finns seqn redan
  # Add our outcome, if patient has a recorded length in the ICU then treated_icu = 1
  mutate(treated_icu = case_when(licu > 24 & hos ==6273 ~ 1,
                                 licu < 25 & hos ==6273 ~ 0,
                                 licu > 0 & hos ==7215 ~ 1,
                                 licu > 0 & hos ==7842 ~ 1,
                                 licu > 0 & hos ==8264 ~ 1,
                                 licu == 0 & hos ==7215 ~ 0,
                                 licu == 0 & hos ==7842 ~ 0,
                                 licu == 0 & hos ==8264 ~ 0)) %>% #var det inte det här vi sa?
  # mutate(treated_icu = case_when(licu > 24 ~ 1,
  #                                licu < 25 ~ 0))
mutate(spO2_1_wO2 = case_when(str_detect(spo2_o2_1, "Yes")  ~ spo2_1))%>%
mutate(spO2_1_woO2 = case_when(str_detect(spo2_o2_1, "Yes")  ~ spo2_1))%>%

mutate(spO2_2_wO2 = case_when(str_detect(spo2_o2_2, "Yes")  ~ spo2_2))%>%
  mutate(spO2_2_woO2 = case_when(str_detect(spo2_o2_2, "Yes")  ~ spo2_2))

#måste nu tillfogar dessa som variabler til codebook
newnames<-data.frame("delay2ifdirect","rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","ambulancefromthescene","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class","spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2")
newlabels<-data.frame("delay2ifdirect","revised trauma score sbp1","revised trauma score sbp2","revised trauma score rr1","revised trauma score rr2","ambulancefromthescene","tranclass","blood received 1st hour", "serum creatinine high","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class","spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2")
newtypes<-data.frame("quantitative","quantitative","quantitative","quantitative","quantitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","quantitative","quantitative","quantitative","quantitative")
#men jag vet inte hur jag lägger till nya rader så...
headersnew = data.frame(matrix(,nrow = nrow(codebook)+19, ncol = ncol(codebook)-1))
headersnew[1:193,1:ncol(codebook)]<-data.frame(codebook)
colnames(headersnew)<-(colnames(codebook))
#start=220 #nrow(headerproperties)+1
#endat1=233 #nrow(headerproperties)+3
count=0
for (a in 1:19){
  count=count+1
  headersnew[nrow(codebook)+count,1]=newnames[1,count]
  headersnew[nrow(codebook)+count,2]=newlabels[1,count]
  headersnew[nrow(codebook)+count,6]=newtypes[1,count]
}
count=0
#fixar att type hamnat på note i dessa rader
for (a in 69:193){
  count=count+1
  misplacedtype=headersnew[a,5]
  headersnew[a,6]=misplacedtype

}

codebook=headersnew

# 3. Set seed and split into training and test samples.
# The seed allows us to always get the same randomized sample when we run the code.
set.seed(42)
# Use 60% of the data for training
train_data <- data %>% sample_frac(.60)
# Use the remaining 40% as test data
test_data <- anti_join(data, train_data, by = 'seqn')
# Optional, Verify that no rows overlap, should return 0
count(train_data %>% filter(seqn %in% test_data$seqn))

# 4. Compare variables between ICU and non-ICU patients
# Define the categorical and continuous variables
cat_variables <- codebook %>% filter(type %in% "qualitative") %>%
  # We filter out pid, not to include that in our analysis
  filter(name != "pid") %>% pull(name)
cont_variables <- codebook %>% filter(type %in% "quantitative") %>%
  # We filter out licu since this is our outcome
  filter(!name %in% c("licu")) %>% pull(name)

# Look at the data using table one, jag fattar inte hur jag kan se om inte variabelnamn
look_cont=tableone::CreateContTable(data = train_data, vars = cont_variables, strata = "treated_icu")
look_cat=tableone::CreateCatTable(data = train_data, vars = cat_variables, strata = "treated_icu")

look_cont1=tableone::CreateContTable(data = test_data, vars = cont_variables, strata = "treated_icu")
look_cat1=tableone::CreateCatTable(data = test_data, vars = cat_variables, strata = "treated_icu")

#nu lägger vi på själva analysen. Vi har alla nya skapade parametrar exkl ambulansransporten med

predictors=data
#"rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2"

#samt de gamla
#..... kolla i saker jag vill ta upp
#died, NISS, Operation 1a timmen, Mode of injury, transfer, age sex

#predictors=data.frame(data["rts_sbp1”],data[”rts_sbp2”],data[”rts_rr1”],data[”rts_rr2”],data[”tranclass”],data[”bl_rec”],data[”sc_hi”],data[”gcs_v_1_class”],data[”gcs_m_1_class”],data[”gcs_e_1_class”],data[”gcs_v_2_class”],data[”gcs_m_2_class”],data[”gcs_e_2_class”], data["spO2_1_wO2"],data[”spO2_1_woO2”],data[”spO2_2_wO2”],data[”spO2_2_woO2"],data["NISS"],data["age"],data["sex"],data["age"],data["op_1"],data["moi"], data["tran"],data["died"])  

predictors = data  %>% select("rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died")

#titta på kont värdena och bedöm
continuous <-select_if(predictors, is.numeric)
summary(continuous)

# Histogram with kernel density curve, exempel med ålder
ggplot(continuous, aes(x = age)) +
geom_density(alpha = .2, fill = "#FF6666")


# Select categorical column
cat <- data.frame(select_if(predictors, is.character))
ncol(cat)

# Create graph for each column
graph <- lapply(names(cat),
                function(x) 
                  ggplot(cat, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

#addera outputten till prediktor data framen
treated_icu=data$treated_icu
tran=data$tran
predictors$treated_icu=treated_icu

#funkar inte
model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+ spO2_1_wO2+spO2_1_woO2+spO2_2_wO2+spO2_2_woO2+ niss+ age+ sex+ age+ ot_1+ moi+ tran+ died,data=predictors, family=binomial, na.omit)

#funkar
model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec, age,sex,data=predictors, family=binomial)

#funkar inte, men errormeddelandet säger det beror på att vi i ot_1 bara har ett unikt värde vilket inte stämmer vi har yes och no
model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec, age,sex,ot_1,data=predictors, family=binomial)

#funkar inte, eftersom jag vet inte längre, först var error 
# contrasts can be applied only to factors with 2 or more levels
#nu är det istället
#'weights' must be a numeric vector
# men det som är förvirrande är att den accepterar bl_rec som är en Yes/no, men inte Sc_hi,ot_1 eller sex
model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec,sc_hi,data=predictors, family=binomial)

map(predictors, ~sum(is.na(.)))