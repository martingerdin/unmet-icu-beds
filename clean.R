library(noacsr)
library(tidyverse)
library(dplyr)
setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")

headerproperties <- read.csv("headerpropertiestrans.csv") #från rad 70 har datan fär "type" hamnat i "note"-kolumnen. det har jag löst manuellt
titcodata <- read.csv("titcodatafull.csv") #untampered version


#filter
#ur headerprops, filtrera fram alla rader i type-kolumnen som är är c("qualitative", "quantitative")), och skapa en array av motsvarande rader i name-kolumnen
# variables <- headerproperties %>% filter(type %in% c("qualitative", "quantitative")) %>% select(name)
#name är header-raden i titco
# # Välj kolumner ur datat baserat på listan av variabler
#name-arrayen variables kan du nu applicera på titcodatan, och välja alla kolumner som finns nämnd i arrayen. variables$name byter objekttyp till chr 1:56 som måste till. all_of gör inget
# titcodata %>% select(all_of(variables$name))

#remove patients who died between arrival and admission
liveonadmission<- titcodata %>% filter(incl %in% c("0","1", "3"))
#skriv över

titcodata=liveonadmission

#ersätt alla fysiologiskt orimliga nollor i kvantitativa datat med NA
#steg ett vilka är det?
cantbezero=data.frame("hb","hc","sc","bun") #välj manuellt ut vilka kvawnt variabler som inte någonsin kan vara noll Hb, serumkreatinin, hematokrit, blod urea nitrogen
cantbezerotrans<-data.table::transpose(cantbezero) #transponera
colnames(cantbezerotrans)<-data.frame("name") #sätt dit headern manuellt så den fattar

#filtrera fram motsvarande kolumner
cantebezeromat=titcodata %>% select(all_of(cantbezerotrans$name))
cantebezeromat[cantebezeromat==0] = NA
#och skriv över. kolumn för kolumn det borde finnas ett lättare sätt?
titcodata["hb"]=cantebezeromat["hb"]
titcodata["hc"]=cantebezeromat["hc"]
titcodata["sc"]=cantebezeromat["sc"]
titcodata["bun"]=cantebezeromat["bun"]

#här ska inga rader bort

#ersätt alla konditionellt orimliga nollor i kvantitativa datat med NA 
#en orimlig nolla är noll blodtryck om puls
# cantbezero=data.frame("sbp_1") 
# cantbezerotrans<-data.table::transpose(cantbezero) #transponera
# colnames(cantbezerotrans)<-data.frame("name") #sätt dit headern manuellt så den fattar

#workaround för att if inte fattar NA
#skapa temporära kolumner
colhr=titcodata["hr_1"]
colsbp=titcodata["sbp_1"]

#byt ut NA mot vad vi vill i dem. 0 om vi vill vara skeptiska och slänga ut värden där HR är noll och SBP är okänt eller vice versa, eller != 0 om vi vill tro att om HR är noll men SBP okänt så är nog SBP som väntat (alltså noll)
colhr[is.na(colhr)] = 1
colsbp[is.na(colsbp)] = 1
hits=0
for (a in 2:nrow(titcodata)) {
  # rowhr=titcodata[a,"hr_1"]
  # rowsbp=titcodata[a,"sbp_1"]
  rowhr=colhr[a,1]
  rowsbp=colsbp[a,1]
  if (rowhr==0 & rowsbp >0) {   #om jag har sbp trots ingen HR
    #colhr[a,1]=NA    
    titcodata[a,"hr_1"]=-4
    hits=hits+1
  }
  if (rowhr>0 & rowsbp ==0) {   #om jag saknar blodtryck trots puls
    #colsbp[a,1]=NA
    titcodata[a,"sbp_1"]=-4
    hits=hits+1
  }
}

#gör om det för andra mätningen:
# colhr=titcodata["hr_2"]
# colsbp=titcodata["sbp_2"]
# 
# colhr[is.na(colhr)] = 1
# colsbp[is.na(colsbp)] = 1

#men vi kan inte göra samma för mätning 2 för den är inte obligat så
# 1. inte så viktig även om den är misstänkt.
# 2. hälften av pat har NA för den inte är obligat. så filtrera bort pat med suspekta värden men låta pat utan värden vara kvar?
# 
# for (a in 2:length(titcodata)) {
#   rowhr=colhr[a,1]
#   rowsbp=colsbp[a,1]
#   if (rowhr==0 & rowsbp >0) {   #om jag har sbp trots ingen HR
#     titcodata[a,"hr_2"]=NA
#   }
#   if (rowhr>0 & rowsbp ==0) {   #om jag saknar blodtryck trots puls
#     titcodata[a,"sbp_2"]=NA
#   }
# }

#nu ska rader bort
expectedHR_1= filter(titcodata, hr_1 >-1)
titcodata=expectedHR_1

expectedSBP_1= filter(titcodata, sbp_1 >-1)
titcodata=expectedSBP_1

#men vi kan inte göra samma för mätning 2 för den har bara hälften av pat fått
#återkom

# expectedHR_2= filter(titcodata, hr_2 >-1)
# titcodata=expectedHR_2
# 
# expectedSBP_2= filter(titcodata, sbp_2 >-1)
# titcodata=expectedSBP_2

#vi har en pat som arrived walking men som hade GCS 3 hur ska man tolka det?
#  det finns pat som är intuberade "before arrival" men som inte är noterade som transferred från ett annat sjukhus. De har kommit med motor rickshaw. Är det rimligt att anta att de är transferred från ett sjukhus iallafall för vem har annars intuberat dessa patienter?
# ändra till vad man tror gäller, eller exkludera?


# datatvätt FRÅGA: quantitativa variablerna har många NA som jag tror egentligen är NO, ex om pat har haft syrgas vid spO2 mätning. kan jag tänka lite fritt här när det känns uppenbart eller ska jag förankra? också finns det ex obs där numeriska värdet för spO2 saknas men ändå data yes/no om pat fått syrgas och jag vet inte om sån data är meningsfull? 
  
colspo2_o2_1=titcodata["spo2_o2_1"]
colspo2_o2_1NA = is.na(colspo2_o2_1)
#eftersom if inte kan hantera NA som jag vill, gör en tillfällig kolumn och byt ut NA mot neg värden
colspo2_1=titcodata["spo2_1"]
colspo2_1[is.na(colspo2_1)] =-4
hits=0
for (a in 2:nrow(titcodata)) {
  #rowspo2_o2_1=colspo2_o2_1NA[a,"spo2_o2_1"]
  rowspo2_1=colspo2_1[a,1]
 # rowspo2_1=colsbp[a,"spo2_1"]
  if (colspo2_o2_1NA[a,1]==TRUE & rowspo2_1 >0) {   #& rowspo2_1 >0)
    titcodata[a,"spo2_o2_1"]="Yes"
    hits=hits+1
  }

}

# Kommentar: inför datatvätten, hur vill vi göra med  småbarnen? -->