setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")


library(tidyverse)
library(dplyr)
library(tableone)

headerproperties <- read.csv("headerpropertiestrans.csv") #från rad 70 har datan fär "type" hamnat i "note"-kolumnen. dessutom är spo2_o2_1 och 2 markerade som kvantitativa men de är Y/n så det har jag löst manuellt
titcodata <- read.csv("titcodatafull.csv") #untampered version
titcodata <- read.csv("titcodatacleaned.csv") #cleaned version, see separate script


#lägg till timerkolumnerna
#det tog för lång tid att googla hur R hanterar HH:MM, så istället:

 ss= data.frame(matrix(":00", nrow=nrow(titcodata), ncol =1 ))
 hms=paste(titcodata$toi[1:nrow(titcodata)],ss[1:nrow(titcodata),1],sep= "")
 hmstoi=data.frame(hms)
 hms=paste(titcodata$toar[1:nrow(titcodata)],ss[1:nrow(titcodata),1],sep= "")
 hmstoa=data.frame(hms)
 hms=paste(titcodata$tom_1[1:nrow(titcodata)],ss[1:nrow(titcodata),1],sep= "")
 hmstom1=data.frame(hms)
# for (a in 1:nrow(titcodata)){
# if(is.na(titcodata$toi[a])) {titcodata$toi[a]=FALSE} else {if(titcodata$toi[a]) {titcodata$toi[a]}}
# hms=paste(titcodata$toi[a],ss[a,1],sep= "")
# }

hoursinj=substr(titcodata$toi, 1, 2)
hoursinj=as.numeric(hoursinj)
hoursinj=data.frame(hoursinj)
minutesinj=substr(titcodata$toi, 4, 5)
minutesinj=as.numeric(minutesinj)
minutesinj=data.frame(minutesinj)
hoursarr=substr(titcodata$toar, 1, 2)
hoursarr=as.numeric(hoursarr)
hoursarr=data.frame(hoursarr)
minutesarr=substr(titcodata$toar, 4, 5)
minutesarr=as.numeric(minutesarr)
minutesarr=data.frame(minutesarr)
hours1st=substr(titcodata$tom_1, 1, 2)
hours1st=as.numeric(hours1st)
hours1st=data.frame(hours1st)
minutes1st=substr(titcodata$tom_1, 4, 5)
minutes1st=as.numeric(minutes1st)
minutes1st=data.frame(minutes1st)

dayinj=substr(titcodata$doi, 1, 2)
dayinj=as.numeric(dayinj)
dayinj=data.frame(dayinj)
dayarr=substr(titcodata$doa, 1, 2)
dayarr=as.numeric(dayarr)
dayarr=data.frame(dayarr)
day1st=substr(titcodata$dom_1, 1, 2)
day1st=as.numeric(day1st)
day1st=data.frame(day1st)

delay2=(hoursarr+minutesarr/60)-(hoursinj+minutesinj/60)

delay3=(hours1st+minutes1st/60)-(hoursarr+minutesarr/60)

delay23=(hours1st+minutes1st/60)-(hoursinj+minutesinj/60)

delaypre <- data.frame(hmstoi,hmstoa,hmstom1,titcodata$tran,delay2,delay3,delay23)


#nollar alla som är negativa och delar med de som väntat över midnatt
for (a in 1:nrow(titcodata)){
  #de som transfererats har olika doi mot arr
if(is.na(titcodata$tran[a])) {titcodata$tran[a]='No'}
if (titcodata$tran[a]=='No'){

  #slipp NA strul
  if(is.na(dayinj[a,1])) {dayinj[a,1]=FALSE} else {if(dayinj[a,1]) {dayinj[a,1]}}
  if(is.na(day1st[a,1])) {day1st[a,1]=FALSE} else {if(day1st[a,1]) {day1st[a,1]}}
  if(is.na(dayarr[a,1])) {dayarr[a,1]=FALSE} else {if(dayarr[a,1]) {dayarr[a,1]}}
  if(is.na(delay2[a,1])) {delay2[a,1]=FALSE} else {if(delay2[a,1]) {delay2[a,1]}}
  if(is.na(delay23[a,1])) {delay23[a,1]=FALSE} else {if(delay23[a,1]) {delay23[a,1]}}
  if(is.na(delay3[a,1])) {delay3[a,1]=FALSE} else {if(delay3[a,1]) {delay3[a,1]}}


  if  (dayinj[a,1] != dayarr[a,1]) {
  delay2[a,1]=delay2[a,1]+24
  }
  #}}


  if (dayarr[a,1] != day1st[a,1]) {
    delay3[a,1]=delay3[a,1]+24
  }


  if (dayinj[a,1] != day1st[a,1]) {
    delay23[a,1]=delay23[a,1]+24
  }
  
  if (delay23[a,1]<0){delay23[a,1]=0 }
  if (delay3[a,1]<0){delay3[a,1]=0 }
  if (delay2[a,1]<0){delay2[a,1]=0 }

}
}

delays <- data.frame(hmstoi,hmstoa,hmstom1,titcodata$tran,delay2,delay3,delay23)

for (a in 1:nrow(titcodata)){
  #de som transfererats har olika doi mot arr
  howmanytrans=0
  if (titcodata$tran[a]=='Yes'){
    howmanytrans=howmanytrans+1
    if(is.na(day1st[a,1])) {day1st[a,1]=FALSE} else {if(day1st[a,1]) {day1st[a,1]}}
    if(is.na(dayarr[a,1])) {dayarr[a,1]=FALSE} else {if(dayarr[a,1]) {dayarr[a,1]}}
    
      delay2[a,1]=NA #fult

    if (dayarr[a,1] != day1st[a,1]) {
      delay3[a,1]=delay3[a,1]+24
    }

      delay23[a,1]=NA

  }

  # if(is.na(delay2[a,1])) {delay2[a,1]=FALSE} else {if(delay2[a,1]) {delay2[a,1]}}
  # if(is.na(delay23[a,1])) {delay23[a,1]=FALSE} else {if(delay23[a,1]) {delay23[a,1]}}
  # if(is.na(delay3[a,1])) {delay3[a,1]=FALSE} else {if(delay3[a,1]) {delay3[a,1]}}
  

}


delays <- data.frame(hmstoi,hmstoa,hmstom1,titcodata$tran,delay2,delay3,delay23)

delaytable<- data.frame(delay2,delay3,delay23)
colnames(delaytable)<-data.frame("delay2","delay3","delay23")

newnames<-data.frame("delay2","delay3","delay23")
newlabels<-data.frame("delay inj to arrival hospital (NA if transferred)","delay arr to 1st assessment","delay inj to 1st assessment (NA if transferred)")
newtypes<-data.frame("quantitative","quantitative","quantitative")
#men jag vet inte hur jag lägger till nya rader så...
headersnew = data.frame(matrix(, nrow = nrow(headerproperties)+3, ncol = ncol(headerproperties)))
headersnew[1:193,1:9]<-data.frame(headerproperties)
colnames(headersnew)<-(colnames(headerproperties))
start=nrow(headerproperties)+1
endat1=nrow(headerproperties)+3
count=0
for (a in start:endat1){
  count=count+1
  headersnew[nrow(headerproperties)+count,1]=newnames[1,count]
  headersnew[nrow(headerproperties)+count,2]=newlabels[1,count]
  headersnew[nrow(headerproperties)+count,6]=newtypes[1,count] 
}
#headersnew=data.frame(headerproperties,newheaders)

#tillfoga dem i titco och headerprop 
titconew=data.frame(titcodata,delaytable)
colnames(titconew)<-colnames(titcodata)
colnames(titconew)[195] <-"delay2"
colnames(titconew)[196]<-"delay3"
colnames(titconew)[197]<-"delay23"

save(headersnew, file = "headersnew.RData")
save(titconew, file = "titconew.RData")
write.csv(headersnew, file = "headersnew.csv")
write.csv(titconew, file = "titconew.csv")

#nu använder vi dem from here on out
titcodata<- read.csv("titconew.csv")
headerproperties<- read.csv("headersnew.csv")

#vi måste hantera att age är numeric och quantiativ men >89 förekommer
#acceptableage<-data.frame(0:89)
titcodata=filter(titcodata, age !=">89")
#resten har lästs som char pga tecknet, måste bytas till num
titcodata$age <- as.numeric(titcodata$age)

#ur headerprops, filtrera fram alla rader i type-kolumnen som är är c("qualitative", "quantitative")), och skapa en array av motsvarande rader i name-kolumnen
quantcolname <- headerproperties %>% filter(type %in% c("quantitative")) %>% select(name)
df_t<- headerproperties %>% filter(type %in% c("quantitative")) %>% select(label)
#name är header-raden i titco
# # Välj kolumner ur datat baserat på listan av variabler
#name-arrayen variables kan du nu applicera på titcodatan, och välja alla kolumner som finns nämnd i arrayen. variables$name byter objekttyp till chr 1:56 som måste till. all_of gör inget
quantcols =titcodata %>% select(all_of(quantcolname$name))

licu=filter(titcodata, licu >0.5)
nolicu=filter(titcodata, licu <0.5)


quantcolnametrans<-data.table::transpose(quantcolname)

#välj kvant kolumnerna (jag måste speca age som muneric igen, vet inte varför)
quantLicu <- licu[, which((names(licu) %in% quantcolnametrans)==TRUE)]
#quantLicu[,1] <- as.numeric(quantLicu[,1])
quantNolicu <- nolicu[, which((names(nolicu) %in% quantcolnametrans)==TRUE)]
#quantNolicu[,1] <- as.numeric(quantNolicu[,1])
quantTitco <- titcodata[, which((names(titcodata) %in% quantcolnametrans)==TRUE)]
#quantTitco[,1] <- as.numeric(titcodata[,1])

#gör stat analysen
quantstatlicu=CreateTableOne(data = quantLicu,includeNA=FALSE)
quantstatnolicu=CreateTableOne(data = quantNolicu,includeNA=FALSE)

#gör t.test för alla kvant kolumner
columncount=nrow(quantcolname) #hur många kolumner ska vi rulla
pvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
tvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
for (a in 1:columncount) { 
  c=t.test(quantLicu[,a],quantNolicu[,a],na.action=na.exclude)
  pval=c[["p.value"]]
  tval=c[["statistic"]][["t"]]
  pvalues[a,1]=pval
  tvalues[a,1]=tval
  
  
  
}

#hämta alla mean och std ur listorna
b=quantstatlicu[["ContTable"]][["Overall"]]
myDf <- as.data.frame(b)
ICU_mean=myDf$mean
ICU_sigma=myDf$sd
myDfmeanlicu <- as.data.frame(ICU_mean)
myDfsdlicu <- as.data.frame(ICU_sigma)

b=quantstatnolicu[["ContTable"]][["Overall"]]
myDf <- as.data.frame(b)
noICU_mean=myDf$mean
noICU_sigma=myDf$sd
myDfmeannolicu <- as.data.frame(noICU_mean)
myDfsdnolicu <- as.data.frame(noICU_sigma)
mean_diff=noICU_mean-ICU_mean
mean_diffdf<- as.data.frame(mean_diff)

table5<-data.frame(df_t,myDfmeanlicu,myDfsdlicu,myDfmeannolicu,myDfsdnolicu,pvalues,tvalues)
colnames(table5)<-data.frame("variables","ICU mean","ICU sigma","no-ICU mean","no-ICU sigma","p-värde","t-värde")
write.csv(table5, file = "table5.csv")