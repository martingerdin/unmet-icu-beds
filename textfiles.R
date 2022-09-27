setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")

#setwd("C:/Users/maria/Downloads/unmet-ICU-beds")

library(tidyverse)
library(dplyr)
library(tableone)

for (b in 2:2) {
  titcodata <- read.csv("titcodatafull.csv")
  if (a==1) {
    licu=filter(titcodata, licu >0.5)
    titcodata=licu #fult
  }
  if (a==2) {
    nolicu=filter(titcodata, licu <0.5)
    titcodata=nolicu #fult
  }
  
  #build strings for the ICU codes
  
  resultmatrix = data.frame(matrix(, nrow = 3, ncol = 11))
  resultmatrixext = data.frame(matrix(, nrow = 1, ncol = 12))
  resultmatrixct = data.frame(matrix(, nrow = 1, ncol = 13))
  resultarray = data.frame(matrix(, nrow = 1, ncol = 11))
  
  #bygg först en liten dataframe med alla ICD koder
  #nej vi måste ha tre pga om jag vill använda dem sen för att filtrera ut kolumner accepterar inte r missing values och de är tre olika längder
  #jag behöver bygga en array inte en matris av resultaten här annars vill det inte förstå, så
  rowno=1 #counter
  loop=0
  uniqueoptot= data.frame(matrix(, nrow = 11*200, ncol =1 )) #resultatmatris, extra stor
  for (a in 1:11) {
    loop=loop+1
    xray=paste("xray_",a,"_icd",sep="")
    fast=paste("fast_",a,"_icd",sep="")
    op=paste("op_",a,"_icd",sep="")
    
    resultmatrix[1,a]=xray
    resultmatrix[2,a]=fast
    resultmatrix[3,a]=op
    
    #vilka unika har vi?
    uniqueop=unique(titcodata[op])
    startat=rowno
    endat=rowno+nrow(uniqueop)-1
    howmanyrows=nrow(uniqueop)
    uniqueoptot[startat:endat,1]<- data.frame(matrix(uniqueop[1:howmanyrows,1])) #data.frame(uniquexray[1:nrow(uniquexray),1])
    rowno=rowno+nrow(uniqueop)
    #DU VAR HÄR
    
  }
  
  uniqueexttot= data.frame(matrix(, nrow = 11*200, ncol =1 ))
  for (a in 1:12) {
    ext=paste("e_",a,"_icd",sep="")
    resultmatrixext[1,a]=ext
    
    uniqueext=unique(titcodata[ext])
    startat=rowno
    endat=rowno+nrow(uniqueext)-1
    howmanyrows=nrow(uniqueext)
    uniqueexttot[startat:endat,1]<- data.frame(matrix(uniqueext[1:howmanyrows,1])) #data.frame(uniquexray[1:nrow(uniquexray),1])
    rowno=rowno+nrow(uniqueext)
  }
  #extu=unique(uniqueexttot)
  
  uniquecttot= data.frame(matrix(, nrow = 11*200, ncol =1 ))
  for (a in 1:13) {
    ct=paste("ct_",a,"_icd",sep="")
    resultmatrixct[1,a]=ct
    
    uniquect=unique(titcodata[ct])
    startat=rowno
    endat=rowno+nrow(uniquect)-1
    howmanyrows=nrow(uniquect)
    uniquecttot[startat:endat,1]<- data.frame(matrix(uniquect[1:howmanyrows,1])) #data.frame(uniquexray[1:nrow(uniquexray),1])
    rowno=rowno+nrow(uniquect)
  }
  ctu=unique(uniquecttot)
  # 
  # resultmatrix[1,12]="e_12_icd"
  # resultmatrix[4,12]="ct_12_icd"
  # resultmatrix[4,13]="ct_12_icd"
  
  #ok. nu har vi en array per us-typ med alla unika icd-koder 
  #tex xray
  resultarray=resultmatrix[1,1:11]
  
  resultarraytrans<-data.table::transpose(resultarray) #transponera
  colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt
  
  resultmat=titcodata %>% select(all_of(resultarraytrans$name))
  
  occurences<-table(unlist(resultmat))
  xraydf <- data.frame(occurences)
  
  xraysort=print(xraydf[order(xraydf$Freq, decreasing = TRUE), ]   )
  
  
  #fast
  resultarray=resultmatrix[2,1:11]
  
  resultarraytrans<-data.table::transpose(resultarray) #transponera
  colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt
  
  resultmat=titcodata %>% select(all_of(resultarraytrans$name))
  
  occurences<-table(unlist(resultmat))
  fastdf <- data.frame(occurences)
  
  fastsort=print(fastdf[order(fastdf$Freq, decreasing = TRUE), ]   )
  
  #op 
  resultarray=resultmatrix[3,1:11]
  
  resultarraytrans<-data.table::transpose(resultarray) #transponera
  colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt
  
  resultmat=titcodata %>% select(all_of(resultarraytrans$name))
  
  occurences<-table(unlist(resultmat))
  opdf <- data.frame(occurences)
  
  opsort=print(opdf[order(opdf$Freq, decreasing = TRUE), ]   )
  
  #ct 
  resultarray=resultmatrixct[1,1:11]
  
  resultarraytrans<-data.table::transpose(resultarray) #transponera
  colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt
  
  resultmat=titcodata %>% select(all_of(resultarraytrans$name))
  
  occurences<-table(unlist(resultmat))
  ctdf <- data.frame(occurences)
  
  ctsort=print(ctdf[order(ctdf$Freq, decreasing = TRUE), ]   )
  
  #ext 
  resultarray=resultmatrixext[1,1:11]
  
  resultarraytrans<-data.table::transpose(resultarray) #transponera
  colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt
  
  resultmat=titcodata %>% select(all_of(resultarraytrans$name))
  
  occurences<-table(unlist(resultmat))
  extdf <- data.frame(occurences)
  
  extsort=print(extdf[order(extdf$Freq, decreasing = TRUE), ]   )
  
  presenttop50 <- data.frame(xraysort[1:50,1:2],fastsort[1:50,1:2],opsort[1:50,1:2],ctsort[1:50,1:2],extsort[1:50,1:2])
  # library(qpcR)
  # presentall <- qpcR:::cbind.na(xraysort,fastsort,opsort,ctsort,extsort)
  # library(dplyr)
  
  if (b==1) {
    write.csv(presenttop50,"presentICU.csv")
    #  write.csv(presentall,"presentICU.csv")
  }
  if (b==2) {
    write.csv(presenttop50,"presentnoICU.csv")
    #  write.csv(presentall,"presentallnoICU.csv")
  }
}


#okej nu har vi en matris med alla x-ray-kolumner
#borde kunna räkna nu
# loops=nrow(xrayu)
# for (b in 1:12) {
#   xrayval=xrayu[b,1]




