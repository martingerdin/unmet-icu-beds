setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")

#setwd("C:/Users/maria/Downloads/unmet-ICU-beds")

library(tidyverse)
library(dplyr)
library(tableone)

headerproperties <- read.csv("headerpropertiestrans.csv") #från rad 70 har datan fär "type" hamnat i "note"-kolumnen. dessutom är spo2_o2_1 och 2 markerade som kvantitativa men de är Y/n så det har jag löst manuellt
titcodata <- read.csv("titcodatafull.csv") #untampered version
icdcodes <- read.csv("icdcodes.csv") #icd bibba

for (b in 1:3) {
  titcodata <- read.csv("titcodatafull.csv")
  if (b==1) {
    licu=filter(titcodata, licu >0.5)
    titcodata=licu #fult
  }
  if (b==2) {
    nolicu=filter(titcodata, licu <0.5)
    titcodata=nolicu #fult
  }
  
  if (b==3) {
    #titcodata<- read.csv("titcodatafull.csv")
  }
  
  #build strings for the ICU codes
  
  resultmatrix = data.frame(matrix(, nrow = 3, ncol = 11))
  resultmatrixext = data.frame(matrix(, nrow = 1, ncol = 12))
  resultmatrixct = data.frame(matrix(, nrow = 1, ncol = 13))
  resultarray = data.frame(matrix(, nrow = 1, ncol = 11))
  
  #bygg först en liten dataframe med alla ICD koder
  #nej vi måste ha tre pga om jag vill använda dem sen för att filtrera ut kolumner accepterar inte r missing values och de är tre olika längder
  #jag behöver bygga en array inte en matris av resultaten här annars vill det inte förstå, så
  rownoxray=1 #counter
  rownofast=1
  rownoop=1
  loop=0
  uniquexraytot= data.frame(matrix(, nrow = 11*200, ncol =3 )) #resultatmatris, extra stor
  uniquefasttot= data.frame(matrix(, nrow = 11*200, ncol =3 )) #resultatmatris, extra stor
  uniqueoptot= data.frame(matrix(, nrow = 11*200, ncol =3 )) #resultatmatris, extra stor
  for (a in 1:11) {
    loop=loop+1
    xray=paste("xray_",a,"_icd",sep="")
    fast=paste("fast_",a,"_icd",sep="")
    op=paste("op_",a,"_icd",sep="")
    
    resultmatrix[1,a]=xray
    resultmatrix[2,a]=fast
    resultmatrix[3,a]=op
    
    #vilka unika har vi?
    uniquexray=unique(titcodata[xray])
    uniquefast=unique(titcodata[fast])
    uniqueop=unique(titcodata[op])
    #skapa en array med unika xray
    startatxray=rownoxray
    endatxray=rownoxray+nrow(uniquexray)-1
    howmanyrowsxray=nrow(uniquexray)
    uniquexraytot[startatxray:endatxray,1]<- data.frame(matrix(uniquexray[1:howmanyrowsxray,1])) #data.frame(uniquexray[1:nrow(uniquexray),1])
    rownoxray=rownoxray+nrow(uniquexray)
    #skapa en array med unika fast
    startatfast=rownofast
    endatfast=rownofast+nrow(uniquefast)-1
    howmanyrowsfast=nrow(uniquefast)
    uniquefasttot[startatfast:endatfast,1]<- data.frame(matrix(uniquefast[1:howmanyrowsfast,1])) #data.frame(uniquefast[1:nrow(uniquefast),1])
    rownofast=rownofast+nrow(uniquefast)
    #skapa en array med unika op
    startatop=rownoop
    endatop=rownoop+nrow(uniqueop)-1
    howmanyrowsop=nrow(uniqueop)
    uniqueoptot[startatop:endatop,1]<- data.frame(matrix(uniqueop[1:howmanyrowsop,1])) #data.frame(uniqueop[1:nrow(uniqueop),1])
    rownoop=rownoop+nrow(uniqueop)
    
  }
  xrayu=unique(uniquexraytot)
  fastu=unique(uniquefasttot)
  opu=unique(uniqueoptot)
  
  rowno=1
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
  extu=unique(uniqueexttot)
  
  rowno=1
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
  
  #notera att jag har ett fungerade paket att göra en data.frame genom att sätta ihop kolumner av olika längd, men när jag laddar libraryt så är det no turning back då slutar alla andra libraries fungera. så jag gör det en gång för att skapa en csv och måste jag starta om rstudio . därför har jag gjort det en gång och sen sparat allt jag vill ha som enskilda x:2 matriser för ICU och no-ICU
  # library(qpcR)
  # presentall <- qpcR:::cbind.na(xraysort,fastsort,opsort,ctsort,extsort)
  # library(dplyr)
  
  if (b==1) {
    write.csv(presenttop50,"presentICU.csv")
    #  write.csv(presentall,"presentICU.csv")
    xraysortICU=xraysort
    fastsortICU=fastsort
    opsortICU=opsort
    ctsortICU=ctsort
    extsortICU=extsort

  }
  if (b==2) {
    write.csv(presenttop50,"presentnoICU.csv")
    #  write.csv(presentall,"presentallnoICU.csv")
    xraysortnoICU=xraysort
    fastsortnoICU=fastsort
    opsortnoICU=opsort
    ctsortnoICU=ctsort
    extsortnoICU=extsort
  }
  
  # vi måste kanske ha en för hela titcosetet eftersom samma ICD-koder inte nödvändigtvis förekommer i ICU och non-ICU och för nästa steg måste alla ICD-koder finnas i båda subsets för att det ska funka (tror jag)
  if (b==3) {
  write.csv(presenttop50,"presentnoICU.csv")
  #  write.csv(presentall,"presentallnoICU.csv")
  xraysortTITCO=xraysort
  fastsortTITCO=fastsort
  opsortTITCO=opsort
  ctsortTITCO=ctsort
  extsortTITCO=extsort
}
}

#ok, nu vill jag skapa en matris som för varje ICD-kod som förekommer på ICU skapar en motsvarande 
n=0

frequencies= data.frame(matrix(, nrow = nrow(xraysortICU), ncol =5 ))
colnames(frequencies)<-data.frame("ICD code","freq ICU","freq no-ICU","freq TITCO","propICUvsnoICU")
for (c in 1:nrow(xraysortICU)) {
       str1 <- xraysortICU[c,1]
       str1 = as.character(str1)
       strfreqICU=xraysortICU[c,2]
       
       #hitta på vilken row denna finns i TITCOsetets ICD rad
      findrow= which(xraysortTITCO$Var1 == str1, arr.ind = TRUE)
      #översätt till ICD-kod
      str2=xraysortTITCO[findrow,1]
      str2 = as.character(str2) #byt till rätt form
      strfreqTITCO=xraysortTITCO[c,2]
      
       
       if (str1 == str2) {
         frequencies[c,1]=str1
         frequencies[c,2]=strfreqICU
         strfreqTITCO=as.numeric(strfreqTITCO)
         frequencies[c,3]=strfreqTITCO-strfreqICU #det funkade dåligt att göra samma sortering på no-ICU (summorna stämemr inte), men de är ju resten så
         strfreqnoICU=strfreqTITCO-strfreqICU
         strfreqICU=as.numeric(strfreqICU)
         strfreqnoICU=as.numeric(strfreqnoICU)
         if (strfreqnoICU>0) {
         prop=strfreqICU/strfreqnoICU
         }
         if (strfreqnoICU==0) {
           prop=0
         }
         frequencies[c,5]=prop
         frequencies[c,4]=strfreqTITCO
       }
      
      #hitta på vilken row denna finns i noICU ICD rad
      # findrow= which(xraysortnoICU$Var1 == str1, arr.ind = TRUE)
      # #översätt till ICD-kod
      # str2=xraysortnoICU[findrow,1]
      # str2 = as.character(str2) #byt till rätt form
      # strfreqnoICU=xraysortnoICU[c,2]
      # 
      # if (length(str2>0)){
      # if (str1 == str2) {
      #   frequencies[c,3]=strfreqnoICU
      #   prop=strfreqICU/strfreqnoICU
      #   frequencies[c,5]=prop
      # }
     #insert some proportions
      xrayed= titcodata %>% filter(grepl('Yes', titcodata$xray)) 
      licupat=nrow(licu)
  
        
        
        # }

}

xrayfreqsort=print(frequencies[order(frequencies$propICUvsnoICU, decreasing = TRUE), ]   )

#nu har jag klöddat så mycket manuellt att copypaste är enda lösningen för att göra om för fast, op ct och ext

#ext
frequencies= data.frame(matrix(, nrow = nrow(extsortICU), ncol =5 ))
colnames(frequencies)<-data.frame("ICD code","freq ICU","freq no-ICU","freq TITCO","propICUvsnoICU")
for (c in 1:nrow(extsortICU)) {
  str1 <- extsortICU[c,1]
  str1 = as.character(str1)
  strfreqICU=extsortICU[c,2]
  
  #hitta på vilken row denna finns i TITCOsetets ICD rad
  findrow= which(extsortTITCO$Var1 == str1, arr.ind = TRUE)
  #översätt till ICD-kod
  str2=extsortTITCO[findrow,1]
  str2 = as.character(str2) #byt till rätt form
  strfreqTITCO=extsortTITCO[c,2]
  
  
  if (str1 == str2) {
    frequencies[c,1]=str1
    frequencies[c,2]=strfreqICU
    strfreqTITCO=as.numeric(strfreqTITCO)
    frequencies[c,3]=strfreqTITCO-strfreqICU #det funkade dåligt att göra samma sortering på no-ICU (summorna stämemr inte), men de är ju resten så
    strfreqnoICU=strfreqTITCO-strfreqICU
    strfreqICU=as.numeric(strfreqICU)
    strfreqnoICU=as.numeric(strfreqnoICU)
    if (strfreqnoICU>0) {
      prop=strfreqICU/strfreqnoICU
    }
    if (strfreqnoICU==0) {
      prop=0
    }
    frequencies[c,5]=prop
    frequencies[c,4]=strfreqTITCO
  }
  
  #hitta på vilken row denna finns i noICU ICD rad
  # findrow= which(extsortnoICU$Var1 == str1, arr.ind = TRUE)
  # #översätt till ICD-kod
  # str2=extsortnoICU[findrow,1]
  # str2 = as.character(str2) #byt till rätt form
  # strfreqnoICU=extsortnoICU[c,2]
  # 
  # if (length(str2>0)){
  #   if (str1 == str2) {
  #     frequencies[c,3]=strfreqnoICU
  #     prop=strfreqICU/strfreqnoICU
  #     frequencies[c,5]=prop
  #   }
    #insert some proportions
    #exted= titcodata %>% filter(grepl('Yes', titcodata$xray)) 
    #licupat=nrow(licu)
    
    
    
  # }
  
}

extfreqsort=print(frequencies[order(frequencies$propICUvsnoICU, decreasing = TRUE), ]   )

#för OP
frequencies= data.frame(matrix(, nrow = nrow(opsortICU), ncol =5 ))
colnames(frequencies)<-data.frame("ICD code","freq ICU","freq no-ICU","freq TITCO","propICUvsnoICU")
for (c in 1:nrow(opsortICU)) {
  str1 <- opsortICU[c,1]
  str1 = as.character(str1)
  strfreqICU=opsortICU[c,2]
  
  #hitta på vilken row denna finns i TITCOsetets ICD rad
  findrow= which(opsortTITCO$Var1 == str1, arr.ind = TRUE)
  #översätt till ICD-kod
  str2=opsortTITCO[findrow,1]
  str2 = as.character(str2) #byt till rätt form
  strfreqTITCO=opsortTITCO[c,2]
  
  
  if (str1 == str2) {
    frequencies[c,1]=str1
    frequencies[c,2]=strfreqICU
    strfreqTITCO=as.numeric(strfreqTITCO)
    frequencies[c,3]=strfreqTITCO-strfreqICU #det funkade dåligt att göra samma sortering på no-ICU (summorna stämemr inte), men de är ju resten så
    strfreqnoICU=strfreqTITCO-strfreqICU
    strfreqICU=as.numeric(strfreqICU)
    strfreqnoICU=as.numeric(strfreqnoICU)
    if (strfreqnoICU>0) {
      prop=strfreqICU/strfreqnoICU
    }
    if (strfreqnoICU==0) {
      prop=0
    }
    frequencies[c,5]=prop
    frequencies[c,4]=strfreqTITCO
  }
  
  #hitta på vilken row denna finns i noICU ICD rad
  # findrow= which(opsortnoICU$Var1 == str1, arr.ind = TRUE)
  # #översätt till ICD-kod
  # str2=opsortnoICU[findrow,1]
  # str2 = as.character(str2) #byt till rätt form
  # strfreqnoICU=opsortnoICU[c,2]
  # 
  # if (length(str2>0)){
  #   if (str1 == str2) {
  #     frequencies[c,3]=strfreqnoICU
  #     prop=strfreqICU/strfreqnoICU
  #     frequencies[c,5]=prop
  #   }
    #insert some proportions
    #oped= titcodata %>% filter(grepl('Yes', titcodata$xray)) 
    #licupat=nrow(licu)
    
    
    
  # }
  
}

opfreqsort=print(frequencies[order(frequencies$propICUvsnoICU, decreasing = TRUE), ]   )

#för ct
frequencies= data.frame(matrix(, nrow = nrow(ctsortICU), ncol =5 ))
colnames(frequencies)<-data.frame("ICD code","freq ICU","freq no-ICU","freq TITCO","propICUvsnoICU")
for (c in 1:nrow(ctsortICU)) {
  str1 <- ctsortICU[c,1]
  str1 = as.character(str1)
  strfreqICU=ctsortICU[c,2]
  
  #hitta på vilken row denna finns i TITCOsetets ICD rad
  findrow= which(ctsortTITCO$Var1 == str1, arr.ind = TRUE)
  #översätt till ICD-kod
  str2=ctsortTITCO[findrow,1]
  str2 = as.character(str2) #byt till rätt form
  strfreqTITCO=ctsortTITCO[c,2]
  
  
  if (str1 == str2) {
    frequencies[c,1]=str1
    frequencies[c,2]=strfreqICU
    strfreqTITCO=as.numeric(strfreqTITCO)
    frequencies[c,3]=strfreqTITCO-strfreqICU #det funkade dåligt att göra samma sortering på no-ICU (summorna stämemr inte), men de är ju resten så
    strfreqnoICU=strfreqTITCO-strfreqICU
    strfreqICU=as.numeric(strfreqICU)
    strfreqnoICU=as.numeric(strfreqnoICU)
    if (strfreqnoICU>0) {
      prop=strfreqICU/strfreqnoICU
    }
    if (strfreqnoICU==0) {
      prop=0
    }
    frequencies[c,5]=prop
    frequencies[c,4]=strfreqTITCO
  }
  
  #hitta på vilken row denna finns i noICU ICD rad
  findrow= which(ctsortnoICU$Var1 == str1, arr.ind = TRUE)
  # #översätt till ICD-kod
  # str2=ctsortnoICU[findrow,1]
  # str2 = as.character(str2) #byt till rätt form
  # strfreqnoICU=ctsortnoICU[c,2]
  
  # if (length(str2>0)){
  #   if (str1 == str2) {
  #     frequencies[c,3]=strfreqnoICU
  #     prop=strfreqICU/strfreqnoICU
  #     frequencies[c,5]=prop
  #   }
    #insert some proportions
    #oped= titcodata %>% filter(grepl('Yes', titcodata$xray)) 
    #licupat=nrow(licu)
    
    
    
  # }
  
}

ctfreqsort=print(frequencies[order(frequencies$propICUvsnoICU, decreasing = TRUE), ]   )

#fast
frequencies= data.frame(matrix(, nrow = nrow(fastsortICU), ncol =5 ))
colnames(frequencies)<-data.frame("ICD code","freq ICU","freq no-ICU","freq TITCO","propICUvsnoICU")
for (c in 1:nrow(fastsortICU)) {
  str1 <- fastsortICU[c,1]
  str1 = as.character(str1)
  strfreqICU=fastsortICU[c,2]
  
  #hitta på vilken row denna finns i TITCOsetets ICD rad
  findrow= which(fastsortTITCO$Var1 == str1, arr.ind = TRUE)
  #översätt till ICD-kod
  str2=fastsortTITCO[findrow,1]
  str2 = as.character(str2) #byt till rätt form
  strfreqTITCO=fastsortTITCO[c,2]
  
  
  if (str1 == str2) {
    frequencies[c,1]=str1
    frequencies[c,2]=strfreqICU
    strfreqTITCO=as.numeric(strfreqTITCO)
    frequencies[c,3]=strfreqTITCO-strfreqICU #det funkade dåligt att göra samma sortering på no-ICU (summorna stämemr inte), men de är ju resten så
    strfreqnoICU=strfreqTITCO-strfreqICU
    strfreqICU=as.numeric(strfreqICU)
    strfreqnoICU=as.numeric(strfreqnoICU)
    if (strfreqnoICU>0) {
      prop=strfreqICU/strfreqnoICU
    }
    if (strfreqnoICU==0) {
      prop=0
    }
    frequencies[c,5]=prop
    frequencies[c,4]=strfreqTITCO
  }
  
  #hitta på vilken row denna finns i noICU ICD rad
  # findrow= which(fastsortnoICU$Var1 == str1, arr.ind = TRUE)
  # #översätt till ICD-kod
  # str2=fastsortnoICU[findrow,1]
  # str2 = as.character(str2) #byt till rätt form
  # strfreqnoICU=fastsortnoICU[c,2]
  
  #det var något fel här, men 
  # if (length(str2>0)){
  #   if (str1 == str2) {
  #     frequencies[c,3]=strfreqnoICU
  #     prop=strfreqICU/strfreqnoICU
  #     frequencies[c,5]=prop
  #   }
  
    #insert some proportions
    fasted= titcodata %>% filter(grepl('Yes', titcodata$fast)) 
    licupat=nrow(licu)
    
    
    
  # }
  
}

fastfreqsort=print(frequencies[order(frequencies$propICUvsnoICU, decreasing = TRUE), ]   )

write.csv(xrayfreqsort,"xrayfreqsort.csv")
write.csv(fastfreqsort,"fastfreqsort.csv")
write.csv(opfreqsort,"opfreqsort.csv")
write.csv(ctfreqsort,"ctfreqsort.csv")
write.csv(extfreqsort,"extfreqsort.csv")
