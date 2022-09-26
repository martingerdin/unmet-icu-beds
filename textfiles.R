setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")

#setwd("C:/Users/maria/Downloads/unmet-ICU-beds")

library(tidyverse)
library(dplyr)
library(tableone)

#build strings for the ICU codes

resultmatrix = data.frame(matrix(, nrow = 3, ncol = 11))
resultmatrixe = data.frame(matrix(, nrow = 1, ncol = 12))
resultmatrixct = data.frame(matrix(, nrow = 1, ncol = 13))
resultarray = data.frame(matrix(, nrow = 1, ncol = 11))

#bygg först en liten dataframe med alla ICD koder
#nej vi måste ha tre pga om jag vill använda dem sen för att filtrera ut kolumner accepterar inte r missing values och de är tre olika längder
#jag behöver bygga en array inte en matris av resultaten här annars vill det inte förstå, så
f=1 #counter
uniquexraytot= data.frame(matrix(, nrow = 11*200, ncol =1 )) #resultatmatris, extra stor
for (a in 1:11) {
xray=paste("xray_",a,"_icd",sep="")
fast=paste("fast_",a,"_icd",sep="")
op=paste("op_",a,"_icd",sep="")

resultmatrix[1,a]=xray
resultmatrix[2,a]=fast
resultmatrix[3,a]=op

#vilka unika har vi?
uniquexray=unique(titcodata[xray])
uniquexraytot[f:nrow(uniquexray),1]<- data.frame(uniquexray) #data.frame(uniquexray[1:nrow(uniquexray),1])
f=f+nrow(uniquexray)
#DU VAR HÄR

}

for (a in 1:12) {
  e=paste("e_",a,"_icd",sep="")
  resultmatrixe[1,a]=e
}

for (a in 1:12) {
  ct=paste("ct_",a,"_icd",sep="")
  resultmatrixct[1,a]=ct
}
# 
# resultmatrix[1,12]="e_12_icd"
# resultmatrix[4,12]="ct_12_icd"
# resultmatrix[4,13]="ct_12_icd"

resultarray=resultmatrix[1,1:11]

resultarraytrans<-data.table::transpose(resultarray) #transponera
colnames(resultarraytrans)<-data.frame("name") #sätt dit headern manuellt så den

resultmat=titcodata %>% select(all_of(resultarraytrans$name))

#okej nu har vi en matris med alla x-ray-kolumner, unique ger orimligt många, så jag vill lägga alla dessa kolumner på rad så det inte spelar någon roll i vilken av dessa 11 kolumner den finns
#det måste finnas smidigare sätt, men...
# resultmattrans<-data.table::transpose(resultmat)
# g=data.frame(resultmattrans[1,1:12465],resultmattrans[2,1:12465],resultmattrans[3,1:12465],resultmattrans[4,1:12465],resultmattrans[5,1:12465],resultmattrans[6,1:12465],resultmattrans[7,1:12465],resultmattrans[8,1:12465] ,resultmattrans[9,1:12465],resultmattrans[10,1:12465],resultmattrans[11,1:12465])
# u=unique(g)
#äh det funkade ändå inte
