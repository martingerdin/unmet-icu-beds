setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")


library(tidyverse)
library(dplyr)
library(tableone)

headerproperties <- read.csv("headerpropertiestrans.csv") #från rad 70 har datan fär "type" hamnat i "note"-kolumnen. dessutom är spo2_o2_1 och 2 markerade som kvantitativa men de är Y/n så det har jag löst manuellt
titcodata <- read.csv("titcodatafull.csv") #untampered version

headerproperties[2,6]="text" #gör mig av med patient ID för den har 16000 varianter 

#ur headerprops, filtrera fram alla rader i type-kolumnen som är är c("qualitative", och skapa en array av motsvarande rader i name-kolumnen
#filtrera fram array kolumnnamnen
qualcolname <- headerproperties %>% filter(type %in% c("qualitative")) %>% select(name)
#begripliga namnen
quallong<- headerproperties %>% filter(type %in% c("qualitative")) %>% select(label)
#och bygg en matrix med de kolumnerna
qualcols =titcodata %>% select(all_of(qualcolname$name))

#data frames med licuows och nonlicurows
licu=filter(titcodata, licu >0.5)
nolicu=filter(titcodata, licu <0.5)

#transponera
qualcolnametrans<-data.table::transpose(qualcolname)
#välj kvant kolumnerna ut licu och nonlicurows och hela titco
qualLicu <- licu[, which((names(licu) %in% qualcolnametrans)==TRUE)]
qualNolicu <- nolicu[, which((names(nolicu) %in% qualcolnametrans)==TRUE)]
qualTitco <- titcodata[, which((names(titcodata) %in% qualcolnametrans)==TRUE)]

#gör stat analysen
qualstatlicu=CreateTableOne(data = qualLicu,includeNA=FALSE)
qualstatnolicu=CreateTableOne(data = qualNolicu,includeNA=FALSE)

#man kan göra mer analys på detta men det kräver att man tolkar list-värden

#unika värden, exempel (datatype character list)

qualcolumncount=nrow(qualcolname) #hur många kolumner ska vi rulla
f=0
#kör först en loop för att räkna varianter och dimensionera resultatmatrisen
for (a in 1:qualcolumncount) { 
  currentqual=qualcolname[a,1] #namn på kolumn
  #currentcolumn=paste("titcodata$",currentqual, sep= "")
  currentcol =titcodata %>% select(all_of(currentqual)) #hela kolumnen
  currentcolchr=titcodata[1:nrow(titcodata),currentqual] #i chr form
  
  variants=qualstatlicu[["CatTable"]][["Overall"]][[currentqual]][["level"]]
  variants=data.frame(variants)
  variantcolumncount=nrow(variants)
  
  #anropa rätt column
  
  for (b in 1:variantcolumncount) { 
    currentvariant=variants[b,1]
    f=f+1
  }
  
}
#nu bygger vi matrisen
qualstatmatrix=data <- data.frame(matrix(NA, nrow = f, ncol = 8))
freqtitco=data <- data.frame(matrix(NA, nrow = f, ncol = 1))


#och gör om
 for (u in 1:3){ #en stor yttermatris för att göra analysen för titco, licu och no-licu

   if (u==1){set2use=qualTitco} #hela setet
   if (u==2){set2use=qualLicu} #licu pat
   if (u==3){set2use=qualNolicu} #nolicu pat

#du måste definiera 5 kolumner som char eftersom de missförståtts som num av createtableone
set2use$hos=as.character(set2use$hos)
set2use$incl=as.character(set2use$incl)
set2use$gcs_m_1=as.character(set2use$gcs_m_1)
set2use$gcs_m_2=as.character(set2use$gcs_m_2)

qualstat=CreateTableOne(data = set2use,includeNA=FALSE)
loopno=0
for (a in 1:qualcolumncount) { 
  currentqual=qualcolname[a,1] #namn på kolumn
  currentlong=quallong[a,1]
  #currentcolumn=paste("titcodata$",currentqual, sep= "")
  currentcol =set2use %>% select(all_of(currentqual)) #hela kolumnen
  currentcolchr=set2use[1:nrow(set2use),currentqual] #i chr form
  
  variants=qualstat[["CatTable"]][["Overall"]][[currentqual]][["level"]]
  variants=data.frame(variants)
  variantcolumncount=nrow(variants)
  
  #anropa rätt column
  
  for (b in 1:variantcolumncount) { 
    loopno=loopno+1
    currentvariant=variants[b,1]
    #vad vill vi samla för info på denna variant då?
    totalpat=4
    
    if (u==1){totalpat=nrow(titcodata)} #total in set
    if (u==2){totalpat=nrow(licu)} #total in set
    if (u==3){totalpat=nrow(nolicu)} #total in set
    NAcurrent=sum(!complete.cases(currentcol)) #anta NA (samma för alla     varianter av samma col behöver inte vara härinne)
    # tempvec <- str_count(titcodata$dama,pattern="Yes")#vi vill veta hur     många av denna variant vi har
    # sumvariant=sum(f,na.rm=TRUE) #och det är här
    #okej bygg resultaten
    qualstatmatrix[loopno,1]=currentlong #variable name
    qualstatmatrix[loopno,2]=currentvariant #variable name
    qualstatmatrix[loopno,3]=totalpat #hur många pat som finns i just detta set
    qualstatmatrix[loopno,4]=NAcurrent #hur många NA vi har
    freq=qualstat[["CatTable"]][["Overall"]][[currentqual]][["freq"]]
    freq=data.frame(freq)
    freqthisvar=freq$Freq[b]
    qualstatmatrix[loopno,5]=freqthisvar #hur många hits vi har
   # qualstatmatrix[b,6]=sumvariant #hur många hits vi har sätt 2 (for comparison) detta funkade ju inte
    perc=qualstat[["CatTable"]][["Overall"]][[currentqual]][["percent"]]
    
    perc=data.frame(perc)
    percthisvar=perc$Freq[b]
    if (u==1){
      #spara 
      freqtitco[loopno,1]=freqthisvar
    } #total in set

    qualstatmatrix[loopno,6]=freqtitco[loopno,1]
    qualstatmatrix[loopno,7]=freqthisvar/(freqtitco[loopno,1]) #återkom
    
    qualstatmatrix[loopno,8]=percthisvar
    
    colnames(qualstatmatrix)=data.frame("variable name","variant name","patients in this subset","NA values","occurence of variant","occurence of variant in TITCO" ,"proportion this subset vs titco","percentage of this variant of variable")
    
    
    #write
    if (u==1){write.csv(qualstatmatrix,"qualstatTITCO.csv")
      #men också spara 
      freqtitco[loopno,1]=freqthisvar
      } #total in set
    if (u==2){write.csv(qualstatmatrix,"qualstatLicu.csv")} #total in set
    if (u==3){write.csv(qualstatmatrix,"qualstatnoLicu.csv")} #total in set
    

      
    }

  }
  
 }






# columncount=nrow(qualcolname) #hur många kolumner ska vi rulla
# 
# for (a in 1:columncount) { 
#   if(is.na(qualLicu[a,1])) {qualLicu[a,1]=FALSE} else {if(qualLicu[a,1]) {qualLicu[a,1]}}  
#   if(is.na(qualNolicu[a,1])) {qualNolicu[a,1]=FALSE} else {if(qualNolicu[a,1]) {qualNolicu[a,1]}}
#   #c=t.test(qualLicu[,a],qualNolicu[,a],na.action=na.exclude)
# #återkom om du pallar, vet ej om detta kommando är tillämpligt på kval parametrar
#   
# }


