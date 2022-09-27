setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")


library(tidyverse)
library(dplyr)
library(tableone)

headerproperties <- read.csv("headerpropertiestrans.csv") #från rad 70 har datan fär "type" hamnat i "note"-kolumnen. dessutom är spo2_o2_1 och 2 markerade som kvantitativa men de är Y/n så det har jag löst manuellt
titcodata <- read.csv("titcodatafull.csv") #untampered version

#vi måste hantera att age är numeric och quantiativ men >89 förekommer
#acceptableage<-data.frame(0:89)
titcodata=filter(titcodata, age !=">89")
#resten har lästs som char pga tecknet, måste bytas till num
titcodata[,5] <- as.numeric(titcodata[,5])

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

#välj kvant kolumnerna
quantLicu <- licu[, which((names(licu) %in% quantcolnametrans)==TRUE)]
quantNolicu <- nolicu[, which((names(nolicu) %in% quantcolnametrans)==TRUE)]
quantTitco <- titcodata[, which((names(titcodata) %in% quantcolnametrans)==TRUE)]

#gör stat analysen
quantstatlicu=CreateTableOne(data = quantLicu,includeNA=FALSE)
quantstatnolicu=CreateTableOne(data = quantNolicu,includeNA=FALSE)

#gör t.test för alla kvant kolumner
columncount=nrow(quantcolname) #hur många kolumner ska vi rulla
pvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
tvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
for (a in 2:columncount) { 
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
