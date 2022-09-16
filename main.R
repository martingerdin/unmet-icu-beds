## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
#noacsr::source_all_functions()
library(tidyverse)
library(tableone)

## Import data
headerproperties <- read.csv("headerproperties.csv")
titcodata <- read.csv("titcodata.csv")
#jag har manuellt ändrat och städat lite i dessa så jag sparar dem så jag kan committa dem:
save(headerproperties, file = "headerproperties.RData")
save(titcodata, file = "titcodata.RData")

#FRÅGA: jag kan inte hitta hur man definierar eller filtrerar på row headers, men jag kan göra det här superjobbiga sättet
typefilt=filter(headerrows, name == "type") #gör en dataframe av type-raden
typetrans <- data.table::transpose(typefilt) #transponera den. jo. humor me. jag måste transponera eftersom jag bara vet hur man filtrerar på kolumnnamn
timers=filter(typetrans, V1 == "time" | V1 == "date") #NU kan jag applicera den här arbetshästen
counttimers=length(timers)
#och det kan jag bara göra för att göra counts jag har ju tappat alla associationer inom datasetet

#ladda vektorerna med kvantitativa och kvalitativa datan
load("quantitativecolumnname.rdata")
load("qualitativecolumnname.rdata")
load("textcolumnname.rdata")

#sortera bort DOA
alive=filter(titcodata, incl != 2)
titcodata=alive

## Whatever you do next, maybe clean data?
#definiera dataseten
licu=filter(titcodata, licu >0.5)
nolicu=filter(titcodata, licu <0.5)

#välj kvant kolumnerna
quantitativeLicu <- licu[, which((names(licu) %in% quantitativecolumnname)==TRUE)]
quantitativeNolicu <- nolicu[, which((names(nolicu) %in% quantitativecolumnname)==TRUE)]
quantitativeTitco <- titcodata[, which((names(titcodata) %in% quantitativecolumnname)==TRUE)]

#gör stat analysen
quantitatestatlicu=CreateTableOne(data = quantitativeLicu,includeNA=FALSE)
quantitatestatnolicu=CreateTableOne(data = quantitativeNolicu,includeNA=FALSE)


#gör t.test för alla kvant kolumner
columncount=length(quantitativecolumnname) #hur många kolumner ska vi rulla
pvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
tvalues<- data.frame(matrix(ncol = 1, nrow = columncount))
for (a in 1:columncount) { 
  c=t.test(quantitativeLicu[,a],quantitativeNolicu[,a],na.action=na.exclude)
  pval=c[["p.value"]]
  tval=c[["statistic"]][["t"]]
  pvalues[a,1]=pval
  tvalues[a,1]=tval

  
  
}


df_t <- data.table::transpose(quantitativelong)

#det här är tabellen med P pch T-värden
statresult<-data.frame(df_t,pvalues,tvalues) #jag vet inte hur jag får headers att stå p och t-värde, men det blir en tabell
colnames(statresult)<-data.frame("variable short","p-värde","t-värde")

b=quantitatestatlicu[["ContTable"]][["Overall"]]
myDf <- as.data.frame(b)
ICU_mean=myDf$mean
ICU_sigma=myDf$sd
myDfmeanlicu <- as.data.frame(ICU_mean)
myDfsdlicu <- as.data.frame(ICU_sigma)

b=quantitatestatnolicu[["ContTable"]][["Overall"]]
myDf <- as.data.frame(b)
noICU_mean=myDf$mean
noICU_sigma=myDf$sd
myDfmeannolicu <- as.data.frame(noICU_mean)
myDfsdnolicu <- as.data.frame(noICU_sigma)

#det här är mean och std av ICU vs no-ICU
table2<- data.frame(df_t,myDfmeanlicu,myDfsdlicu,myDfmeannolicu,myDfsdnolicu)