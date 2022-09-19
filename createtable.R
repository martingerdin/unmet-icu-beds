

# for (a in 1:7) {
#   
# currentname = headerproperties[a,1]
# 
# currentnamerow=filter(headerrows, name == currentname) #gör en dataframe av type-raden
# currentnamerowtrans <- data.table::transpose(currentnamerow) #transponera den. jo. humor me. jag måste transponera eftersom jag bara vet hur man filtrerar på kolumnnamn
# timers=filter(typetrans, V1 == "time" | V1 == "date") #NU kan jag applicera den här arbetshästen
# counttimers=length(timers)
# #och det kan jag bara göra för att göra counts jag har ju tappat alla associationer inom datasetet
# 
# }

headerrows <- head(headerproperties)
typefilt=filter(headerrows, name == "type") #gör en dataframe av 
typetrans <- data.table::transpose(typefilt) #transponera den. jo.
types=unique(typetrans)

typenames<- data.frame(matrix(ncol = 1, nrow = 6))
typecount<- data.frame(matrix(ncol = 1, nrow = 6))

for (a in 1:6) {

  currenttype=types[a,1]
  
thistype=filter(typetrans, V1 == currenttype) #NU kan jagapplicera den här arbetshästen
thistypetrans <- data.table::transpose(thistype) #transpose

counttimes=length(thistypetrans)

typenames[a,1]=currenttype
typecount[a,1]=counttimes

collect <- data.frame(typenames, typecount)

}


#fler kolumner
#hur ifyllda är kolunerna?
quantitativeTitco <- titcodata[, which((names(titcodata) %in% quantitativecolumnname)==TRUE)]
qualitativeTitco <- titcodata[, which((names(titcodata) %in% qualitativecolumnname)==TRUE)]
textTitco <- titcodata[, which((names(titcodata) %in% textcolumnname)==TRUE)]

completionquant=1-sum(is.na(quantitativeTitco))/(nrow(titcodata)*length(quantitativecolumnname)) #completion rate quant
completionquant=round(completionquant, digits = 2)
completionqual=1-sum(is.na(qualitativeTitco))/(nrow(titcodata)*length(qualitativecolumnname))
completionqual=round(completionqual, digits = 2)

#för text får vi göra lite annorlunda:
# completiontext=1-sum(is.na(textTitco))/(nrow(titcodata)*length(textcolumnname))
# completiontext=round(completiontext, digits = 2)


completion<-data.frame("",completionqual,completionquant,"","","not possible to distinguish between No Findings and Not Documenteed ")
completiontrans<-data.table::transpose(completion)


typeofvariable<-c("","")

comment<-data.frame("", "incl three admin : hospital ID, patient ID and sequence no","","","","")
commenttrans<-data.table::transpose(comment)

examples<-data.frame("", "varibles such as whether or not patient had surgical airway, or received x-ray","varibles such as heart rate","timing of GCS assessment","","variables such as type of surgery, observations during examinations or ICD codes")
exampletrans<-data.table::transpose(examples)

table1<-data.frame(collect,completiontrans,commenttrans)


colnames(table1)<-data.frame("data type","count","completion rate","comment")

table1_1=table1[2:6,1:3]

save(table1_1, file = "table1_1.RData")
