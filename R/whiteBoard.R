library(XML)

buildExamProbabilityCache<-function() {
  
  listaNodi<-getNodeSet(jradio.xml,'/root/esame/dati_esame')
  matrice<-c()
  for(i in listaNodi) {
    nomeEsame<-xpathApply(xmlDoc(i),'/dati_esame',xmlAttrs)
    nomeEsame<-nomeEsame[[1]]["nome_esame"]
    listaFamiglieAssociate<-xpathApply(xmlDoc(i),'/dati_esame/associazione_famiglia_esame',xmlAttrs)
    for( ii in seq(1,length(listaFamiglieAssociate))) {
     matrice<-rbind( matrice, c(nomeEsame, listaFamiglieAssociate[[ii]]["nome_famiglia"],listaFamiglieAssociate[[ii]]["richieste"]   )  ) 
    }    
  }
  sommaTotale<-sum(as.numeric(matrice[,3]))
  colonnaCumulativi<-cumsum(as.numeric(matrice[,3]))/sommaTotale
  matrice<-cbind(matrice,colonnaCumulativi)
  examProbabilityCache<<-matrice;  
}

getRandomExam<-function() {
  if( length(examProbabilityCache)==0 ) buildExamProbabilityCache();
  numeroRandom <- runif(1)
  newArr<-as.numeric(examProbabilityCache[,4])-numeroRandom
  newArr[newArr<0]<-1
  posizioneMinimo<-which ( newArr==min(newArr),arr.ind = T  ) 
  return(examProbabilityCache[posizioneMinimo,])  
}
getIdFromFamilyName<-function(familyName) {
  if( length(IdVSFamilyNameCache)==0 ) {
    a<-xpathApply(jradio.xml,'/root/famiglie/tipo_famiglia',xmlAttrs)
    IdVSFamilyNameCache<-c()
    for( i in seq(1,length(a)))    {
      IdVSFamilyNameCache<<-rbind(IdVSFamilyNameCache,c( a[[i]]["id_tipo_famiglia"], a[[i]]["nome_famiglia"]))
    }
  }
  a<-which(IdVSFamilyNameCache[,2]==familyName,arr.ind = T)
  if( length(a) == 0 ) {cat("#fhd8hf8dh8fd"); stop();}
  return(IdVSFamilyNameCache[which(IdVSFamilyNameCache[,2]==familyName,arr.ind = T),1])
  
}

buildCalendarStruct<-function(   ) {
  # per ogni disgnostica
  calendar<<-list()
  listaSale<-getNodeSet(jradio.xml,'/root/diagnostica/dati_diagnostica')
  for( i in listaSale) {
    aTT<-xmlAttrs(i)["cDiagnostica"]
    for(ct in seq(1, (60/5)*24  )) {
      calendar[[ aTT["cDiagnostica"]   ]][[1]][[ ct ]]<-list()
    }        
    for( days in seq(1,365)) {       
      calendar[[ aTT["cDiagnostica"]   ]][[ days ]] <- calendar[[ aTT["cDiagnostica"]   ]][[1]]
    }
  }
  # ora leggi l'XML per popolare il calendario
  lRules<-getNodeSet(jocc.xml,'/root/rules/rule')  
  
  for( i in lRules ) {
    aTT<-xmlAttrs( i )  
    cDiagnostica<-aTT["cDiagnostica"]
    fromSlot<-aTT["fromSlot"]
    toSlot<-aTT["toSlot"]
    dayOfTheWeek<-aTT["dayOfTheWeek"]
    fromDay<-aTT["fromDay"]
    toDay<-aTT["toDay"]
    period<-aTT["period"]
    if ( period == "w" ) {
      
      for( ii in seq(from=fromDay,to=toDay,by=7)) {
        
      }
      
      
    } else {cat ("#gj9fj9gf"); stop();}
    
  }
  
  
}

findOutTheFirstPositionInAgenda<-function(examName,familyName) {
  IdFamiglia<-getIdFromFamilyName(familyName)
  if( length(calendar)==0 ) buildCalendarStruct()
  
}


phoneCall<-function(atTime) {
  examStruct<-getRandomExam()
  findOutTheFirstPositionInAgenda( examName=examStruct["nome_esame"], familyName=examStruct["nome_famiglia"] )
}




numOfReservation<-50
examProbabilityCache<<-list()
jradio.xml = xmlInternalTreeParse("./jradio.xml")
jocc.xml = xmlInternalTreeParse("./jocc.xml")


for(i in seq(1,50)) {
  phoneCall(atTime=i)
}

