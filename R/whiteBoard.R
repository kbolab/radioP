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

findOutTheFirstPositionInAgenda<-function() {
  
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
=======
#' prova 2
#' @param x parametro
prova<-function(x) {
    return(x)
}



>>>>>>> 7e6a1ff51acc9cc8060a2e63ffbf2e3a39d3b3d2
