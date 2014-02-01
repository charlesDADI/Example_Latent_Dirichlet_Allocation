####function of text mining
install.packages("tm")
install.packages('SnowballC')
library(tm)
library(SnowballC)

#Fonction traitant la ponctuation, double espace, enlève majuscule, enlève stop word 
Pretraitement=function(tweet,langue='english',remove_stop_word=NULL){

##supprime les accents 
iconv(tweet, to="ASCII//TRANSLIT")  

  traitement1=tm_map(tm_map(Corpus(VectorSource(tweet)),stripWhitespace),tolower)
if(length(remove_stop_word)==0){
 #,stemDocument,language=langue
  traitement_final=tm_map(tm_map(tm_map(tm_map(traitement1,removeWords,stopwords(langue))),removePunctuation),stripWhitespace)}
  else{not_remove=rep(0,length(remove_stop_word))
       for(i in 1:length(remove_stop_word)){
			if(length(which(stopwords(langue)==remove_stop_word[i]))==1){
			not_remove[i]=which(stopwords(langue)==remove_stop_word[i])}}
       traitement_final=tm_map(tm_map(tm_map(tm_map(traitement1,removeWords,stopwords(langue)[-not_remove]),stemDocument,language=langue),removePunctuation),stripWhitespace)}
return(as.character(traitement_final))}

#Fonction traitant la négation : ex  remplace didnt tell par NON-tell 
negation_traitement=function(text,negation_word=stopwords('english')[81:98]){
  negation_word=as.character(tm_map(Corpus(VectorSource(negation_word)),removePunctuation)) 
{for(i in 1:length(negation_word)){text=gsub(paste(negation_word[i]," ",sep=""),"NON-",text)}}
  return(text)}

#On enlève les mots qui n'apparaissent qu'une seule fois
enleve_mot_rare=function(text){
Text_corpus=Corpus(VectorSource(text))
p=findFreqTerms(DocumentTermMatrix(Text_corpus),lowfreq=0,highfreq=100)
p=paste(" ",p," ")
l=length(text)
if( l <= 1000)text=gsub(paste(p,collapse="|"),"",text)
if( l > 1000){
			pas=l/100
			ind=seq(0,l,pas)
			for(i in 1:(length(ind)-1)){
					text=gsub(paste(p[ind[i]:ind[i+1]]	,collapse="|")," ",text)
								}
					}
while(length(grep("  ",text))>0){text=gsub("  "," ",text)}
return(text)}

#fonction gardant 1.31,1.30,1.29
garde_valeur=function(msg,valeurIN=c(131,130,129),valeurOUT=c("1.30","1.31","1.29")){
gsub(valeurIN,valeurOUT, msg)
}


##compter le nombre de mot
occurence_keywords_affirmatif=function(msg,keywords) #fonction comptant l'occurence des mots clés uniquement positifs
{ comptage=rep(0,length(keywords))
  names(comptage)=keywords
  words=strsplit(msg," ")
for(i in 1:length(words[[1]]))
{if(length(which(keywords==words[[1]][i]))==1){comptage[which(keywords==words[[1]][i])]=comptage[which(keywords==words[[1]][i])]+1}
  }
return(comptage)}


