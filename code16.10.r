
library(RPostgres)
library(stringr)
library(dplyr)
library(quanteda)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readr)
library(readtext)
library(SentimentAnalysis)
library(tidyverse)
library(quanteda.dictionaries)
library(corpustools)
library(stringi)
library(RColorBrewer)
library(quanteda.dictionaries)
library(quanteda.sentiment)
library(lubridate)
library(ggplot2)


import_data <- function(){
  # Importiert alle Texte (google und Wiso) aus der
  # PostgreSQL Datenbank und merged die Datensaetze
  library(DBI)
  library(RPostgres)
  
  con <- dbConnect(Postgres(),dbname = 'testpscss', 
                   host = '193.196.54.88',
                   port = 1235,
                   user = 'testpscss',
                   password = "P*ass*css")
  
  dbListTables(con)
  
  gebietseinheit <- dbReadTable(con, "gebietseinheit")
  gebietseinheit <- gebietseinheit[gebietseinheit$land=="08",] #08=BW
  
  corpus <- dbReadTable(con, "google_parsed")
  corpus2 <- dbReadTable(con, "wiso_parsed")
  corpus <- bind_rows(corpus, corpus2)
  df <- corpus %>% mutate(domain = case_when(is.na(domain) ~ wiso_code,
                                             TRUE ~ domain))
  #con <- dbConnect(Postgres(),dbname = 'testpscss', 
  #                host = '193.196.54.88',
  #               port = 1235,
  #              user = 'testpscss',
  #             password = "P*ass*css")
  #dfg = dbReadTable(con, "google_parsed")
  #dfw = dbReadTable(con, "wiso_parsed")
  
  
  
  return(df)
}


datum_auslesen <- function(text){
  # Manche Datumsformate nutzen Punkt, diese muessen 
  # durch Strich erstezen, dann als Datum ausgeben
  # unique(substr(df$temp_date, 4,4))
  temp = substr(text, 1,10)
  #temp = gsub(".", "-", temp, fixed=T)
  
  
  #unique(substr(df$temp_date, 4,4))
  date = as.Date(temp)
  return(date)
}

text_preprocessing <- function(text){
  # Da die Analysebene auf Satzebene liegt, mÃ¼ssen alle Artikel in 
  # Saetze zerlegt werden. Dies geschieht an der Punktierung. 
  # Dabei kann aber ein Punkt z.B. nach 2. die Trennung verwirren.
  # Aus 2. soll also 2te werden:
  temp = gsub("(?<=\\b[0-9]|\\b[0-9]{2})\\.\\s", "te ", text, perl = TRUE)
  
  # Ggf. ergÃ¤nzen, wenn weitere Vorbereitende Schritte notwendig sind
  
  return(temp)
}

parteibezeichnungen_normalisieren <- function(text, dictionnaire){
  # DiktionÃ¤r von Parteien als Liste
  for(i in c(1:length(dictionnaire))){
    text <- gsub(paste(dictionnaire[[i]],
                       collapse = "|"),
                 names(dictionnaire)[i],
                 text,
                 ignore.case = TRUE)
  }
  return(text)
}




parteinennungen_zuspielen <- function(cps, dictionnaire){
  count_mentions <- function(cps, dictionnaire){
    # Sucht nach Parteien in  SÃ¤tzen und zÃ¤hlt wie hÃ¤ufig. 
    # Gibt Counts in Vektor der LÃ¤nge Anzahl pattern zurÃ¼ck 
    occurance <- matrix(NA,
                        nrow = length(unlist(cps)),
                        ncol = length(dictionnaire))
    for(i in c(1:length(dictionnaire))){
      occurance[,i] <- stringr::str_count(cps, names(dictionnaire)[i])
    }
    return(occurance)
  }
  
  parteinennungen <- count_mentions(cps, dictionnaire)
  
  parteinennungen <- as.data.frame(parteinennungen)
  names(parteinennungen) <- names(dictionnaire)
  for(d in names(dictionnaire)){
    docvars(cps, d) <- parteinennungen[,d]
  }
  #docvars(cps) <- parteinennungen
  return(cps)
}

# Mit diesem Befehl werden die Funktionen eingelesen
#source("./funktionen.R")


# Daten einelsen aus Datenbank
df <- import_data()

# Datumsvariable ertstellen (Datum der Publikation)
df$pub_datum <- datum_auslesen(df$parsed_text)


# Kleinere Vorabreiten
# Hinweis: Texte werden nicht in Kleinschreibung umgewandelt, da 
# sonst die Zerlegung in einzelne SÃ¤tze nicht korrekt funktioniert (dabei
# wird auf GroÃŸ/Kleinschreibung nach der Punktion geachtet).
df$preprocessed_text <- text_preprocessing(df$parsed_text)


# Parteinamen mit Diktionaer vereinheitlichen

dparteien <- list(SPD = c("spd", "sozialdemokrat\\w*", "sozen"),
                  CDU = c("cdu\\w*", "christdemokrat\\w*", "union\\w*", "csu\\w*", "christsozial\\w*"),
                  Grüne = c("bündnis 90","bündnis90", "grüne\\w*"),
                  Linke = c("linke\\w*"),
                  AFD = c("afd\\w*", "blaue\\w*"),
                  FDP = c("fdp\\w*", "liberale\\w*"),
                  PIRATEN = c("piraten\\w*", "seeräuber"),
                  NPD = c("npd\\w*"),
                  DiePartei = c("die partei\\w*"),
                  ÖDP = c("Ödp\\w*"))



df$preprocessed_text <- parteibezeichnungen_normalisieren(df$preprocessed_text, dparteien)


# Dann quanteda Corpus erstellen
library(quanteda)
cp = corpus(df$preprocessed_text)
# Die docvars sind ein dataframe, der als Metainformation dem Corpus 
# angehÃ¤ngt wird. zeilenweise die Dokumente, spaltenweise Variablen 
docvars(cp) <- df[,c("ortsname", "RS", "pub_datum")]

# Nun den Corpus auf Satzebene erstellen:
cps <- corpus_reshape(cp, 
                      to = c("sentences"), 
                      use_docvars = TRUE)
# Und fuer jeden Satz als docvar zuspielen, welche 
# Parteien im Satz genannt werden
cps <- parteinennungen_zuspielen(cps, dparteien)
docvars(cps)

# Die Parteien aus den docvars koennen jetzt als unabhaengige Dummy-Variable genutzt werden, 
# die einen Einfluss auf das Sentiment haben.

# Saetze zu Windkraft, in denen CDU azftaucht z.B. negativer 
# Saetze zu Windkraft, in denen GrÃ¼ne auftacuhen z.B: positiver

suchvektor <- c( "Anwaltsplanung", "Arbeitsgruppen", "Arbeitskreis", "Arbeitsgruppe", "Auftakt-Veranstaltung", "Auftaktveranstaltung", "Beteiligung", "Beteiligungsverfahren", "Bürgerbeteiligung", "Beteiligungsangebot", "beteiligungsorientiert", "Beteiligungsbeirat", "Bürgerbeirat", "Bürgerdialog", "Bürger-Dialog", "Bürgercafe", "Bürgercafé", "Bürger-Café", "Bürger-Cafe", "Bürgerbeteiligungsabend","Bürgerforum", "Bürger-Forum", "Bürgergipfel", "Bürgerversammlung", "Bürgerkonferenz", "Bürgerinnen Forum", "Bürgerforen","Bürgergutachten","Bürgerhaushalt", "Kiezfonds", "Schülerhaushalt", "Bürgerbudget","Bürgerinnen Rat", "Bürger Rat", "Bürgerrat", "Bürgerräte" , "Bürgerinnenrat", "Bürgerinnen-Rat", "Bürger-Rat", "minipublic", "Mini-Publics", "Mini-Public", "Minipublics","Bürgertisch", "Bürgerstammtisch", "Bürgergespräch","Deliberation", "deliberativ", "Deliberative Mapping", "Deliberative Polling","Demokratiewerkstatt", "Demokratie Werkstatt","Dialog", "dialogorientiert","Dialogverfahren",  "Dialogangebot","Diskussionsveranstaltung", "Diskussionsrunde", "Diskussionsabend", "Einwohnerantrag", "Bürgerantrag","Einwohnerkonferenz", "Anwohnerkonferenz", "Einwohnerbeteiligung", "Anwohnerbeteiligung", "Einwohnerversammlung", "Anwohnerversammlung", "Nachbarschaftsgespräche","Flüchtlingsdialog", "Coronadialog","Fokusgruppe", "Fishbowl", "Dynamic Facilitation", "Aktivierende Befragung", "frühe Öffentlichkeitsbeteiligung", "Anhörung", "Öffentlichkeitsbeteiligung", "Erörterungstermin", "Informationsveranstaltung", "Bürgerinformation", "Infoabend", "Informationsrunde", "Informationsabend", "Jugendgemeinderat", "Jugendparlament", "Kinderparlament", "Kindergemeinderat", "Jugendbeteiligung", "Jugendforum", "Kinderforum", "Jugendbeirat", "Achterrat", "8er-Rat", "Konsensuskonferenz", "Konsensus-Konferenz", "Konsensorientierte Abstimmung", "Konsensorientierte Abstimmungsverfahren", "Konsultation", "Mitwirkung", "Mitbestimmung", "Koproduktion", "Leitbild","Masterplan", "Leitlinien", "Mediation", "Schlichtung", "Konfliktlösung", "Konfliktlösungskonferenz", "Faktenklärung", "Meinungsumfrage", "Umfrage", "Bürgerumfrage", "Bürgerbefragung", "Bürgerpanel", "Vorschlagswesen", "Online-Beteiligung", "Online-Dialog", "Online-Konsultation", "E-Konsultation", "E-Panel", "Open Space Konferenz", "Open Space", "Open-Space-Konferenz", "Ortsbegehung", "Stadtteilspaziergang", "Stadtteil-Spaziergang", "Ortstermin", "Ortsbegang", "Partizipation", "partizipativ", "Partizipationsangebot", "Planungszelle", "Planungswerkstatt", "Planungsworkshop", "Runder Tisch", "Stadtteilforum", "Stadtteilkonferenz", "Werkstatt", "Workshop", "Perspektivenwerkstatt", "Kompetenzwerkstatt", "Strategiewerkstatt", "Bilanzwerkstatt", "Themenwerkstatt", "Bürgerwerkstatt", "World Cafe", "World-Cafe", "Worldcafe", "Zukunftskonferenz", "Zukunftswerkstatt")
suchvektor <- paste0(suchvektor, collapse = "|")


verfahren <- str_extract_all(df$preprocessed_text, suchvektor)
verfahren <- unlist(lapply(verfahren,paste0, collapse = ", "))
df <- data.frame(df, verfahren)
head(df)


df$anzahl <- str_count(df$verfahren, suchvektor)

df2 <- df %>% 
  group_by(pub_datum) %>%
  summarise(total = sum(anzahl))

best <- df2[which.max(df2$total), ]
best$total 
dummy <- str_detect(df$parsed_text, regex(suchvektor, ignore_case = T))
length(dummy[dummy == TRUE])



#############

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readr)
library(readtext)
library(SentimentAnalysis)
library(tidyverse)
library(quanteda.dictionaries)
library(corpustools)
library(stringi)
library(RColorBrewer)
library(quanteda.dictionaries)
library(quanteda.sentiment)
library(lubridate)
library(ggplot2)


class(cps)
head(cps)


#Create a dtm

dtm = cps %>% tokens %>% dfm()
dtm
class(dtm)

#preprocessing
dtm = dfm(cps, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm


textplot_wordcloud(dtm, max_words = 50)     ## top 50 (most frequent) words
textplot_wordcloud(dtm, max_words = 30, color = c('blue','pink')) ## change colors
textstat_frequency(dtm, n = 10)   


head(dtm)
docvars(dtm)

#sentiment 

#Dictionnary anwenden
##devtools::install_github("kbenoit/quanteda.dictionaries") # install dictionaries if not already done

devtools::install_github("kbenoit/quanteda.dictionaries", force=TRUE)
library(devtools)
library(quanteda)
#deutsches sentiment dict

dic <- data_dictionary_sentiws 
head(dic)

#Corpus erstellen
#mit der Funktion bleiben die frueheren Variablen erhalten was man an der AUgabe "and 7 docvars" erkennen kann
corp = corpus(df, text_field = 'parsed_text')
summary(corp)
class(corp)

#Create a dtm

dtm = corp %>% tokens %>% dfm()
dtm
class(dtm)



#Next step: clean document term matrix

dtm = dfm(corp, stem=T, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm

#General Sentiment on Windkraft in Karsruhe:
colSums(dtm) / sum(ntoken(corp)) # total number of words


#apply dictionarie, dfm_lookup gibt eine neue DTM raus das hier nur noch dictionnary terms ernthÃ¤lt, zu dataframe konvertiert
# und ein tidyverse tibble daraus macht 
result = dtm %>%
  dfm_lookup(data_dictionary_sentiws)%>%
  convert(to ="data.frame")%>%
  as_tibble
result

#Add how many words where in the doc in total
result = result %>% mutate (length = ntoken(dtm))
result

#compute overall sentiment score -> 3 different ways
result = result %>% mutate(sentiment1=(positive - negative) / (positive + negative)) #value between -1 und 1 -> 1 if it only contains positive words and -1 if only negative words, if it balances somwhere in between
result = result %>% mutate(sentiment2=(positive - negative) / length) #score between -1 and 1 but takes lenght into account
result = result %>% mutate(subjectivity=(positive + negative) / length) #here interest in subjectivity, but of course only a indication because concept of subjectivity is difficult to define
result




#Ort datum dyade wann wo welches projekt


## Dyaden 
con <- dbConnect(Postgres(),dbname = 'testpscss', 
                 host = '193.196.54.88',
                 port = 1235,
                 user = 'testpscss',
                 password = "P*ass*css")


dyaden <- dbReadTable(con, "ort_datum_dyaden")
dyaden

# suchen nach Hardheim       082255001032 20090402:20140401 082255001032_1
# um einen subcorpus daraus zu erstellen

docvars(dtm)
dtm$pub_datum
class(dtm$pub_datum)



#datrange in date 


dyaden <- dyaden %>% mutate(anfang = str_extract(daterange, "[:digit:]+"),
                            anfang = as.Date(anfang, format = "%Y%m%d"),
                            ende = str_extract(daterange, "[:digit:]+$"),
                            ende = as.Date(ende, format = "%Y%m%d"), 
                            zeitraum = interval(anfang, ende))

dyaden

# erst Text auswählen also sätze, die in diesem Bereich liegen von Regionalschlüssel und in dem Zeitraum liegen 

#vorkommen zählen und dann das nehmen was am häufigsten vorkommt 
#geht aber nur auf textebene und nicht auf satzebene


#daaat verfahren

#### Datum funktion erstllen 

dat_verfahren <- function(txt) { #txt bedeutet Input ist ein Korpus wie corpus$parsed_text
  
  # Der Text wird in einen Vektor gepackt
  txt = c(txt)
  
  #1. Datum herausziehen und als Datum formatieren
  datum <- unlist(str_extract_all(txt, "\\d{4}\\-\\d{2}\\-\\d{2}"))
  datum <- as.Date(datum, format = "%Y-%m-%d")
  
  # Datum und Text werden in ein Dataframe gepackt  
  df <- data.frame(datum, txt)
  
  
  #2. Verfahren werden aus dem Text herausgezogen
  # Pro Zeitungsartikel können verschiedene Verfahren mehrmals erwähnt werden
  # Wenn man dies nicht will muss man die Funktion unique() verwenden
  verfahren <- str_extract_all(df$txt, suchvektor)
  
  # Die Verfahren befinden sich in einem Vektor und werden hier herauskopiert
  verfahren <- unlist(lapply(verfahren,paste0, collapse = ", "))
  
  df <- data.frame(df, verfahren)
  
  
  #3. Verfahren-Strings werden gezählt
  df$anzahl <- str_count(df$verfahren, suchvektor) 
  
  #df$anzahl <- str_count(df$txt, suchvektor) 
  # ^ Der Zweite Schritt kann wegfallen, wenn man gleich so vorgeht und die Verfahren im Text zählt.
  # Die Übersichtlichkeit kann dadurch jedoch verloren gehen.
  
  #4. Beteiligungsverfahren zusammen zählen pro Datum
  
  df <- df %>% 
    # Daten werden nach Datum gruppiert
    group_by(datum) %>% 
    # Die Anzahl der Verfahren wird nach Datum zusammengezählt
    summarise(total = sum(anzahl))
  #5. Datum mit den meisten Verfahren wird ausgegeben
  return(df[which.max(df$total), ])
  
}



####
# hier wird auf jede einzele reihe von dyaden angewandt .
# 1. hier wird ein rgeionalschlüssel rausgezogen aus dyaden rausgezogen
# zeitraum wird rausgezogen
# sucorpus erstellen, der aus texte im zeitraum ist 
#filter -> rs aus dyade der richtige?
#liegt das datum in dem zeitraum oder nicht ?
library(lubridate)


#datefunktion auf den dataframe anwenden


datum_auslesen(df$parsed_text)
df$pub_datum



verf_dat <-  lapply(1:nrow(dyaden), function(x){
  rs <- dyaden$regionalschluessel[x]
  zeit <- dyaden$zeitraum[x]
  sub_corp <- df %>% filter(RS == rs &  pub_datum %within% zeit)
  if(nrow(sub_corp) == 0) return(NA_Date_) #bei 0 dann NA lol
  verf_dat <- dat_verfahren(sub_corp$parsed_text) # wenn nix dann NA
  if(verf_dat$total == 0) return(NA_Date_) #wenn er was findet dann sage er mir das DATUM !!!
  else return(verf_dat$datum) 
}) %>% do.call("c", .)
dyaden$verf_dat <- verf_dat 
sum(!dyaden$regionalschluessel %in% df$RS)
sum(is.na(dyaden$verf_dat))


str(df)
docvars(cp)




##########################
#Nächster teil

#Dataframe nach Datum Sortieren
dftest <- df[with(df, order(pub_datum)),]

#Sub Dataframe -> vor dem Tag mit den meisten beteiligungsverfahren (165)
df_sub1 <- dftest[1:351, 1:15]

#Sub Dataframe -> nach dem Tag mit den meisten Beteiligungsverfahren (165)
df_sub2 <- dftest[351:2661, 1:15]

class(cps)

dummy2 <- docvars(cps)
sum(dummy2$SPD)
sum(dummy2$CDU)
sum(dummy2$Grüne)
sum(dummy2$Linke)
sum(dummy2$AFD)
sum(dummy2$FDP)
sum(dummy2$PIRATEN)
sum(dummy2$NPD)
sum(dummy2$DiePartei)
sum(dummy2$ÖDP)

class(dummy2)
head(dummy2)
dummy3 <- dummy2[dummy2$SPD >= 1,]
l1 <- length(df_sub1$pub_datum)
spdL <- length(dummy3$pub_datum)
l1
df_sub1[1, 12]

####### Erzeugung eines data frames mit nennungen vor dem Tag mit den meisten beteiligungsverfahren der SPD
library(dplyr)
f = 0
SPD_davor <- data.frame(File=character(),
                        Date=as.Date(character()))
                        
                           
                    
for(i in 1:l1) {
  for(k in 1:spdL){
    if(is.na(df_sub1[i, 12]) || is.na(dummy3[k, 3])){
      
    }
     else if(df_sub1[i, 12] == dummy3[k, 3]){
      SPD_davor[f, 1] <-  df_sub1[i, 13] 
      SPD_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

SPD_davor <- distinct(SPD_davor)




l2 <- length(df_sub2$pub_datum)



f = 0
SPD_danach <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:spdL){
    if(is.na(df_sub2[i, 12]) || is.na(dummy3[k, 3])){
      
    }
    else if(df_sub2[i, 12] == dummy3[k, 3]){
      SPD_danach[f, 1] <-  df_sub2[i, 13] 
      SPD_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

SPD_danach <- distinct(SPD_danach)



CDUdf <- dummy2[dummy2$CDU >= 1,]
CDUL <- length(CDUdf$pub_datum)


f = 0
CDU_davor <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:CDUL){
    if(is.na(df_sub1[i, 12]) || is.na(CDUdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == CDUdf[k, 3]){
      CDU_davor[f, 1] <-  df_sub1[i, 13] 
      CDU_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

CDU_davor <- distinct(CDU_davor)


f = 0
CDU_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:CDUL){
    if(is.na(df_sub2[i, 12]) || is.na(CDUdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == CDUdf[k, 3]){
      CDU_danach[f, 1] <-  df_sub2[i, 13] 
      CDU_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

CDU_danach <- distinct(CDU_danach)





FDPdf <- dummy2[dummy2$FDP >= 1,]
FDPL <- length(FDPdf$pub_datum)


f = 0
FDP_davor <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:FDPL){
    if(is.na(df_sub1[i, 12]) || is.na(FDPdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == FDPdf[k, 3]){
      FDP_davor[f, 1] <-  df_sub1[i, 13] 
      FDP_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

FDP_davor <- distinct(FDP_davor)


f = 0
FDP_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:FDPL){
    if(is.na(df_sub2[i, 12]) || is.na(FDPdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == FDPdf[k, 3]){
      FDP_danach[f, 1] <-  df_sub2[i, 13] 
      FDP_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

FDP_danach <- distinct(FDP_danach)






ÖDPdf <- dummy2[dummy2$ÖDP >= 1,]
ÖDPL <- length(ÖDPdf$pub_datum)


f = 0
ÖDP_davor <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:ÖDPL){
    if(is.na(df_sub1[i, 12]) || is.na(ÖDPdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == ÖDPdf[k, 3]){
      ÖDP_davor[f, 1] <-  df_sub1[i, 13] 
      ÖDP_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

ÖDP_davor <- distinct(ÖDP_davor)


f = 0
ÖDP_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:ÖDPL){
    if(is.na(df_sub2[i, 12]) || is.na(ÖDPdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == ÖDPdf[k, 3]){
      ÖDP_danach[f, 1] <-  df_sub2[i, 13] 
      ÖDP_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

ÖDP_danach <- distinct(ÖDP_danach)





Grünedf <- dummy2[dummy2$Grüne >= 1,]
GrüneL <- length(Grünedf$pub_datum)


f = 0
Grüne_davor <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:GrüneL){
    if(is.na(df_sub1[i, 12]) || is.na(Grünedf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == Grünedf[k, 3]){
      Grüne_davor[f, 1] <-  df_sub1[i, 13] 
      Grüne_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

Grüne_davor <- distinct(Grüne_davor)


f = 0
Grüne_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:GrüneL){
    if(is.na(df_sub2[i, 12]) || is.na(Grünedf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == Grünedf[k, 3]){
      Grüne_danach[f, 1] <-  df_sub2[i, 13] 
      Grüne_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

Grüne_danach <- distinct(Grüne_danach)







AFDdf <- dummy2[dummy2$AFD >= 1,]
AFDL <- length(AFDdf$pub_datum)


f = 0
AFD_davor <- data.frame(File=character(),
                          Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:AFDL){
    if(is.na(df_sub1[i, 12]) || is.na(AFDdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == AFDdf[k, 3]){
      AFD_davor[f, 1] <-  df_sub1[i, 13] 
      AFD_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

AFD_davor <- distinct(AFD_davor)


f = 0
AFD_danach <- data.frame(File=character(),
                           Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:AFDL){
    if(is.na(df_sub2[i, 12]) || is.na(AFDdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == AFDdf[k, 3]){
      AFD_danach[f, 1] <-  df_sub2[i, 13] 
      AFD_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

AFD_danach <- distinct(AFD_danach)





Linkedf <- dummy2[dummy2$Linke >= 1,]
LinkeL <- length(Linkedf$pub_datum)


f = 0
Linke_davor <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:LinkeL){
    if(is.na(df_sub1[i, 12]) || is.na(Linkedf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == Linkedf[k, 3]){
      Linke_davor[f, 1] <-  df_sub1[i, 13] 
      Linke_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

Linke_davor <- distinct(Linke_davor)


f = 0
Linke_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:LinkeL){
    if(is.na(df_sub2[i, 12]) || is.na(Linkedf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == Linkedf[k, 3]){
      Linke_danach[f, 1] <-  df_sub2[i, 13] 
      Linke_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

Linke_danach <- distinct(Linke_danach)






PIRATENdf <- dummy2[dummy2$PIRATEN >= 1,]
PIRATENL <- length(PIRATENdf$pub_datum)


f = 0
PIRATEN_davor <- data.frame(File=character(),
                          Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:PIRATENL){
    if(is.na(df_sub1[i, 12]) || is.na(PIRATENdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == PIRATENdf[k, 3]){
      PIRATEN_davor[f, 1] <-  df_sub1[i, 13] 
      PIRATEN_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

PIRATEN_davor <- distinct(PIRATEN_davor)


f = 0
PIRATEN_danach <- data.frame(File=character(),
                           Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:PIRATENL){
    if(is.na(df_sub2[i, 12]) || is.na(PIRATENdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == PIRATENdf[k, 3]){
      PIRATEN_danach[f, 1] <-  df_sub2[i, 13] 
      PIRATEN_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

PIRATEN_danach <- distinct(PIRATEN_danach)





NPDdf <- dummy2[dummy2$NPD >= 1,]
NPDL <- length(NPDdf$pub_datum)


f = 0
NPD_davor <- data.frame(File=character(),
                            Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:NPDL){
    if(is.na(df_sub1[i, 12]) || is.na(NPDdf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == NPDdf[k, 3]){
      NPD_davor[f, 1] <-  df_sub1[i, 13] 
      NPD_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

NPD_davor <- distinct(NPD_davor)


f = 0
NPD_danach <- data.frame(File=character(),
                             Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:NPDL){
    if(is.na(df_sub2[i, 12]) || is.na(NPDdf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == NPDdf[k, 3]){
      NPD_danach[f, 1] <-  df_sub2[i, 13] 
      NPD_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

NPD_danach <- distinct(NPD_danach)






DieParteidf <- dummy2[dummy2$DiePartei >= 1,]
DieParteiL <- length(DieParteidf$pub_datum)


f = 0
DiePartei_davor <- data.frame(File=character(),
                        Date=as.Date(character()))



for(i in 1:l1) {
  for(k in 1:DieParteiL){
    if(is.na(df_sub1[i, 12]) || is.na(DieParteidf[k, 3])){
      
    }
    else if(df_sub1[i, 12] == DieParteidf[k, 3]){
      DiePartei_davor[f, 1] <-  df_sub1[i, 13] 
      DiePartei_davor[f, 2] <-  df_sub1[i, 12]
      f <- f + 1
    }
    
  }
}

DiePartei_davor <- distinct(DiePartei_davor)


f = 0
DiePartei_danach <- data.frame(File=character(),
                         Date=as.Date(character()))



for(i in 1:l2) {
  for(k in 1:DieParteiL){
    if(is.na(df_sub2[i, 12]) || is.na(DieParteidf[k, 3])){
      
    }
    else if(df_sub2[i, 12] == DieParteidf[k, 3]){
      DiePartei_danach[f, 1] <-  df_sub2[i, 13] 
      DiePartei_danach[f, 2] <-  df_sub2[i, 12]
      f <- f + 1
    }
    
  }
}

DiePartei_danach <- distinct(DiePartei_danach)

df_sub1d <- distinct(df_sub1)
df_sub2d <- distinct(df_sub2)


#jetzt haben wir subcorpus mit den Zeitungsartikeln jeweils vor und nach dem Tag mit den meisten Beteiligunsverfahren 

#Beginn der Auswertung 

#der ganze corpus davor

class(df_sub1)

class(SPD_danach)

#Corpus erstellen
#mit der Funktion bleiben die frueheren Variablen erhalten
Corp_danach = corpus(df_sub2, text_field = 'parsed_text')
summary(Corp_danach)
class(Corp_danach)

#Create a dtm

dtm_Corp_danach = (Corp_danach) %>% tokens %>% dfm()
dtm_Corp_danach
class(dtm_Corp_danach)

#corp davor
Corp_davor = corpus(df_sub1, text_field = 'parsed_text')
summary(Corp_davor)
class(Corp_davor)

#Create a dtm

dtm_Corp_davor = (Corp_davor) %>% tokens %>% dfm()
dtm_Corp_davor
class(dtm_Corp_davor)




#General Sentiment on Windkraft in Karsruhe: davor
colSums(dtm_Corp_davor) / sum(ntoken(Corp_davor)) # total number of words
#danch sentiment
colSums(dtm_Corp_danach) / sum(ntoken(dtm_Corp_danach))

#Corpus erstellen
#mit der Funktion bleiben die frueheren Variablen erhalten 
SPD_corp_danach = corpus(SPD_danach, text_field = 'File')
summary(SPD_corp_danach)
class(SPD_corp_danach)

#Create a dtm

dtm_SPD_danach = (SPD_corp_danach) %>% tokens %>% dfm()
dtm_SPD_danach
class(dtm_SPD_danach)

#davor

SPD_corp_davor = corpus(SPD_davor, text_field = 'File')
summary(SPD_corp_davor)
class(SPD_corp_davor)

#Create a dtm

dtm_SPD_davor = (SPD_corp_davor) %>% tokens %>% dfm()
dtm_SPD_davor
class(dtm_SPD_davor)

#Jetzt alle Parteien hintereinander einzeln 
#danach

AFD_corp_danach = corpus(AFD_danach, text_field = 'File')
summary(AFD_corp_danach)
class(AFD_corp_danach)

#Create a dtm

dtm_AFD_danach = (AFD_corp_danach) %>% tokens %>% dfm()
dtm_AFD_danach
class(dtm_AFD_danach)

#davor

AFD_corp_davor = corpus(AFD_davor, text_field = 'File')
summary(AFD_corp_davor)
class(AFD_corp_davor)

#Create a dtm

dtm_AFD_davor = (AFD_corp_davor) %>% tokens %>% dfm()
dtm_AFD_davor
class(dtm_AFD_davor)

####CDUCSU

CDU_corp_danach = corpus(CDU_danach, text_field = 'File')
summary(CDU_corp_danach)
class(CDU_corp_danach)

#Create a dtm

dtm_CDU_danach = (CDU_corp_danach) %>% tokens %>% dfm()
dtm_CDU_danach
class(dtm_CDU_danach)

#davor

CDU_corp_davor = corpus(CDU_davor, text_field = 'File')
summary(CDU_corp_davor)
class(CDU_corp_davor)

#Create a dtm

dtm_CDU_davor = (CDU_corp_davor) %>% tokens %>% dfm()
dtm_CDU_davor
class(dtm_CDU_davor)

#fdp

FDP_corp_danach = corpus(FDP_danach, text_field = 'File')
summary(FDP_corp_danach)
class(FDP_corp_danach)

#Create a dtm

dtm_FDP_danach = (FDP_corp_danach) %>% tokens %>% dfm()
dtm_FDP_danach
class(dtm_FDP_danach)

#davor

FDP_corp_davor = corpus(FDP_davor, text_field = 'File')
summary(FDP_corp_davor)
class(FDP_corp_davor)

#Create a dtm

dtm_FDP_davor = (FDP_corp_davor) %>% tokens %>% dfm()
dtm_FDP_davor
class(dtm_FDP_davor)

#Gruene

Grüne_corp_danach = corpus(Grüne_danach, text_field = 'File')
summary(Grüne_corp_danach)
class(Grüne_corp_danach)

#Create a dtm

dtm_Grüne_danach = (Grüne_corp_danach) %>% tokens %>% dfm()
dtm_Grüne_danach
class(dtm_Grüne_danach)

#davor

Grüne_corp_davor = corpus(Grüne_davor, text_field = 'File')
summary(Grüne_corp_davor)
class(Grüne_corp_davor)

#Create a dtm

dtm_Grüne_davor = (Grüne_corp_davor) %>% tokens %>% dfm()
dtm_Grüne_davor
class(dtm_Grüne_davor)

#Linke

Linke_corp_danach = corpus(Linke_danach, text_field = 'File')
summary(Linke_corp_danach)
class(Linke_corp_danach)

#Create a dtm

dtm_Linke_danach = (Linke_corp_danach) %>% tokens %>% dfm()
dtm_Linke_danach
class(dtm_Linke_danach)

#davor

Linke_corp_davor = corpus(Linke_davor, text_field = 'File')
summary(Linke_corp_davor)
class(Linke_corp_davor)

#Create a dtm

dtm_Linke_davor = (Linke_corp_davor) %>% tokens %>% dfm()
dtm_Linke_davor
class(dtm_Linke_davor)

###alle
dtm_Corp_davor
dtm_Corp_danach
dtm_AFD_davor
dtm_AFD_davor
dtm_SPD_danach
dtm_SPD_davor
dtm_CDU_danach
dtm_CDU_davor
dtm_FDP_danach
dtm_FDP_davor
dtm_Grüne_danach
dtm_Grüne_davor
dtm_Linke_danach
dtm_Linke_davor


dtm_Corp_davor = dfm(dtm_Corp_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Corp_davor

dtm_Corp_danach = dfm(dtm_Corp_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Corp_danach
#
#Overview von den dtms schaffen

#creating a simple wordcloud

textplot_wordcloud(dtm_Corp_davor, max_words = 50)     ## top 50 (most frequent) words
textplot_wordcloud(dtm_Corp_danach, max_words = 50)
textplot_wordcloud(dtm_Corp_davor, max_words = 30, color = c('blue','pink')) ## change colors
textstat_frequency(dtm_Corp_davor, n = 10)    


### AFD

dtm_AFD_danach = dfm(dtm_AFD_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_AFD_danach

dtm_AFD_davor = dfm(dtm_AFD_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_AFD_davor

textplot_wordcloud(dtm_AFD_danach, max_words = 50) 
textplot_wordcloud(dtm_AFD_davor, max_words = 50) 


#compare corpora

class(dtm_AFD_davor)
head(dtm_AFD_davor)
class(dtm_Corp_davor)
head(dtm_Corp_davor)

#Sentiment analysis

#deutsches sentiment dict
library(devtools)
devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)
dic <- data_dictionary_sentiws 
head(dic)



#AFD sentiment
dtm_AFD_davor <- dfm(dtm_AFD_davor, dictionary = dic)
dtm_AFD_davor
colSums(dtm_AFD_davor) / sum(ntoken(dtm_AFD_davor)) # total number of words

dtm_AFD_danach <- dfm(dtm_AFD_danach, dictionary = dic)
dtm_AFD_danach
colSums(dtm_AFD_danach) / sum(ntoken(dtm_AFD_danach)) # total number of words

#hier doce für sentimentsanalyse für alle Parteien einfügen 

df_sub2_corp = corpus(df_sub2, text_field = 'preprocessed_text')


dtm_sub2 = df_sub2_corp %>% tokens %>% dfm()
dtm_sub2
class(dtm_sub2)


#Next step: clean document term matrix

dtm_danach = dfm(df_sub2_corp, stem=F, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_danach

#General Sentiment on Windkraft in Karsruhe:
sentimentscore_danach <- colSums(dtm_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach)
class(sentimentscore_danach)


#apply dictionarie, dfm_lookup gibt eine neue DTM raus das hier nur noch dictionnary terms ernthÃ¤lt, zu dataframe konvertiert
# und ein tidyverse tibble daraus macht 
class(df_sub2)

result1danach = dtm_danach %>%
  dfm_lookup(data_dictionary_sentiws)%>%
  convert(to ="data.frame")%>%
  as_tibble
result1danach

#Add how many words where in the doc in total
result2danach = result1danach %>% mutate (length = ntoken(dtm_danach))
result2danach

#compute overall sentiment score -> 3 different ways
result3danach = result2danach %>% mutate(sentiment1=(positive - negative) / (positive + negative)) #value between -1 und 1 -> 1 if it only contains positive words and -1 if only negative words, if it balances somwhere in between
result4danach = result3danach %>% mutate(sentiment2=(positive - negative) / length) #score between -1 and 1 but takes lenght into account
result5danach = result4danach %>% mutate(subjectivity=(positive + negative) / length) #here interest in subjectivity, but of course only a indication because concept of subjectivity is difficult to define
result5danach
result4danach
class(result1danach)
result6danach <- colMeans(result4danach[ , 2:6])
result6danach
result6danach <- colMeans(result4danach[ , 2:6])
result6danach


#davor 

#CDU/CSU
#
dtm_CDU_danach
dtm_CDU_davor

dtm_CDU_danach = dfm(dtm_CDU_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_CDU_danach

dtm_CDU_davor = dfm(dtm_CDU_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_CDU_davor

#wordcloud
textplot_wordcloud(dtm_CDU_danach, max_words = 50) 
textplot_wordcloud(dtm_CDU_davor, max_words = 50) 
#sentiment
sentimentscore_danach_CDU <- colSums(dtm_CDU_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_CDU)
class(sentimentscore_danach_CDU)

sentimentscore_davor_CDU <- colSums(dtm_CDU_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_CDU)
class(sentimentscore_davor_CDU)

#FDP
dtm_FDP_danach
dtm_FDP_davor

dtm_FDP_danach = dfm(dtm_FDP_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_FDP_danach

dtm_FDP_davor = dfm(dtm_FDP_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_FDP_davor

#wordcloud
textplot_wordcloud(dtm_FDP_danach, max_words = 50) 
textplot_wordcloud(dtm_FDP_davor, max_words = 50) 
#sentiment
sentimentscore_danach_FDP <- colSums(dtm_FDP_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_FDP)
class(sentimentscore_danach_FDP)

sentimentscore_davor_FDP <- colSums(dtm_FDP_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_FDP)
class(sentimentscore_davor_FDP)

#Gruene

dtm_Grüne_danach
dtm_Grüne_davor

dtm_Grüne_danach = dfm(dtm_Grüne_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Grüne_danach

dtm_Grüne_davor = dfm(dtm_Grüne_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Grüne_davor

#wordcloud
textplot_wordcloud(dtm_Grüne_danach, max_words = 50) 
textplot_wordcloud(dtm_Grüne_davor, max_words = 50) 
#sentiment
sentimentscore_danach_Grüne <- colSums(dtm_Grüne_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_Grüne)
class(sentimentscore_danach_Grüne)

sentimentscore_davor_Grüne <- colSums(dtm_Grüne_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_Grüne)
class(sentimentscore_davor_Grüne)


#SPD

dtm_SPD_danach
dtm_SPD_davor

dtm_SPD_danach = dfm(dtm_SPD_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_SPD_danach

dtm_SPD_davor = dfm(dtm_SPD_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_SPD_davor

#wordcloud
textplot_wordcloud(dtm_SPD_danach, max_words = 50) 
textplot_wordcloud(dtm_SPD_davor, max_words = 50) 
#sentiment
sentimentscore_danach_SPD <- colSums(dtm_SPD_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_SPD)
class(sentimentscore_danach_SPD)

sentimentscore_davor_SPD <- colSums(dtm_SPD_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_SPD)
class(sentimentscore_davor_SPD)

#die Linke
dtm_Linke_danach
dtm_Linke_davor

dtm_Linke_danach = dfm(dtm_Linke_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Linke_danach

dtm_Linke_davor = dfm(dtm_Linke_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_Linke_davor

#wordcloud
textplot_wordcloud(dtm_Linke_danach, max_words = 50) 
textplot_wordcloud(dtm_Linke_davor, max_words = 50) 
#sentiment
sentimentscore_danach_Linke <- colSums(dtm_Linke_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_Linke)
class(sentimentscore_danach_Linke)

sentimentscore_davor_Linke <- colSums(dtm_Linke_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_Linke)
class(sentimentscore_davor_Linke)

#Afd
dtm_AFD_danach
dtm_AFD_davor

dtm_AFD_danach = dfm(dtm_AFD_danach, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_AFD_danach

dtm_AFD_davor = dfm(dtm_AFD_davor, remove=stopwords("de"), remove_punct=T, remove_symbols=T, remove_url=T)
dtm_AFD_davor

#wordcloud
textplot_wordcloud(dtm_AFD_danach, max_words = 50) 
textplot_wordcloud(dtm_AFD_davor, max_words = 50) 
#sentiment
sentimentscore_danach_AFD <- colSums(dtm_AFD_danach) / sum(ntoken(df_sub2_corp)) # total number of words

head(sentimentscore_danach_AFD)
class(sentimentscore_danach_AFD)

sentimentscore_davor_AFD <- colSums(dtm_AFD_davor) / sum(ntoken(df_sub1_corp)) # total number of words

head(sentimentscore_davor_AFD)
class(sentimentscore_davor_AFD)












#apply dictionarie, dfm_lookup gibt eine neue DTM raus das hier nur noch dictionnary terms ernthÃ¤lt, zu dataframe konvertiert
# und ein tidyverse tibble daraus macht 
class(df_sub2)

result1danach = dtm_danach %>%
  dfm_lookup(data_dictionary_sentiws)%>%
  convert(to ="data.frame")%>%
  as_tibble
result1danach

#Add how many words where in the doc in total
result2danach = result1danach %>% mutate (length = ntoken(dtm_danach))
result2danach

#compute overall sentiment score -> 3 different ways
result3danach = result2danach %>% mutate(sentiment1=(positive - negative) / (positive + negative)) #value between -1 und 1 -> 1 if it only contains positive words and -1 if only negative words, if it balances somwhere in between
result4danach = result3danach %>% mutate(sentiment2=(positive - negative) / length) #score between -1 and 1 but takes lenght into account
result5danach = result4danach %>% mutate(subjectivity=(positive + negative) / length) #here interest in subjectivity, but of course only a indication because concept of subjectivity is difficult to define
result5danach
result4danach
class(result1danach)
result6danach <- colMeans(result4danach[ , 2:6])
result6danach
result6danach <- colMeans(result4danach[ , 2:6])
result6danach




#Topic modelling
library(topicmodels)
dtm_Corp_davor = convert(dtm, to = "topicmodels") 
set.seed(1)
Topics_davor = LDA(dtm_Corp_davor, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
Topics_davor

#terms to look at the top terms per topic:
terms(Topics_davor, 5)
terms(Topics_davor, 12)

#posterior function gives the posterior distribution of words and documents to topics, which can be used to plot a word cloud of terms proportional to their occurrence
topic = 6
words = posterior(Topics_davor)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)


#wordcloud erstellen
library(wordcloud)
wordcloud(names(topwords), topwords)



# Visualisierung der topics 
library(LDAvis)
library(servr)

dtm_Corp_davor = dtm[slam::row_sums(dtm_Corp_davor) > 0, ]
phi = as.matrix(posterior(Topics_davor)$terms)
theta <- as.matrix(posterior(Topics_davor)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm_Corp_davor)
term.freq = slam::col_sums(dtm_Corp_davor)[match(vocab, colnames(dtm_Corp_davor))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


#Topics danach
dtm_Corp_danach = convert(dtm, to = "topicmodels") 
set.seed(2)
Topics_danach = LDA(dtm_Corp_danach, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
Topics_danach


#terms to look at the top terms per topic:
terms(Topics_danach, 5)
terms(Topics_danach, 12)

#posterior function gives the posterior distribution of words and documents to topics, which can be used to plot a word cloud of terms proportional to their occurrence
topic = 6 #hier beliebiges topic einfügen
words = posterior(Topics_danach)$terms[topic, ]
topwords_danch = head(sort(words, decreasing = T), n=50)
head(topwords_danch)


#wordcloud erstellen
library(wordcloud)
wordcloud(names(topwords_danch), topwords_danch)


# Visualisierung der topics danach
library(LDAvis)
library(servr)

dtm_Corp_danach = dtm[slam::row_sums(dtm_Corp_danach) > 0, ]
phi = as.matrix(posterior(Topics_danach)$terms)
theta <- as.matrix(posterior(Topics_danach)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm_Corp_danach)
term.freq = slam::col_sums(dtm_Corp_danach)[match(vocab, colnames(dtm_Corp_danach))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


#Hier topic modedeling für alle Parteicorpora davor und danach 

dtm_AFD_davor
dtm_AFD_davor
dtm_SPD_danach
dtm_SPD_davor
dtm_CDU_danach
dtm_CDU_davor
dtm_FDP_danach
dtm_FDP_davor
dtm_Grüne_danach
dtm_Grüne_davor
dtm_Linke_danach
dtm_Linke_davor

library(topicmodels)
dtm_AFD_davor = convert(dtm, to = "topicmodels") 
set.seed(1)
Topics_AFD_davor = LDA(dtm_AFD_davor, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
Topics_AFD_davor

#terms to look at the top terms per topic:
terms(Topics_AFD_davor, 5)
terms(Topics_AFD_davor, 12)
#wordclouds
topic = 1 #hier beliebiges topic einfügen
words = posterior(Topics_AFD_davor)$terms[topic, ]
topwords_AFD_davor = head(sort(words, decreasing = T), n=50)
head(topwords_AFD_davor)


#wordcloud erstellen
library(wordcloud)
wordcloud(names(topwords_AFD_davor), topwords_AFD_davor)

#Visualisierung

dtm_AFD_davor = dtm[slam::row_sums(dtm_AFD_davor) > 0, ]
phi = as.matrix(posterior(Topics_AFD_davor)$terms)
theta <- as.matrix(posterior(Topics_AFD_davor)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm_AFD_davor)
term.freq = slam::col_sums(dtm_AFD_davor)[match(vocab, colnames(dtm_Corp_danach))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)