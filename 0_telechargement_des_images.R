#Lib
library(tidyverse)     #pour manipuler le code 
library(dplyr)
library(knitr)

# INFOS =================================================

  #Fichier des donn�es sources de test : df_consoj12.rds
  #les img sont format jpg ou png (pas de gif...?)
  #ID unique dans la base = status_id

# QUESTIONS

  #les vid�os renvoient la premi�re image (on traite)
  #ou rien ? (on ne traite pas)
  #Choix pour l'instant : on ne traite pas (on filtre)

# AMELIORER

  #HIGH
  #G�rer la question des images multiples dans un tweet 
  #G�rer le fait que certaines images, dans la boucle, ne se t�l�chargent pas (ex : )
  #ex : 
  #image = https://pbs.twimg.com/media/EUN2PfyWAAEdWu0.jpg  
  #de ce tweet : 1243967941114302464
  #r�ception unique via le GET pourtant OK... 
  
  #LOW
  #choix du dossier ou enregistrer les images en local

# NEXT STEP

  #stats descriptives : 
  # %photos, video, txt seul
  # evolution 3 cat dans le tps

#GO===========================================================================
# RECUPERER LA BASE ET TELECHARGER LES IMAGES 

#r�cup�ration de la base de donn�es
adressedb <- "C:/R&D2/IA/AnalyseImages/df_consoj12.rds"
db <- readRDS(adressedb)

#filtrage: (garder photos et videos uniquement, enlever retweets)
dbPhotoVideo <- db %>% 
  filter(db$media_type == "photo") %>% #garde photos ET videos
  filter(is_retweet == FALSE) 

  #enlever videos | le mot "video" est pr�sent colonne media_expanded_url
  dbPhoto <- filter(dbPhotoVideo, !grepl('video', media_expanded_url))   
  
  #enlever les GIFS | le mot "video_thumb" est pr�sent dans l'URL dans media_url
  dbPhoto <- filter(dbPhotoVideo, !grepl('video_thumb', media_url))     

  #TEMP: on filtre sur 10 images (pour commencer)
  nbPhotoAretenir <- 20
  dbPhoto <- dbPhoto[1:nbPhotoAretenir,]

#cr�ation d'une colonne "nom de l'image en local" si elle n'existe pas
nomsColonnes <- ""
for (i in 1:length(colnames(dbPhoto))){
  nomsColonnes <- paste(nomsColonnes,colnames(dbPhoto)[i])
}
if (grepl("FichierImg", nomsColonnes) == FALSE){
  dbPhoto <- cbind(dbPhoto, FichierImg="_TWEET NON TRAITE")
  dbPhoto$FichierImg <- as.character(dbPhoto$FichierImg) #force le type de la nouvelle colonne en Character
}

#TEMP: t�l�charger les images 
#recuperation des images
#for (i in 1:nrow(databasePhoto)){
for (i in 1: nbPhotoAretenir){
  
  #identification de l'URL de l'image
  AdresseURLimg <- unlist(dbPhoto$media_url[i])
  
  #pour la creation des fichiers image en local, format # + [ID du tweet].jpg ou .png. Ex : 3_1243971298595831808.jpg
    DossierLocalImg <- "C:\\R&D2\\IA\\AnalyseImages\\ImgTxt_fichiersImg"
    IDduTweet <- unlist(dbPhoto$status_id[i])
    ExtensionImg <- str_sub(AdresseURLimg, start= -3)
    NomFichierImg <- paste(i, "_", IDduTweet, ".", ExtensionImg, sep="")
    AdresseFichierImg <- paste(DossierLocalImg, "\\", NomFichierImg, sep="")
  
  #telecharger les images 
  if (object.size(GET(AdresseURLimg))>50000){     #si la taille de l'objet renvoye est > a 50000 bytes
    GET(AdresseURLimg, write_disk(AdresseFichierImg, overwrite = TRUE))  #telecharger le fichier
    dbPhoto$FichierImg[i] <- NomFichierImg  #mettre le nom du nouveau fichier dans la base, pour info
  } else { 
    dbPhoto$FichierImg[i] <- "_IMAGE_NON_RECUE"
  }
  
  #messages de suivi
  message <- paste("FAIT : ", "Image n�", i, " - ", "ID = ", IDduTweet, sep="")
  print(message)
}

#message de controle
nbFichiersFaits <-length(dir(DossierLocalImg))
Manquants <- nbPhotoAretenir - nbFichiersFaits
pourcentage <- Manquants / nbPhotoAretenir * 100
print(paste("MANQUANT: ", Manquants, " sur ", nbPhotoAretenir, " = ", pourcentage, "%", sep=""))

#enregistrer la base sous un nouveau nom : 
CheminEtFichierNewBase <- "C:/R&D2/IA/AnalyseImages/dbImages.rds"
saveRDS(dbPhoto, CheminEtFichierNewBase)
print(paste("Base d'�tiquettage enregistr�e ici: ", CheminEtFichierNewBase, sep=""))

#fin
