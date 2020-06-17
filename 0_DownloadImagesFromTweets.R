

# SCRIPT POUR TELECHARGER LES IMAGES A PARTIR 
# D'UNE LISTE D'URL ACCOMPAGNANT DES TWEETS

# v 1.0
# Auteurs: Mehdi ELMOUKHLISS, Julien CLOAREC


#ATTENTION : LIMITES / A AMELIORER

  #Ne gere PAS la presence d'images multiples pour un tweet 

  #Environ 10% des images ne se telechargent PAS

  #Par securite : Enregistre une NOUVELLE base "100% images" 
  #(plutot que d'etiquetter et ecraser la base d'origine)
  #nom de la nouvelle base : dbImages.rds


#================= CODE A MODIFIER ========================================

#VARIABLES LOCALES (A ADAPTER A VOTRE CONTEXTE LOCAL)

#ou se trouve la base de tweets
CheminLocalBaseTweets <- "C:/R&D2/IA/AnalyseImages/new/df_consoj12.rds"  # <== chemin et nom base source des tweets

#maindir/subdir : le dossier ou seront enregistree les images et la nouvelle base
mainDir <- "C:/R&D2/IA/AnalyseImages/new/"   #ou sera stockee la nouvelle base   <==
 subDir <- "images"                          #ou seront stockees les images      <==

dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

  #le dossier ou sera enregistre les images
  CheminLocalDossierImage <- paste(mainDir, subDir, "/", sep="")
  
  #le dossier et nom de fichier ou sera enregistre la nouvelle base
  CheminEtFichierNewBase <- paste(mainDir, "dbImages.rds", sep="")       # <==


#===================================================================

#INSTALL

if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("httr")) install.packages("httr") 
  
#RUN

##PREPARER LA BASE

#lire base tweets

db <- readRDS(CheminLocalBaseTweets)

#filtrer
dbPhoto <- db %>% 
  filter(db$media_type == "photo") %>% #garde photos ET videos
  filter(is_retweet == FALSE) #enleve retweets

  #enlever videos | le mot "video" est present colonne media_expanded_url:
   dbPhoto <- filter(dbPhoto, !grepl('video', media_expanded_url))   
  #enlever les GIFS | le mot "video_thumb" est present dans l'URL dans media_url:
   dbPhoto <- filter(dbPhoto, !grepl('video_thumb', media_url)) 
  
#creation d'une colonne avec l'adresse en local pour chaque image telechargee
NomsCol <- ""
   for (i in 1:length(colnames(dbPhoto))){
     NomsCol <- paste(nomsCol,colnames(dbPhoto)[i])
   }
   if (grepl("FichierImg", NomsCol) == FALSE){
     dbPhoto <- cbind(dbPhoto, FichierImg="")
     #force le type de la nouvelle colonne en Character
     dbPhoto$FichierImg <- as.character(dbPhoto$FichierImg)
   }

#RECUPERER LES IMAGES



#recuperer TOUTES les images
nbPhotos <- nrow(dbPhoto)

# TEMP utile pour TESTS a petite echelle (les 10 premieres images:
for (i in 1:10){

#for (i in 1:nbPhotos){   #pour chaque image...
  
  #identification de l'URL de l'image
  AdresseURLimg <- unlist(dbPhoto$media_url[i])
  
  #pour la creation des fichiers image en local, format # + [ID du tweet].jpg ou .png. Ex : 3_1243971298595831808.jpg
  IDduTweet <- unlist(dbPhoto$status_id[i])
  ExtensionImg <- str_sub(AdresseURLimg, start= -3)
  NomFichierImg <- paste(i, "_", IDduTweet, ".", ExtensionImg, sep="")
  AdresseFichierImg <- paste(CheminLocalDossierImage, "\\", NomFichierImg, sep="")
  
  #telecharger l'image
  if (object.size(GET(AdresseURLimg))>50000){     #si la taille de l'objet renvoye est > 50000 bytes (=<>fichier dumb)
    GET(AdresseURLimg, write_disk(AdresseFichierImg, overwrite = TRUE))  #telecharger le fichier
    dbPhoto$FichierImg[i] <- NomFichierImg  #mettre le nom du nouveau fichier dans la base, pour info
   } else { 
    dbPhoto$FichierImg[i] <- ""
   }

#LOG de suivi
print(paste("FAIT : ", "IMG = ", i, "_", IDduTweet, " | ", i, " images sur ", nrow(dbPhoto), sep=""))

}

#LOG de controle
nbFichiersFaits <- length(dir(CheminLocalDossierImage))
Manquants <- nbPhotos - nbFichiersFaits
pourcentage <- Manquants / nbPhotos * 100
Message1 <- paste("IMAGES MANQUANTES: ", Manquants, " sur ", i, " = ", pourcentage, "%", sep="")
print(Message1)

#enregistrer la base sous un nouveau nom : 
saveRDS(dbPhoto, CheminEtFichierNewBase)
print(paste(nbFichiersFaits, " Images telechargees, nouvelle base cree. Voir : ", CheminEtFichierNewBase))
#decharger la memoire
nbFichiersFaits <- ""
i <- ""
db <- ""
dbPhoto <- ""


  