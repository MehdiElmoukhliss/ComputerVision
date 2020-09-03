# TELECHARGER LES IMAGES 

#v 2.1 FONCTIONNEMENT DU SCRIPT
#Cree, a partir d’une base listant des tweets et les images associees, 
#Une nouvelle base (dbImages.rds) avec uniquement les tweets comportant des images. 
#Le code telecharge aussi toutes les images, et les positionne dans un dossier local.
#Il etiquette la nouvelle base avec le nom de fichier de l’image (telechargee) de chaque tweet.
#Cette nouvelle base est destinee a etre etiquettee par les resultats de l’analyse d’image.

#LIMITES / A AMELIORER
#Ne gere pas la presence d'images multiples pour un tweet 
#Environ 20% des images ne se telechargent pas et donc échappent à l’analyse


#VARIABLES LOCALES 

# ! A ADAPTER
#ou se trouve la base Source de tweets
CheminLocalBaseTweets <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/df_consoj12.rds"  

# ! A ADAPTER
#maindir/subdir : les dossiers ou seront enregistrees les images et la nouvelle base
mainDir <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest"   #ou sera stockee la nouvelle base   <==A ADAPTER
subDir <- "images"     #sous-dossier ou seront stockees les images      

dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

#le dossier ou sera enregistre la nouvelle base + nomde fichier de celle-ci
CheminEtFichierNewBase <- paste(mainDir, "dbImages.rds", sep="/")       

#le dossier ou seront enregistrees les images
CheminLocalDossierImage <- paste(mainDir, subDir, sep="/")


#INSTALL
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("httr")) install.packages("httr") 

#RUN

#1/ PREPARER LA BASE

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
  
#2/ RECUPERER LES IMAGES

#TEST (utile pour des tests a petite echelle)
#nbPhotos <- 5   #les 5 premieres images

#Recuperer TOUTES les images 
nbPhotos <- nrow(dbPhoto)

for (i in 1:nbPhotos){   #pour chaque image...
  
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
  print(paste("FAIT : ", "IMG = ", i, "_", IDduTweet, ".", ExtensionImg, " | ", i, " images sur ", nbPhotos, sep=""))
  
}

#LOG de controle
nbFichiersFaits <- length(dir(CheminLocalDossierImage))
Manquants <- nbPhotos - nbFichiersFaits
pourcentage <- Manquants / nbPhotos * 100
Message1 <- paste("IMAGES MANQUANTES: ", Manquants, " sur ", nbPhotos, " = ", pourcentage, "%", sep="")
print(Message1)

#enregistrer la base sous un nouveau nom
saveRDS(dbPhoto, CheminEtFichierNewBase)
print(paste(nbFichiersFaits, " Images telechargees, nouvelle base cree. Voir : ", CheminEtFichierNewBase, sep=""))