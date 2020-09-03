#v2.0 INFO
#Analyse les images et etiquette une base de donnees associee.

#INSTALLATION
if (!require("imagefluency")) install.packages("imagefluency")

#VARIABLES LOCALES 

# ! A ADAPTER
chemin_dbimages <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/dbImages.rds"  #BDD a etiqueter

# ! A ADAPTER
dossier_img <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/images"  #dossier contenant les images

#BOUCLE

dbimages <- readRDS(chemin_dbimages) 

#TEST 
#nb_lignes <- 3  #pour analyser seulement les 3 premieres images (desactive)

#Run
nb_lignes <- nrow(dbimages) #pour analyser toutes les images

#Etiquetage
for (i in 1:nb_lignes){

  image <- dbimages$FichierImg[i]
  chemin_img <- paste(dossier_img,image,sep = "/")
  if (image != "") 
  {
    image_lue <- img_read(chemin_img)
    dbimages$contraste[i] <- imagefluency::img_contrast(image_lue)
    dbimages$complexite[i] <- imagefluency::img_complexity(image_lue)
    dbimages$symmetrie[i] <- imagefluency::img_symmetry(image_lue) #long !
  }
  #pour monitorer l'analyse dans la console
  print(paste("image ", i, " sur ", nb_lignes, sep = ""))
}

saveRDS(dbimages, file=chemin_dbimages)

print("Etiquetage fini : Constraste, Complexite, Symetrie")
