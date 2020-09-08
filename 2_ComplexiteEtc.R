#v2.0 Analyse les images et etiquette une base de donnees associee.

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

#Creer les colonnes
nomsColonnes <- ""
for (j in 1:length(colnames(dbimages))){
  nomsColonnes <- paste(nomsColonnes,colnames(dbimages)[j])
}
if (grepl("contraste", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, contraste=0)
}
if (grepl("complexite", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, complexite=0)
}
if (grepl("symetrieVerti", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, symetrieVerti=0)
}
if (grepl("symetrieHoriz", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, symetrieHoriz=0)
}

#Etiquetage
for (i in 1:nb_lignes){ 
  image <- dbimages$FichierImg[i]
  chemin_img <- paste(dossier_img,image,sep = "/")
  if (image == "") 
    {
    dbimages$contraste[i] <- 0
    dbimages$complexite[i] <- 0
    dbimages$symetrieVerti[i] <- 0
    dbimages$symetrieHoriz[i] <- 0
    } else {
    image_lue <- img_read(chemin_img)
    contraste <- imagefluency::img_contrast(image_lue)
    try(complexite <- imagefluency::img_complexity(image_lue))
    try(symetrie <- imagefluency::img_symmetry(image_lue))
    
    dbimages$contraste[i] <- contraste
    dbimages$complexite[i] <- complexite
    dbimages$symetrieVerti[i] <- symetrie[1] 
    dbimages$symetrieHoriz[i] <- symetrie[2]
    }
  #pour monitorer l'analyse dans la console
  print(paste("Fini : image ", i, " sur ", nb_lignes, sep = ""))
  
  contraste <- 0
  complexite <- 0
  symetrieVerti <- 0
  symetrieHoriz <- 0
  symetrie[1] <- 0
  symetrie[2] <- 0
}

saveRDS(dbimages, file=chemin_dbimages)

print("Etiquetage fini : Constraste, Complexite, Symetrie")
