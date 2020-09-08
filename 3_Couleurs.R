#v 2.0 Extrait les informations colorimetriques d'images (RGB, HSV)

#INSTALLATION
if (!require("ImaginR")) install.packages("ImaginR")

#VARIABLES LOCALES ! A ADAPTER
chemin_dbimages <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/dbImages.rds"  #BDD a etiqueter
dossier_img <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/images"  #dossier contenant les images

#BOUCLE ET ETIQUETAGE
dbimages <- readRDS(chemin_dbimages) 

#Creer les colonnes
nomsColonnes <- ""
for (j in 1:length(colnames(dbimages))){
  nomsColonnes <- paste(nomsColonnes,colnames(dbimages)[j])
}
if (grepl("R_color", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, R_color=0)
}
if (grepl("G_color", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, G_color=0)
}
if (grepl("B_color", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, B_color=0)
}
if (grepl("CouleurMoyenne", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, CouleurMoyenne=0)
}
if (grepl("Onde_Dominante", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, Onde_Dominante=0)
}
if (grepl("Saturation", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, Saturation=0)
}
if (grepl("Eclairage", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, Eclairage=0)
}

#TEST (desactive)
#nb_lignes <- 3

#RUN
nb_lignes <- nrow(dbimages)

#for (i in 1:nb_lignes){
for (i in 1656:nb_lignes){  
  image <- dbimages$FichierImg[i]
  chemin_img <- paste(dossier_img,image,sep = "/")
  if (image == "") {
  dbimages$R_color[i] <- 0
  dbimages$G_color[i] <- 0
  dbimages$B_color[i] <- 0
  dbimages$CouleurMoyenne[i] <- 0
  dbimages$Onde_Dominante[i] <- 0
  dbimages$Saturation[i] <- 0
  dbimages$Eclairage[i] <- 0
  } else if (image == "1656_1243876693846167552.jpg") {  #image 1656 mal encodée, exclue
  dbimages$R_color[i] <- 0
  dbimages$G_color[i] <- 0
  dbimages$B_color[i] <- 0
  dbimages$CouleurMoyenne[i] <- 0
  dbimages$Onde_Dominante[i] <- 0
  dbimages$Saturation[i] <- 0
  dbimages$Eclairage[i] <- 0  
  } else {
  CouleursImage <- ImaginR::PictureResults(imager::load.image(chemin_img)) 
  #RGB
  R <- CouleursImage$data$hsv[1]
  G <- CouleursImage$data$hsv[2]
  B <- CouleursImage$data$hsv[3]
  dbimages$R_color[i] <- R
  dbimages$G_color[i] <- G
  dbimages$B_color[i] <- B
  dbimages$CouleurMoyenne[i] <- CouleursImage$const$color  #code HEX de la couleur moyenne de l'img
  #HSV
  HueSatuBright <- rgb2hsv (R, G, B)
  dbimages$Onde_Dominante[i] <- HueSatuBright[1]  #longueur d'onde dominante
  dbimages$Saturation[i] <- HueSatuBright[2]      #saturation
  dbimages$Eclairage[i] <- HueSatuBright[3]       #brightness / eclairage
  }
  #monitoring
  print(paste("FAIT : image ", i, " sur ", nb_lignes, sep = ""))

}

saveRDS(dbimages, file=chemin_dbimages)

print("Etiquetage fini : couleurs")



