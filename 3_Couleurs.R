#v 2.0 Extrait les informations colorimetriques d'images (RGB, HSV)

#INSTALLATION
if (!require("ImaginR")) install.packages("ImaginR")

#VARIABLES LOCALES ! A ADAPTER
chemin_dbimages <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/dbImages.rds"  #BDD a etiqueter
dossier_img <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/images"  #dossier contenant les images

#BOUCLE ET ETIQUETAGE
dbimages <- readRDS(chemin_dbimages) 

#TEST (desactive)
nb_lignes <- 3

#RUN
#nb_lignes <- nrow(dbimages)

for (i in 1:nb_lignes){
  
  image <- dbimages$FichierImg[i]
  chemin_img <- paste(dossier_img,image,sep = "/")
  if (image != "") 
  {
    
  CouleursImage <- ImaginR::PictureResults(imager::load.image(chemin_img))
  
  #RGB
  dbimages$CouleurMoyenne[i] <- CouleursImage$const$color  #code HEX de la couleur moyenne de l'img
  dbimages$R_color[i] <- CouleursImage$data$hsv[1]#R
  dbimages$G_color[i] <- CouleursImage$data$hsv[2]#G
  dbimages$B_color[i] <- CouleursImage$data$hsv[3]#B

  #HSV
  HueSatuBright <- rgb2hsv (R, G, B)
  dbimages$Onde_Dominante[i] <- HueSatuBright[1]  #longueur d'onde dominante
  dbimages$Saturation[i] <- HueSatuBright[2]      #saturation
  dbimages$Eclairage[i] <- HueSatuBright[3]       #brightness / eclairage

  #pour monitorer l'analyse dans la console
  print(paste("FAIT : image ", i, " sur ", nb_lignes, sep = ""))

  }
  
}
print("Etiquetage fini : couleurs")





