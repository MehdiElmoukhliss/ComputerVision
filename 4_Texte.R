
#v2.0 EXTRAIT LE TEXTE PRESENT DANS DES IMAGES (FR+ENG)

#INSTALLATION
if (!require("tesseract")) install.packages("tesseract")
if (!require("magick")) install.packages("magick")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

  #telecharger les dictionnaires :
  tesseract::tesseract_download("fra")  #pas le meilleur
  tesseract::tesseract_download("eng")    #pas le meilleur
  #cf https://github.com/tesseract-ocr/tessdata

library("dplyr")
library("tesseract")

  
#VARIABLES LOCALES ! A ADAPTER
chemin_dbimages <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/dbImages.rds"  #BDD a etiqueter
dossier_img <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/images"  #dossier contenant les images

#Parametrage
PallierConfidence <- 30   #Seuil de confiance. Va de 0 (garde tout) à 100 
langue <- "FRENG"   #langue. Accepte : FR, ENG ou FRENG

if (langue == "FR") dico <- tesseract::tesseract("fra")  
if (langue == "ENG") dico <- tesseract::tesseract("eng") 
if (langue == "FRENG") dico <- tesseract::tesseract(c("fra", "eng"))

dbimages <- readRDS(chemin_dbimages) 

#Creer la colonne
nomsColonnes <- ""
for (j in 1:length(colnames(dbimages))){
  nomsColonnes <- paste(nomsColonnes,colnames(dbimages)[j])
}
if (grepl("Texte", nomsColonnes) == FALSE){
  dbimages <- cbind(dbimages, Texte="")
}

# TEST (desactive)
#nb_lignes <- 5 #analyser seulement les 5 premieres images

#RUN
nb_lignes <- nrow(dbimages)  #analyser toutes les images

#Etiquetage
for (i in 1:nb_lignes)  #***************************

{

  image <- dbimages$FichierImg[i]
  chemin_img <- paste(dossier_img,image,sep = "/")
  
  if (image != "") 
  {
    image_traitee <- magick::image_read(chemin_img)

    #PRE-TRAITEMENT
    image_traitee <- magick::image_resize(image_traitee, 
                                          magick::geometry_size_percent(width = 150, height = 150)) #augmenter la définition (%)
    image_traitee <- magick::image_reducenoise(image_traitee)  #reduc. bruit
    try(image_traitee <- magick::image_contrast(image_traitee, sharpen = 1)) #augm. contraste (try car cree parfois une erreur)
    image_traitee <- magick::image_enhance(image_traitee) #augm. nettete
    image_traitee <- magick::image_convert(image_traitee, type = "grayscale") #noir & blanc
    try(image_traitee <- magick::image_contrast(image_traitee, sharpen = 1)) #augm. contraste (try car cree parfois une erreur)
    try(image_traitee <- magick::image_contrast(image_traitee, sharpen = 1)) #augm. contraste (try car cree parfois une erreur)
        
    #EXTRACTION ET NETTOYAGE DU TEXTE
    #stats <- tesseract::ocr_data(image, engine = langue) #si on veut un vecteur stats + coordonnées box
    texte_brut <- tesseract::ocr_data(image_traitee, engine = dico)
    
    texte_filtre <- texte_brut %>% filter(confidence > PallierConfidence) #filtre > seuil de confiance
    le_texte <- paste(texte_filtre$word, collapse = ",")
    le_texte <- iconv(le_texte, from = "UTF-8", to ="") %>% #regle pb encodage
      stringr::str_replace_all("\n", " ") %>% #enlever les \n
      stringr::str_replace_all("[:punct:]", " ") #transforme ponctuation en ","
    le_texte <- stringr::str_replace_all(le_texte, "[^[:alnum:]]", " ") #enlever les caracteres non alphanumeriques
    le_texte <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", le_texte) # Enlever les mots de 1 a 2 lettres 
    le_texte <- gsub('[0-9]+', '', le_texte) #enlever les chiffres
    le_texte <- gsub("^ +| +$|( ) +", "\\1", le_texte) # Enlever les espaces en trop
    
    dbimages$Texte[i] <- as.character(le_texte)
    
    #monitoring
    print(paste("Image ", i, " sur ", nb_lignes, " | ", "Fichier = ", image, " | Texte extrait = ", le_texte, sep=""))
  }
  else
  {
  print(paste("Image num.", i, " : INEXISTANTE. Passage à l'image suivante.", sep=""))
  }
}
saveRDS(dbimages, file=chemin_dbimages)

print("Etiquetage fini : Texte")

