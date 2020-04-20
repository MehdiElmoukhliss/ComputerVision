#ANALYSE DU TEXTE

library(dplyr)
library(knitr)
library(magick)
library(tesseract)  

tesseract_download("fra")  #telecharge langage (~1 Mo)
tesseract_download("eng")  #telecharge langage (~1 Mo)

langue <- tesseract(c("fra", "eng")) #francais et anglais

#ETAPE0 : PREPARATION

#recuperer la base d'image de travail
CheminEtFichierNewBase <- "C:/R&D2/IA/AnalyseImages/dbImages.rds"
dbPhoto <- readRDS(CheminEtFichierNewBase)

#creation d'une colonne "texte de l"image" si elle n'existe pas
nomsColonnes <- ""
for (i in 1:length(colnames(dbPhoto))){
  nomsColonnes <- paste(nomsColonnes,colnames(dbPhoto)[i])
}
if (grepl("TexteImage", nomsColonnes) == FALSE){
  dbPhoto <- cbind(dbPhoto, TexteImage="_TXT PAS ENCORE DETECTE")
  dbPhoto$TexteImage <- as.character(dbPhoto$TexteImage) #force le type de la nouvelle colonne en Character
}

#lecture des images contenues dans le dossier - dossier contenant (uniquement) les images a traiter
dossier_img <- "C:/R&D2/IA/AnalyseImages/ImgTxt_fichiersImg/"  #ADD PROMPT
list_img <- dir(dossier_img) #recupere le nombre de fichiers

for (i in 1:length(list_img)){  #on lance la boucle de preproc.
  
  image <- list_img[i]
  chemin_img <- paste(dossier_img,image,sep = "")
  
  #ETAPE1 : PREPROCESSING
    image_preproc <- magick::image_read(chemin_img) %>% #charge img
      image_convert(type = "grayscale") %>% #noir et blanc
      image_trim() %>% #?
      #image_noise() %>%
      image_enhance() %>%
      image_normalize() %>%
      image_contrast(sharpen = 2) %>%
      image_reducenoise() #%>%
      #image_quantize() %>% #diminue le nombre de couleurs
      #image_browse(image_retraitee)
  
  
  #ETAPE2: Analyse du texte
    result_img <- ocr_data(image_preproc, engine = langue)
    
    ConfidenceMin <- 50    #On retient le texte avec filtre interv. conf. < 30
    resultimg <- result_img %>% filter(confidence > ConfidenceMin) 
    
    le_texte <- paste(result_img$word, collapse = ",") %>% 
      stringr::str_replace_all("\n", " ") %>% #enlever les \n
      stringr::str_replace_all("[:punct:]", " ") 
    
    le_texte <- str_replace_all(le_texte, "[^[:alnum:]]", " ") #enlever les caracteres non alphanumeriques
    le_texte <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", le_texte) # Enlever les mots de 1 a 2 lettres 
    le_texte <- gsub('[0-9]+', '', le_texte) #enlever les chiffres
    le_texte <- gsub("^ +| +$|( ) +", "\\1", le_texte) # Enlever les espaces en trop
  
  #enregistrer dans la base
  dbPhoto$TexteImage[i] <- as.character(le_texte)
  
  #message de controle
  print(paste("Image #", i, " - ", "Fichier = ", image, " | Texte extrait = ", le_texte, sep=""))
  
}

print(paste("Fin de l'identification du texte. La base a ete enrichie. ", "Voir: ", CheminEtFichierNewBase, sep =""))


