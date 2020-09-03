
#Script permettant de compter le nombre d'images / vidéos / gifs dans un corpus de tweets.
#v2.0

library(dplyr)

#recuperation de la base de donnees de travail
adressedb <- "C:/R&D3/MES PAPIERS/Images/_EnvironnementTest/df_consoj12.rds"  ### ATTENTION : CHEMIN LOCAL 

#tout

options("scipen"=100, "digits"=3) #desactive l'affichage e+0x des nombres 

db <- readRDS(adressedb)   
  db_brute_nb <- nrow(db)   #  nb total de tweets (inclu les retweets)

  #sans RT
  db_sansRT <- db %>% filter(is_retweet == FALSE) 
    NoRT_nb <- nrow(db_sansRT)    #  nb de tweets (sans les retweets)
    RT_nb <- db_brute_nb - NoRT_nb   # nb de retweets
    RatioTweetSansRT <- NoRT_nb / db_brute_nb
      
    #Images et video only
    db_imgETvid <- subset(db_sansRT, db_sansRT$media_type == "photo")
      db_imgvid_nb <- nrow(db_imgETvid)   #nb de tweets avec un media (image, gif ou video)
    
      nb_videos <- nrow(subset(db_imgETvid, grepl('video', media_expanded_url))) #nb videos
      nb_img <- nrow(subset(db_imgETvid, !grepl('video', media_expanded_url)))   #nb images (inclu les GIF)
    
      #Images only
      db_OnlyImg <- filter(db_imgETvid, !grepl('video', media_expanded_url)) 
        OnlyImgETgif_nb <- nrow(db_OnlyImg)  #nb images (inclu les GIF)
      
        nb_gif <- nrow(subset(db_OnlyImg, grepl('video_thumb', media_url)))
        nb_jpegpng <- nrow(subset(db_OnlyImg, !grepl('video_thumb', media_url)))

        #JPEG et PNG only
        db_jpegpng_NoRT <- filter(db_OnlyImg, !grepl('video_thumb', media_url)) 
          nb_jpegpng_verif <- nrow(db_jpegpng_NoRT)
    
          #ratios
          nb_jpegpng_sur_totalbrut <- nb_jpegpng / db_brute_nb  # ~ 6 %
          nb_jpegpng_sur_tweetsSansRT <- nb_jpegpng / NoRT_nb   # ~ 30 %
          nb_jpegpng_sur_img_et_vid <- nb_jpegpng / db_imgvid_nb  # ~ 70 %
          
          nb_textonly <- NoRT_nb - db_imgvid_nb


overview1_tweets_et_RT <- data.frame(
  Objet = c("Nb de tweets bruts", 
            "Nb tweets sans retweets",
            "Prop. tweets sans RT"),
  Valeur = c(db_brute_nb, 
             NoRT_nb,
             RatioTweetSansRT))

overview2 <- data.frame(
  Objet = c("Nb tweets (avec RT)",
            "Nb tweets (sans RT)",  
            "Nb tweets sans media",
            "Nb tweets avec un media",
            "Nb tweets avec une video",
            "Nb tweets avec un GIF",
            "Nb tweets avec une image (JPEG ou PNG)",
            "Prop. de tweets avec image / total des tweets hors RT"),
  
  Valeur = c(db_brute_nb,
             NoRT_nb, 
             nb_textonly,
             db_imgvid_nb,
             nb_videos,
             nb_gif,
             nb_jpegpng,
             nb_jpegpng_sur_tweetsSansRT))

overview1_tweets_et_RT
overview2



#plot
# library(ggplot2)
# library(scales)

# #MB NOT      
# #plot
# le_plot<- ggplot(nb_synthese_sansRT, aes(x="", y=valeur, fill=Media))+
#   geom_bar(width = 1, stat = "identity")
# 
# pie <- le_plot + coord_polar("y", start=0)
# 
# pie




          