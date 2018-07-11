#chargement des paquetages nécessaires
library(osmdata)
library(sf)
library(units)
#library(rgdal) 
#require("rgdal")
#require("rgeos")
library("dplyr")
library("tidyverse")
library("tidyr")
library("stringr")
library(cartography)
library("DT")
library("formattable")


#routes nationales
#définition des bornes 
q0 <- opq(bbox = c(-75.04211,17.34015 , -71.59241, 20.42186))
#extraction des relations 
q1 <- add_osm_feature(opq = q0, key = "network", value = "HT:RN-road")
#transformation requete en sf
res1 <- osmdata_sf(q1)
# prise en compte géométrie relations routes
route <- st_geometry(res1$osm_multilines)
#utiliser SCR utm 18n Haïti
route <- st_transform(route, 32618)
#calculer longueur chaque route
res1$osm_multilines$longueurOsm <- st_length(res1$osm_multilines)/1000
#sélection des colonnes à garder ref et longueur
osm <- res1$osm_multilines[,c(7,11)]
osm <- distinct(osm, ref, .keep_all = TRUE)


#charger shp routes ministere
mtptc <- read_sf("~/Documents/images/vecteur/reseau_routier_haiti/reseau_routier_haiti.shp",  quiet = TRUE, stringsAsFactors = FALSE)
# sélectionner uniquement routes départementales
mtptc <- mtptc[mtptc$COD1_TYP == "RN",]
# dissoudre en fonction ref de routes
mtptc <- mtptc %>% group_by(mtptc$CODE_MTPTC) %>% summarize()
#calcul longueur
mtptc$longueurM <- st_length(mtptc)/1000
# passage en data.frame pour permettre fusion 2 tableaux
mtptc <- as.data.frame(mtptc)
#on retire la colonne géométrie
mtptc <- mtptc[,-c(2)]
#calcul nombre caractères réf dans osm pour choisir comment les concaténer
osm$nbc <- str_length(osm$ref)
# concaténation des ref dans osm pour que cela corresponde à celles du ministère
osm$ref <- ifelse(osm$nbc == 4, paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -1)), sep  = ""),
                  paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -2,-1)), sep  = ""))
                  


#fusion tableau mtptc et osm par les références des routes
compar <- full_join(mtptc, osm, by = c("mtptc$CODE_MTPTC" = "ref"))
#activation des variables en entrée directe
attach(compar)
# arrondi longueur à 2 décimales 
compar$longueurM <- round(compar$longueurM, digits = 2)
compar$longueurOsm <- round(compar$longueurOsm, digits = 2)
#calcul de la différence entre distance Ministère et OSM
compar$diffL <- longueurM - longueurOsm
compar$diffL <- round(compar$diffL, digits = 2)
#ajout colonne pourcentage finition dans osm par rapport Ministère
compar$pourdiff <- longueurOsm *100/ longueurM
#arrondi à deux décimales
compar$pourdiff<-  round(compar$pourdiff, digits = 2)
#renommer colonne
#compar$ref <- colnames(compar$`mtptc$CODE_MTPTC`)  

#retire colonne géométrie et nbc
compar <- compar[,-c(4,5)]
#compar <- compar[-c(36,37),]


#affichage du résultat dans un tableau
formattable(compar, list(
  longueurM = color_tile("white", "orange"),
  area(col = c(diffL, pourdiff)) ~ normalize_bar("pink", 0.2)
))


#routes départementales
#définition des bornes 
q0 <- opq(bbox = c(-75.04211,17.34015 , -71.59241, 20.42186))
#extraction des relations 
q1 <- add_osm_feature(opq = q0, key = "network", value = "HT:RD-road")
#transformation requete en sf
res1 <- osmdata_sf(q1)
# prise en compte géométrie relations routes
route <- st_geometry(res1$osm_multilines)
#utiliser SCR utm 18n Haïti
route <- st_transform(route, 32618)
#calculer longueur chaque route
res1$osm_multilines$longueurOsm <- st_length(res1$osm_multilines)/1000
#sélection des colonnes à garder ref et longueur
osm <- res1$osm_multilines[,c(7,12)]


#charger shp routes ministere
mtptc <- read_sf("reseau_routier_haiti/reseau_routier_haiti.shp",  quiet = TRUE, stringsAsFactors = FALSE)
# sélectionner uniquement routes départementales
mtptc <- mtptc[mtptc$COD1_TYP == "RD",]
# dissoudre en fonction ref de routes
mtptc <- mtptc %>% group_by(mtptc$CODE_MTPTC) %>% summarize()
#calcul longueur
mtptc$longueurM <- st_length(mtptc)/1000
# passage en data.frame pour permettre fusion 2 tableaux
mtptc <- as.data.frame(mtptc)
#on retire la colonne géométrie
mtptc <- mtptc[,-c(2)]
#calcul nombre caractères réf dans osm pour choisir comment les concaténer
osm$nbc <- str_length(osm$ref)
# concaténation des ref dans osm pour que cela corresponde à celles du ministère
osm$ref <- ifelse(osm$nbc == 5, paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -2,-1)), sep  = ""),
                   ifelse(osm$nbc == 6, paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -3,-1)), sep  = ""),
                          paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -4,-1)), sep  = ""))
                   )

#fusion tableau mtptc et osm par les références des routes
compar <- full_join(mtptc, osm, by = c("mtptc$CODE_MTPTC" = "ref"))
#activation des variables en entrée directe
attach(compar)
# arrondi longueur à 2 décimales 
compar$longueurM <- round(compar$longueurM, digits = 2)
compar$longueurOsm <- round(compar$longueurOsm, digits = 2)
#calcul de la différence entre distance Ministère et OSM
compar$diffL <- longueurM - longueurOsm
compar$diffL <- round(compar$diffL, digits = 2)
#ajout colonne pourcentage finition dans osm par rapport Ministère
compar$pourdiff <- longueurOsm *100 / longueurM

#arrondi à deux décimales
compar$pourdiff<-  round(compar$pourdiff, digits = 2)
#renommer colonne
#compar$ref <- colnames(compar$`mtptc$CODE_MTPTC`)  

#retire colonne géométrie et nbc
compar <- compar[,-c(4,5)]
#compar <- compar[-c(36,37),]

#affichage du résultat dans un tableau
formattable(compar, list(
  longueurM = color_tile("white", "orange"),
  area(col = c(diffL, pourdiff)) ~ normalize_bar("pink", 0.2)
))

#routes communales
#définition des bornes 
q0 <- opq(bbox = c(-75.04211,17.34015 , -71.59241, 20.42186))
#extraction des relations 
q1 <- add_osm_feature(opq = q0, key = "network", value = "HT:RC-road")
#transformation requete en sf
res1 <- osmdata_sf(q1)
# prise en compte géométrie relations routes
route <- st_geometry(res1$osm_multilines)
#utiliser SCR utm 18n Haïti
route <- st_transform(route, 32618)
#calculer longueur chaque route
res1$osm_multilines$longueurOsm <- st_length(res1$osm_multilines)/1000
#sélection des colonnes à garder ref et longueur
osm <- res1$osm_multilines[,c(4,9)]
#permet d’enlever les doubles
#osm <- distinct(osm, ref, .keep_all = TRUE)


#charger shp routes ministere
comtp <- read_sf("~/Documents/images/vecteur/reseau_routier_haiti/reseau_routier_haiti.shp",  quiet = TRUE, stringsAsFactors = FALSE)
# sélectionner uniquement routes départementales
comtp <- comtp[comtp$COD1_TYP == "RC",]
# dissoudre en fonction ref de routes
comtp <- comtp %>% group_by(comtp$CODE_MTPTC) %>% summarize()
#calcul longueur
comtp$longueurM <- st_length(comtp)/1000
# passage en data.frame pour permettre fusion 2 tableaux
comtp <- as.data.frame(comtp)
#on retire la colonne géométrie
comtp <- comtp[,-c(2)]
#calcul nombre caractères réf dans osm pour choisir comment les concaténer
osm$nbc <- str_length(osm$ref)
# concaténation des ref dans osm pour que cela corresponde à celles du ministère
osm$ref <- ifelse(osm$nbc == 7, paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref,-4, -1)), sep  = ""),
                  paste(c(str_sub(osm$ref,1,2)),(str_sub(osm$ref, -2,-1)), sep  = ""))



#fusion tableau comtp et osm par les références des routes
commu <- full_join(comtp, osm, by = c("comtp$CODE_MTPTC" = "ref"))
#activation des variables en entrée directe
attach(commu)
# arrondi longueur à 2 décimales 
commu$longueurM <- round(commu$longueurM, digits = 2)
commu$longueurOsm <- round(commu$longueurOsm, digits = 2)
#calcul de la différence entre distance Ministère et OSM
commu$diffL <- longueurM - longueurOsm
commu$diffL <- round(commu$diffL, digits = 2)
#ajout colonne pourcentage finition dans osm par rapport Ministère
commu$pourdiff <- longueurOsm *100/ longueurM
#arrondi à deux décimales
commu$pourdiff<-  round(commu$pourdiff, digits = 2)
#renommer colonne
#commu$ref <- colnames(commu$`mtptc$CODE_MTPTC`)  

#retire colonne géométrie et nbc
commu <- commu[,-c(4,5)]
#commu <- commu[-c(36,37),]


#affichage du résultat dans un tableau
formattable(commu, list(
  longueurM = color_tile("white", "orange"),
  area(col = c(diffL, pourdiff)) ~ normalize_bar("pink", 0.2)
))




#affiche résultats dans tableau DT
datatable(compar)
#changement nom colonnes 
datatable(head(compar), colnames = c('Réf' = 'mtptc$CODE_MTPTC'))
#tableau croisé dynamique
# sélectionner uniquement les routes RD
#rd <- mtptc[`mtptc$CODE_MTPTC`=="RD",]
#table(mtptc$COD1_TYP)

#table(mtptc$COD1_TYP, mtptc$LENGTH)

#datatable(compar) %>% 
  #changement nom colonnes 
 datatable(compar, colnames = c('Réf' = 'mtptc$CODE_MTPTC')) %>%
  #formatStyle('longueurM', fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
   'pourdiff',
   target = 'row',
   backgroundColor = styleInterval(.5,c('red', 'green')
)) %>%
  formatPercentage('pourdiff', digits = 2, interval = 3, mark = ",", 
                    dec.mark = ',')
 
 # using styleColorBar
 datatable(compar) %>% formatStyle('pourdiff',

                               background = styleColorBar(range(compar$pourdiff), 'blue'),
                               backgroundSize = '98% 70%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center') %>%

   datatable(compar) %>%
   formatStyle(
     'longueurM',
     background = styleColorBar(compar$longueurM, 'steelblue'),
     backgroundSize = '100% 80%',
     backgroundRepeat = 'no-repeat',
     backgroundPosition = 'center'
   )
 

 
 
# save data to avoid over downloading
#save(list= c("route"), 
 #    file = "route_osm.RData", compress = "xz")

#plot(route)




