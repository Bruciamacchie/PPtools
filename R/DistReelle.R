#' Calcul des distances théoriques d'une maille en distance réelle (intégration de la pente)
#'
#' @description La fonction permet de calculer les distances réelles sur le terrain en intégrant la pente
#' à partir d'une maille qui donne les distances horizontales entre deux placettes
#'
#' @return La fonction renvoie un tableau de données avec les distances réelles en mètre entre les placettes (les plus proches).
#'
#' @param shp = shape de la maille du réseau de placettes permanentes (points). Table attributaire avec seulement identifiant de la placette
#' @param perim = shape du périmètre de la zone d'étude (polygone)
#' @param mnt = mnt en format raster de la zone d'étude
#' @param nom = nom du fichier de sortie. Par défaut MailleDistReelle
#'
#' @import tcltk
#' @import sf
#' @import tidyverse
#' @import raster
#' @import elevatr
#' @import openxlsx
#'
#' @author Marie-Laure Martin
#'
#' @export



# Maille <- st_read("~/Desktop/Auto-entreprise/Travail/RPP_Wegscheid/MailleAut.shp")
# Perim <- st_read("~/Desktop/Auto-entreprise/Travail/RPP_Wegscheid/SIG/perimetre.shp")
# mnt <- raster("~/Desktop/Auto-entreprise/Travail/RPP_Wegscheid/MNT.tif")


CalculDistReelle <- function(shp,perim, mnt, nom="MailleDistReelle.shp"){
  newproj <- "+init=epsg:2154"
  mnt <- projectRaster(mnt, crs=newproj)
  mnt <- crop(mnt, as(perim, "Spatial"))

  Maille <- shp %>% 
    mutate(Alti = raster::extract(mnt, as(shp, "Spatial"))) # données d'altitude des points

  Distance <- as.data.frame(st_distance(Maille)) # distance entre les points (toutes les combinaisons)

  Dist <- Distance %>% 
    dplyr::mutate(Name = row.names(Distance)) %>% 
    pivot_longer(cols = starts_with("V"), names_to = c("Dist_au_point"),
                values_to = "Numero") %>% 
    mutate(Dist_au_point = str_replace(Dist_au_point,"V",""),
           Dist_au_point = as.numeric(Dist_au_point),
           Name = as.numeric(Name))
  
  Dist <- Dist %>% 
    left_join(Maille, by = c("Name"="name"))
  tab <- Dist %>% 
    left_join(Maille, by= c("Dist_au_point"="name"))
  tab <- tab %>% 
    dplyr::select(Name, Dist_au_point, Numero, Alti.x, Alti.y)
  tab <- tab %>% 
    mutate(Numero = as.numeric(Numero))
  tab <- tab %>% 
    mutate(dist_reelle = sqrt(Numero^2 + (Alti.x-Alti.y)^2))
  
  tab <- tab %>% 
    filter(Name != Dist_au_point) %>% 
    dplyr::select(Name, Dist_au_point,dist_reelle)
  tab <- tab %>% 
    rename(Placette1 = Name,
           Placette2 = Dist_au_point,
           Dist_relle = dist_reelle)
  tab <- tab %>% 
    arrange(Placette1, Dist_relle) %>% 
    group_by(Placette1) %>% 
    slice(1:4) # retient les 4 placettes les plus proches
  
  essai <- tab %>% 
    mutate(Somme = Placette1 + Placette2,
           id = paste(Dist_relle, Somme)) %>% # permet de distinguer les combinaisons égales (1-2, 2-1 etc) 
    mutate(From_To = paste(Placette1,Placette2, sep="-")) %>% 
    ungroup() %>% 
    dplyr::select(From_To, Dist_relle, id)
 
  Maille <- Maille %>% 
    left_join(tab, by = c("name"="Placette1")) %>% 
    rename(to_placette = Placette2)
  
  write.xlsx(essai, "DistancesRéelles.xlsx")
  # st_write(Maille, "MailleDistReelle.shp")
  }
  
  
    
  
  
  
