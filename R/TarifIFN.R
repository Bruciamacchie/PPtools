#' Tarif de cubage volume géométrique bois fort
#'
#' @description La fonction renvoie un tableau des types et numéros de tarif Schaeffer par essence
#' en utilisant la base de données arbres de l'IFN. Il s'agit d'un volume géométrique bois fort tige.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif,
#' sylvoécorégion, etc.
#'
#' @return La fonction renvoie un tableau des types et numéros de tarif Schaeffer par essence.
#'
#' @param perim = objet au format sf. Si NULL ouverture d'une boîte de dialogue permettant de sélectionner
#' un fichier shp contenant des POLYGON. Par défaut perim=NULL.
#' @param SeuilCircf = seuil minimal de circonférence en cm en dessous duquel l'arbre n'est pas retenu
#' pour le calcul du tarif. Par défaut le seuil est fixé à 50 cm.
#' @param SeuilNb = seuil minimal du nombre d'arbres dans la base IFN pour qu'une essence soit retenue.
#' Par défaut ce seuil est égal à 10.
#' @param pas = résolution (par défaut = 1000 m)
#' @param UseSer = argument permettant de choisir si le calcul des numéros de tarif doit se faire
#' au sein d'une même sylvoécorégion. Par défaut, UseSer=TRUE.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .xlsx.
#' Par défaut enreg=FALSE.
#'
#' @import tidyverse
#' @import sp
#' @import sf
#' @import tools
#' @import tcltk
#' @import gstat
#' @import openxlsx
#'
#' @author Bruciamacchie Max
#'
#' @export

TarifIFN <- function(perim=NULL, SeuilCircf=50, SeuilNb=5, pas=1000, UseSer=F, enreg=F) {
  # ------------ Gestion du perimetre ----------------------------------------------
  perimetre <- Find_Verif_poly(perim)

  # ------------ Extraction placettes et arbres ------------------
  placettes <- ExtractPlac(perimetre) %>%
    dplyr::select(idp:yl93)
  if (UseSer) {
    placettes <- placettes %>% st_intersection(ser)
  }
  
  Echan <- IFNarbres %>%
    filter(!is.na(v)) %>%
    filter(idp %in% placettes$idp) %>%
    filter(c13 >=SeuilCircf) %>% 
    mutate(code = as.character(espar))

  # ------------- simple calcul de moyennes -------------
  Volumes <- Echan  %>%
    rename(Essence = espar,
           Vol = v) %>%
    mutate(Diam = c13/pi) 
    
  sortie <- TarifFindSch(Volumes)
  df <- sortie$tab %>%
    rename(code=Essence) %>%
    mutate(code = as.character(code)) %>% 
    left_join(CodesEssIFN, by = "code")

  Num <- df %>%
    dplyr::select(code, SchR:SchTL) %>%
    pivot_longer(cols=c(SchR:SchTL), names_to = "Type", values_to = "Num")

  Moys <- df %>%
    dplyr::select(code,libelle, SchRcv:SchTLcv) %>%
    pivot_longer(cols=c(SchRcv:SchTLcv), names_to = "Type", values_to = "CV") %>%
    mutate(Type = substr(Type,1,nchar(Type)-2)) %>%
    left_join(Num, by = c("code", "Type")) %>%
    group_by(code) %>%
    arrange(CV) %>%
    slice(1) %>%
    left_join(df[,c("code","Nb")], by = "code") %>%
    dplyr::select(code,libelle,Nb,Type,Num,CV) %>%
    arrange(libelle)

  # ------------- krigeage -------------
  grd <- st_make_grid(st_buffer(placettes, dist=450), cellsize=pas) # Création grid

  Tarifs <- Echan %>%
    # filter(code %in% df$code) %>% 
    mutate(Diam = c13 / pi,
           SchR  = v/5*70000/(Diam-5)/(Diam-10)-8,
           SchI  = v/5*80000/(Diam-2.5)/(Diam-7.5)-8,
           SchL  = v/5*90000/Diam/(Diam-5)-8,
           SchTL = v/5*101250/Diam^2-8) %>%
    left_join(placettes, by="idp") %>%
    dplyr::select(espar,xl93,yl93,SchR,SchI,SchL,SchTL) %>%
    group_by(espar,xl93,yl93) %>%
    summarise_all(.funs=mean) %>%
    st_as_sf(coords=c("xl93","yl93"), crs=2154)
  
  df <- Tarifs %>% 
    st_drop_geometry() %>%
    mutate(code = as.character(espar)) %>%
    group_by(code) %>% 
    summarise(Freq = n()) %>% 
    filter(Freq >= SeuilNb) %>% 
    left_join(CodesEssIFN, by = "code")

  shp <- st_sf(st_sfc(), crs=2154)
  for (i in 1:dim(df)[1]) {
    print(paste0("Traitement de l'essence : ", df[i,"libelle"]))
    for (j in 2:5) {
      t <- Tarifs %>%
        filter(espar==paste0(df[i,1],"")) %>%
        dplyr::select(j)
      names(t)[1] <- "Z"
      temp <- t %>%
        as_Spatial()
      v <- variogram(Z ~ 1, data=temp, cutoff=20000, width = 1)
      vmf <- fit.variogram(v, vgm(c("Exp", "Sph", "Mat", "Ste"), psill=50, range = 5000, nugget = 1))
      # plot(v, pl = T, model = vmf)
      k <- krige(Z ~ 1, locations = t, newdata = grd, model = vmf) %>%
        st_intersection(perimetre)
      k <- k %>% mutate(code = as.character(df[i,1]),
                        Type = names(Tarifs)[j])
      shp = rbind(shp, k)
    }
  }
  shp <- shp %>%
    mutate(Type = factor(Type, levels=c("SchR","SchI","SchL","SchTL"))) %>%
    rename(Num = var1.pred)

  g <- ggplot() +
    geom_sf(data=shp, aes(fill=Num)) +
    facet_grid(code ~ Type) +
    scale_fill_gradientn(colours = rainbow(5)) +
    theme_bw() + labs(fill="Numéro") +
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_blank())

  opt <- shp %>%
    st_drop_geometry() %>%
    group_by(code,Type) %>%
    summarise(var = mean(var1.var),
              Num = mean(Num)) %>%
    arrange(var) %>%
    slice(1) %>%
    mutate(Id = paste(code,Type,sep="_"))

  tabopt <- opt %>%
    left_join(CodesEssIFN, by = "code") %>%
    dplyr::select(code,libelle,Type,Num) %>%
    mutate(Num = round(Num,1))

  shp1 <- shp %>%
    mutate(Id = paste(code,Type,sep="_")) %>%
    filter(Id %in% opt$Id) %>%
    mutate(Tarif = paste(code,Type,sep="_"))


  gopt <- ggplot() +
    geom_sf(data=shp1, aes(fill=Num)) +
    facet_wrap( ~ Tarif, ncol=3) +
    scale_fill_gradientn(colours = rainbow(5)) +
    theme_bw() + labs(fill="Numéro") +
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_blank()) +
    theme(legend.position="bottom")

  tab <- shp1 %>%
    st_drop_geometry() %>%
    group_by(code,Type) %>%
    summarise(Num = round(mean(Num),1)) %>%
    left_join(CodesEssIFN, by='code') %>%
    dplyr::select(code,libelle,Type,Num)

  if (enreg) {
    wb <- createWorkbook()
    addWorksheet(wb, "data")
    writeData(wb, "data", tab)
    dir.create("Out", showWarnings = F)
    saveWorkbook(wb, "Out/tarifIFN.xlsx", overwrite = TRUE)
    print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
  }
  out=list(tab, gopt, Moys, sortie$graph, shp, g)
  names(out) <- c("Ktab","Kgraph", "MoyTab","MoyGraph","Ksf","KgraphTous")
  return(out)
}



