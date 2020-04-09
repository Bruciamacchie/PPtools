#' Cheminement pour accéder aux placettes
#' @description La fonction permet, à partir de la localisation des placettes et de la desserte
#' (routes, pistes, chemins), de déterminer le cheminement
#'
#' @return La fonction renvoie un tableau indiquant par placette la distance à parcourir.
#' Elle sauvegarde également le point de départ situé sur la desserte.
#'
#' @param parc = limites internes et externes (parcellaire, pistes, etc.).
#' @param CreateNetPP = Si TRUE (valeur par défaut) la fonction crée le réseau de placettes.
#' Si FALSE une boîte de dialogue permet de sélectionner le réseau de placettes.
#' @param Nb = nombre de placettes. Utile uniquement si CreateNetPP = TRUE.
#' @param enrg = Si TRUE (valeur par défaut) la fonction enregistre dans un fichier au format .shp
#' le point le plus proche de chaque placette situé sur une limite interne ou externe.
#' @param nom : nom du fichier (par défaut nom="Acces.shp"). Utile uniquement si enrg = TRUE.
#'
#' @import sf
#' @import nngeo
#' @import tcltk
#'
#' @author Bruciamacchie Max
#'
#' @export
#'
AccessPlot <- function(parc=NULL, CreateNetPP=T, Nb=30, enrg=T, nom="Acces.shp") {
  parc = ParFor
  # ----------- Import parcellaire + piste ---------
  parcelles <- Find_Verif_poly(parc) # Choix du parcellaire
  parcelles_u <- RmSmallPolys(parcelles)$Après
  lines <- parcelles %>%
    st_cast("POLYGON") %>%
    st_cast("LINESTRING")
  lines_m <- parcelles_u %>%
    st_cast("POLYGON") %>%
    st_cast("LINESTRING")
  # ----------- Import placettes -----------
  if(CreateNetPP) {
    Plac <- CreateNetPP(parcelles_u, Nb)$Plac
  } else {
    Plac <- Find_Verif_point(plac)
  }
  # ----------- Recherche point -----------
  nrst = st_nn(Plac, lines, returnDist = T)
  l = st_connect(Plac, lines, ids = nrst$nn)

  pts <- st_cast(l, "POINT") %>%
    st_sf() %>%
    slice(seq(2,dim(Plac)[1]*2,2)) %>%
    mutate(Num = Plac$Num,
           Dist = unlist(nrst$dist)) %>%
    dplyr::select(Num, Dist, geometry)

  # ----------- Analyse distances -----------
  # distances <- data.frame(Num = Plac$Num,
  #                         Dist = unlist(nrst$dist)) %>%
  distances <- pts %>%
    st_drop_geometry() %>%
    mutate(Classe = floor(Dist/10+0.5)*10)

  classDist <- distances %>%
    group_by(Classe) %>%
    summarise(Freq = n()) %>%
    mutate(Group = ifelse(Classe <100,"Proche","Loin"))

  tab <- distances %>%
    mutate(Group = ifelse(Dist < 95,"Proche","Loin")) %>%
    group_by(Group) %>%
    summarise(Nb = n(),
              Moy = mean(Dist))
  Xgraph = tab$Moy[which(tab$Group=="Loin")]
  Ygraph = max(classDist$Freq)
  Nbgraph = tab$Nb[which(tab$Group=="Loin")]
  Etiquette = paste("Nb placettes loin :", Nbgraph)

  g <- ggplot(classDist, aes(x=Classe,y=Freq, fill=Group)) +
    geom_bar(stat='identity') + theme_bw() +
    labs(x="Distance",y="Nombre de placettes",fill="") +
    geom_text(x=Xgraph, y=Ygraph, label=Etiquette, col='red')

  # ----------- Sauvegarde -----------
  if (enrg) {
    st_write(pts, nom, update=T, delete_layer = T)
  }
  out=list(parcelles, Plac, pts, distances, classDist, g)
  names(out) <- c("parcelles", "Plac", "pts", "distances", "classDist", "graph")
  return(out)
}

