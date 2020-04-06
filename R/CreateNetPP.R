#' Création d'un réseau de placettes permanentes
#'
#' @description La fonction construit un réseau de placettes permanentes en fonction
#' du périmètre de la zone d'étude, du nombre de placettes souhaité et de la forme du réseau.
#'
#' @return La fonction construit le fichier géoréférencé des placettes dans le même dossier
#' que le ficheir en entrée avec "MailleAut" comme nom par défaut.
#'
#' @param perim = objet au format sf. Si NULL ouverture d'une boîte de dialogue permettant de sélectionner
#' un fichier shp contenant des POLYGON.
#' @param Nb = nombre souhaité de placettes
#' @param Cellsize = pas de la maille en mètre
#' @param Type = type de maille souhaitée. Les options possibles sont : random, regular
#' (option par défaut), stratified, nonaligned, hexagonal
#' @param enrg = Si TRUE (valeur par défaut) la fonction enregistre la grille de points.
#' @param nom = nom souhaité pour le fichier ("MailleAut" par défaut)
#'
#' @import sf
#' @import tcltk
#' @import sp
#'
#' @author Bruciamacchie Max
#' @export
#'
CreateNetPP <- function(perim=NULL,Nb=30, Cellsize=NULL, Type="regular", enrg=T, nom="MailleAut.shp") {
  perimetre <- Find_Verif_poly(perim)

  if (sum(!class(perimetre) %in% c("sf")) == 0) return(print("Uniquement objet de la classe"))
  if (is.null(Cellsize)) {
    Plac <- spsample(as(perimetre,"Spatial"), n=Nb, Type) %>%
      st_as_sf()
  } else {
    Plac <- spsample(as(perimetre,"Spatial"), n=Nb, cellsize=Cellsize, Type) %>%
      st_as_sf()
  }
  Plac <- Plac %>% mutate(Num=1:dim(.)[1]) %>%
    dplyr::select(Num, geometry)

  # ---------- Sauvegarde --------
  if (enrg) {
    st_write(Plac, nom, update=T, delete_layer = T)
  }

  out=list(perimetre, Plac)
  names(out) <- c("perimetre", "Plac")
  return(out)
}
