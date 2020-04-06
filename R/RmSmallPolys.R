#' Simplification du domaine d'étude - suppression des trous au sein des polygones
#'
#' @description La fonction RmSmallPolys() utilise en entrée un shape de type surfacique.\cr
#' Elle permet de supprimer des polygones de surface inférieure à un seuil choisi par l'opérateur.\cr
#' Elle n'a de sens qu'après l'utilisation de la fonction UnionPolyDeleteHole()
#'
#' @return La fonction renvoit un fichier au format .shp.
#'
#' @param perim = objet au format sf. Si NULL ouverture d'une boîte de dialogue permettant de sélectionner
#' un fichier shp contenant des POLYGON.
#' @param minarea = seuil minimal de surface (2500 m2 par défaut)
#' @param nom = nom souhaité pour le fichier en sortie ("PerimDef" par défaut).
#'
#' @import tidyverse
#' @import tcltk
#' @import sf
#' @import nngeo
#'
#' @author Bruciamacchie Max
#'
#' @export


RmSmallPolys <- function(perim=NULL,  nom="PerimDef.shp") {
  shp <- Find_Verif_poly(perim)

  shp1 <- shp %>%
    st_make_valid() %>%
    st_union %>%
    st_sf()

  shp_cor <- nngeo::st_remove_holes(shp1)

  # ------- Sauvegarde
  st_write(shp, nom, quiet = TRUE, update=TRUE, delete_layer = TRUE)

  out=list(shp,shp1,shp_cor)
  names(out) <- c("Avant","Union", "Après")

  return(out)
}
