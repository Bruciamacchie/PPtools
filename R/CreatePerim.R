#' Création d'un périmètre autour des points de placettes
#'
#' @description La fonction utilise en entrée un shape de type ponctuel contenant les placettes permanentes
#' d’une ou plusieurs forêts. Le fichier en entrée est un SpatialPointsDataFrame dont la table attributaire
#' contient au moins 2 champs. Le premier permet de distinguer les dispositifs, le second les placettes.
#' Les périmètres sont construits à l'aide de la fonction gConvexHull du package rgeos.
#'
#' @return La fonction sauvegarde un SpatialPolygonDataFrame.
#'
#' @param points = objet sf ou fichier POINT au format .shp
#' @param TailleBuffer = Taille du buffer en mètre (50m par défaut)
#' @param nom = nom souhaité pour le fichier en sortie.
#'
#' @import tcltk
#' @import tidyverse
#' @import sf
#'
#' @author Bruciamacchie Max
#'
#' @export

CreatePerim <- function(points=NULL, TailleBuffer=100, nom="PerimAut.shp") {
  pts <- Find_Verif_point(points)

  # if (is.null(points)) {
  #   file <- tk_choose.files(caption = "Choix du points",
  #                           filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  #   if (length(file) > 0) {
  #     pts <- st_read(file)
  #   } else { stop("Import annulé") }
  # } else {
  #   pts <- points
  # }
  # ------- Création du périmètre
  perim <- st_convex_hull(st_union(pts)) %>%
    st_buffer(dist=TailleBuffer)
  # ------- Sauvegarde
  st_write(perim, nom, quiet = TRUE, update=TRUE, delete_layer = TRUE)

  out <- list(perim, pts)
  names(out) <- c("perim", "pts")
  return(out)
}
