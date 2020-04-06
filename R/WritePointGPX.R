#' Ecriture au format GPX de données pontuelles
#'
#' @description La fonction sauvegarde au format GPX des données ponctuelles en permettant de choisir
#' dans la table attributaire, par le biais d'une boîte de dialogue, le champ qui sera utilisé
#' pour nommer les points.
#'
#' @return La fonction enregistre dans le même répertoire que le fichier utlisé en entrée.
#'
#' @param points = objet sf ou fichier POINT au format .shp
#' @param nom = nom du fichier en sortie. Par défaut nom="gpxfile.gpx".
#'
#' @import sf
#' @import tcltk
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' WritePointGPX()
#'
#' @export

WritePointGPX <- function(points=NULL, nom="gpxfile.gpx") {
  pts <- Find_Verif_point(points) %>%
    st_transform(4326)

  rep=getwd()

  Noms <- names(pts)[-length(names(pts))]
  Choix <- tk_select.list(Noms, title = "Choisir une variable en tant qu'identifiant")
  if (str_length(Choix) > 0) {
    pts <- pts %>%
      dplyr::select(which(Noms==Choix))
    # Le format n'admet comme nom de champs que "name" pour le nom, "ele" pour elevation
    # et "time" pour l'information sur le temps
    names(pts)[1] <- "name"

    st_write(pts, dsn=nom, layer="waypoints", driver="GPX", update=T, delete_dsn=T, quiet =T)
    print(paste("Fichier imprimé sous le nom",nom,"dans le répertoire",rep))
  } else {print("Conversion annulée")}
}
