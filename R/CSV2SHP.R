#' Conversion CSV to SHP
#'
#' @description La fonction permet de convertir un fichier de points au format CSV
#' en fichier géoréférencé au format SHP
#'
#' @return La fonction renvoie un objet au format sf mais l'enregistre dans le format
#' indiqué dans l'extension de l'argument nom.
#' Le fichier est enregistré dans le répertoire en cours.
#'
#' @param epsg = code EPSG (système de projection)
#' @param nom = nom du fichier en sortie. Par défaut "Placettes.shp"
#'
#' @import tcltk
#' @import sf
#'
#' @author Bruciamacchie Max
#'
#' @export


CSV2SHP <- function(epsg=2154, nom="Placettes.shp") {
  file <- tk_choose.files(caption = "Choix du fichier ponctuel au format CSV",
                          filters=matrix(c(".csv",".csv"),1,2, byrow = T))
  if (length(file) > 0) {
    if (sum(c("X","Y") %in% names(tab)) == 2) {
      shp <- read_csv2(file) %>%
        st_as_sf(coords = c("X","Y"), crs=epsg, agr="constant")
      st_write(shp, nom, quiet = TRUE)
    }else {print("Le fichier csv doit contenir une variable X et une variable Y")}
  }else {print("Import du périmètre annulé")}
}
