#' Coefficients de variation
#'
#' @description La fonction renvoie des coefficients de variation sur la surface terrière
#' en utilisant la base de données arbres de l'IFN.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif,
#' sylvoécorégion, etc.
#'
#'
#' @return La fonction renvoie un tableau des coefficients de variation sur la surface terrière
#' \itemize{
##'  \item des essences contribuant à plus de 10\% de la surface terrière
##'  \item des PER, PB, BM et GB
##'  \item des gros bois de l'essence contribuant le plus à la surface terrière
##' }
#'
#' @param perim = objet au format sf. Si NULL ouverture d'une boîte de dialogue permettant de sélectionner
#' un fichier shp contenant des POLYGON.
#' @param TailleBuffer = Taille du buffer en mètre. Elle est calculée de manière à ce que la zone de
#' recherche des placettes IFN soit au moins égale à 2000 ha. Elle est au minimum égale à 450 m.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .xlsx.
#' Par défaut enreg=FALSE.
#'
#' @import tidyverse
#' @import tools
#' @import tcltk
#' @import openxlsx
#' @import DataForet
#'
#' @author Bruciamacchie Max
#'
#' @name CvIFN
#' @title CvIFN
#' @examples
#' library(tidyverse)
#' library(sf)
#' library(DataForet)
#' library(PPtools)
#' #
#' data(FD)
#' perim = FD %>% filter(IIDTN_FRT =="F09405S")
#' res <- CvIFN(perim)
#' plot(st_geometry(res$perimetre))
#' plot(st_geometry(res$placettes), col='red', add=T)
#'
#' @export

CvIFN <- function(perim = NULL, rep = NULL, TailleBuffer=NULL, enreg=F) {
  perimetre <- Find_Verif_poly(perim)

  placettes <- ExtractPlac(perimetre)
  n <- dim(placettes)[1]

  tab <- ExtractArbres(placettes)

  if (enreg) {
    wb <- createWorkbook()
    addWorksheet(wb, "data")
    writeData(wb, "data", tab)
    setwd(dirname(dirname(dirname(perim))))
    dir.create("Out", showWarnings = F)
    saveWorkbook(wb, "Out/CV.xlsx", overwrite = T)
    print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
  }
  tab$Cv <- round(tab$Cv,2)
  out <- list(tab, n, perimetre, placettes)
  names(out) <- c("tab","Nb","perimetre","placettes")
  return(out)
}
