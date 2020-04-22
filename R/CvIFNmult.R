#' Coefficients de variation pour des groupes
#'
#' @description La fonction renvoie des coefficients de variation sur la surface terrière
#' en utilisant la base de données arbres de l'IFN. Par rapport à la fonction CvIFN,
#' elle permet de raisonner par secteur, groupe de parcelle, regroupement de polygone, etc.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu (forêts, massifs,
#' etc.). Une boîte de dialogue permet de choisir le champ de regroupement.
#'
#' @return La fonction renvoie un tableau par groupe des coefficients de variation
#' sur la surface terrière
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
#' @name CvIFNmult
#' @title CvIFNmult
#'
#' @examples
#' library(tidyverse)
#' library(sf)
#' library(DataForet)
#' library(PPtools)
#' #
#' data(FD)
#' res <- CvIFNmult(FD)
#' res$tab
#' res$Nbres
#'
#' @export

CvIFNmult <- function(perim = NULL, rep = NULL, TailleBuffer=NULL, enreg=F) {
  perimetre <- Find_Verif_poly(perim)

  Noms <- names(perimetre)
  ChoixVar <- tk_select.list(Noms, title = "Choisir la variable qui permet de creer des sous-ensembles")
  PosVar <- which(Noms==ChoixVar)
  perimetre <- perimetre %>% mutate(key = .[[PosVar]])
  modalites <- unique(perimetre$key)

  placettes <- ExtractPlac(perimetre)
  t1 <- ExtractArbres(placettes)
  t1$Foret <- "Totalité"
  tab <- t1
  Nbres <- data.frame(Population = "Totalité",
                      Nbre = dim(placettes)[1])

  if (length(modalites) >1) {
    TailleBuffer=NULL
    for (i in 1:length(modalites)) {
      foret=modalites[i]
      EnTour <- perimetre %>% filter(key==foret)
      placettes <- ExtractPlac(EnTour)
      t1 <- ExtractArbres(placettes)
      t1$Foret <- foret
      tab <- rbind(tab, t1)
      Nbre <- data.frame(Population = foret,
                         Nbre = dim(placettes)[1])
      Nbres <- rbind(Nbres, Nbre)
      print(paste("Traitement de la forêt", foret, "terminé"))
    }
  }

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
  out <- list(tab, Nbres)
  names(out) <- c("tab","Nbres")
  return(out)

}
