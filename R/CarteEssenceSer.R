#' Cartographie du volume d'une essence par sylvoécorégion.
#'
#' @description La fonction renvoie une carte de la France découpée en sylvoécorégion. La couleur des sylvoécorégions
#' est proportionnelle au volume moyen à l'hectare de l'essence retenue (rouge d'autant plus intense que
#' le volume à l'hectare est élevé). Les points bleus permettent de localiser les placettes IFN.
#' Leur densité permet d'estimer la précision du volume.
#'
#' @return La fonction renvoie une carte des sylvoécorégions avec un niveau de rouge proportionnel
#' au volume de l'essence retenue. Les sylvoécorégions qui ne contiennent pas l'essence retenue sont en gris foncé.
#' Les placettes IFN utilisées sont en bleu.
#'
#' @param Choix = Code essence de l'IFN. Par défaut ce code est égal à "09". Il correspond au hêtre.
#'
#' @import tidyverse
#' @import sf
#' @import ggthemes
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(tidyverse)
#' library(PPtools)
#' library(DataForet)
#' library(sf)
#' library(ggthemes)
#' #############
#' CarteEssenceSer("09")
#'
#' @export
#'
# ----------- Selection essence
CarteEssenceSer <- function(Choix="09") {
  data(IFNarbres)
  data(IFNplacettes)
  data(ser)

  t1 <- IFNarbres %>%
    filter(espar==Choix) %>%
    group_by(idp) %>%
    summarise(Vol = sum(v*w))

  t3 <- IFNplacettes %>%
    left_join(t1, by="idp") %>%
    filter(!is.na(Vol))

  t2 <- t1 %>%
    left_join(IFNplacettes[,c("idp","ser")], by="idp") %>%
    mutate(Vol = replace(Vol, which(is.na(Vol)), 0)) %>%
    group_by(ser) %>%
    summarise(Vol = mean(Vol)) %>%
    rename(codeser = ser)

  ser.f <- ser %>%
    left_join(t2, by = "codeser")

  g <- ggplot() +
    geom_sf(data=ser.f, aes(fill = Vol), colour = "gray50", size = 0.2) +
    scale_fill_gradient(low = "white", high = "darkred") +
    theme_map() + theme(legend.position="right") +
    geom_point(data=t3, aes(x=xl93, y=yl93), size=0.5, alpha=0.2, color="blue")

  return(g)
}
