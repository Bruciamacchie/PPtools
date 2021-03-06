% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CarteEssenceSer.R
\name{CarteEssenceSer}
\alias{CarteEssenceSer}
\title{Cartographie du volume d'une essence par sylvoécorégion.}
\usage{
CarteEssenceSer(Choix = "09")
}
\arguments{
\item{Choix}{= Code essence de l'IFN. Par défaut ce code est égal à "09". Il correspond au hêtre.}
}
\value{
La fonction renvoie une carte des sylvoécorégions avec un niveau de rouge proportionnel
au volume de l'essence retenue. Les sylvoécorégions qui ne contiennent pas l'essence retenue sont en gris foncé.
Les placettes IFN utilisées sont en bleu.
}
\description{
La fonction renvoie une carte de la France découpée en sylvoécorégion. La couleur des sylvoécorégions
est proportionnelle au volume moyen à l'hectare de l'essence retenue (rouge d'autant plus intense que
le volume à l'hectare est élevé). Les points bleus permettent de localiser les placettes IFN.
Leur densité permet d'estimer la précision du volume.
}
\examples{
library(tidyverse)
library(PPtools)
library(DataForet)
library(sf)
library(ggthemes)
#############
CarteEssenceSer("09")

}
\author{
Bruciamacchie Max
}
