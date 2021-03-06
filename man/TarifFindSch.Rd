% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TarifFindSch.R
\name{TarifFindSch}
\alias{TarifFindSch}
\title{Recherche des tarifs de cubage Schaeffer}
\usage{
TarifFindSch(df = NULL, enreg = F)
}
\arguments{
\item{df}{= tableau contenant au moins 3 colonnes nommées Essence, Vol et Diam.}

\item{enreg}{= argument permettant de choisir l'enregistrement ou pas du tableau au format .xlsx.
Par défaut enreg=FALSE.}
}
\value{
La fonction renvoie un tableau qui fournit par essence le numéro de tarif Schaeffer rapide, intermédiaire,
lent ou très lent ainsi que les coeffoicients de variations associés, ainsi qu'un graphique permettant d’apprécier
la variabilité des numéros.
}
\description{
La fonction permet de trouver parmi les tarifs Schaeffer, ceux qui sont le plus adaptés au cubage d'un échantillon
d'arbres. La fonction nécessite en entrée un tableau contenant au moins 3 colonnes dénommées Essence, Diam et Vol
}
\examples{
\donttest{
####### Utilisation
library(tcltk)
library(readxl)
library(openxlsx)
library(Hmisc) # pour les moyennes et variances pondérées

data(Vol)
res <- TarifFindSch(Vol)
####### Analyse des résultats
res$tab
res$graph
}

}
\author{
Bruciamacchie Max
}
