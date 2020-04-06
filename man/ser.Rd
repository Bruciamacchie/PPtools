\name{ser}
\alias{ser}
\docType{data}
\title{Syvo-Eco-Région}
\description{Limites des Syvo-Eco-Région de France métropole avec une résolution de 50m}
\usage{data("ser")}
\format{'SpatialPolygonsDataFrame'
  ..@ data       :'data.frame':	473 obs. of  2 variables:

  .. ..$ codeser: Factor w/ 87 levels

  .. ..$ NomSER : Factor w/ 86 levels "Adour atlantique",..:

  ..@ polygons   :List of 473

  ..@ bbox       : num [1:2, 1:2] 99038 6046556 1242430 7110524

  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots

  .. .. ..@ projargs: chr "+init=epsg:2154 +proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0"| __truncated__
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ser)
plot(ser)
}
\keyword{datasets}
