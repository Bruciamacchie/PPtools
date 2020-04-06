################ Recherche et vérification de POLYGON ##############
Find_Verif_poly <- function(poly) {
  if (is.null(poly)) {
    file <- tk_choose.files(caption = "Choix du périmètre",
                            filters=matrix(c(".shp",".shp"),1,2, byrow = T))
    if (length(file) > 0) {
      perimetre <- st_read(file, quiet=T)
    } else { stop("Import annulé") }
  } else {
    perimetre <- poly
  }
  if (sum(st_geometry_type(perimetre) %in% c("POLYGON","MULTIPOLYGON"))==0) {
    stop("Le fichier en entrée doit être un POLYGON ou un MULTIPOLYGON")
  }
  return(perimetre)
}

################ Recherche et vérification de POINT ##############
Find_Verif_point <- function(points) {
  if (is.null(points)) {
    file <- tk_choose.files(caption = "Choix du points",
                            filters=matrix(c(".shp",".shp"),1,2, byrow = T))
    if (length(file) > 0) {
      pts <- st_read(file)
    } else { stop("Import annulé") }
  } else {
    pts <- points
  }
  if (sum(st_geometry_type(pts) %in% c("POINT","MULTIPOINT"))==0) {
    stop("Le fichier en entrée doit être un POINT ou un MULTIPOINT")
  }
  return(pts)
}





################ Extraction des placettes IFN au voisinage foret ##############
ExtractPlac <- function(shp, TailleBuffer=NULL) {
  shp <- shp %>%
    st_transform(2154) %>%
    st_union() %>%
    st_sf()

  Surf <- as.numeric(st_area(shp))
  if(is.null(TailleBuffer)) {
    a=pi
    b=2*pi*sqrt(Surf/pi)
    c=Surf-20000000
    delta = b^2-4*a*c
    TailleBuffer = max((-b+sqrt(delta))/2/a, 450)
  }
  zone <- shp %>%
    st_buffer(dist = TailleBuffer)

  placettes <- IFNplacettes %>%
    st_as_sf(coords = c("xl93", "yl93"), crs = 2154, remove=F, agr="constant") %>%
    st_intersection(zone) %>%
    group_by(xl93,yl93) %>%
    slice(1) %>% # on retire les doublons
    ungroup()
}

################ Extraction des arbres IFN au voisinage foret ##############
ExtractArbres <- function(spdf) {
  # ------------------- Arbres
  Gha <- IFNarbres %>%
    filter(idp %in% spdf$idp) %>%
    filter(!is.na(w)) %>%
    mutate(espar = as.character(espar)) %>%
    dplyr::select(idp, espar,c13,w) %>%
    mutate(Diam = round(c13/pi,0)) %>%
    mutate(Classe=floor(c13/pi/5+0.5)*5) %>%
    mutate(Gha = c13^2/pi/40000*w) %>%
    left_join(CodesEssIFN, by=c("espar"="code"))
  # ------------------- CV Total
  GhaPla <- Gha %>%
    group_by(idp) %>%
    summarise(Gtot = sum(Gha))
  n <- dim(spdf)[1]
  tab <- data.frame(Population = "Totale",
                    Gha = mean(GhaPla$Gtot),
                    Cv = sd(GhaPla$Gtot)/mean(GhaPla$Gtot))
  # ------------------- CV essences principales
  GhaEss <- Gha %>%
    group_by(idp, espar) %>%
    summarise(Gtot = sum(Gha)) %>%
    group_by(espar) %>%
    summarise(Moy = sum(Gtot)/n,
              Sd = ((n*sum(Gtot^2)-sum(Gtot)^2)/n/(n-1))^0.5,
              Cv = Sd/Moy)  %>%
    left_join(CodesEssIFN, by=c("espar"="code")) %>%
    arrange(desc(Moy))

  ListeEss <- GhaEss$espar

  GhaEss <- GhaEss %>%
    dplyr::select(libelle, Moy, Cv) %>%
    rename(Population = libelle,
           Gha = Moy)

  tab <- rbind(tab, GhaEss)
  # ------------------- CV categories diametre
  GhaPla <- Gha %>%
    mutate(Cat = cut(Classe, breaks=c(0, 17.5, 27.5, 47.5, 200),
                     labels=c("PER","PB","BM", "GB"))) %>%
    group_by(idp, Cat) %>%
    summarise(Gtot = sum(Gha)) %>%
    group_by(Cat) %>%
    summarise(Moy = sum(Gtot)/n,
              Sd = ((n*sum(Gtot^2)-sum(Gtot)^2)/n/(n-1))^0.5,
              Cv = Sd/Moy)  %>%
    dplyr::select(Cat, Moy, Cv) %>%
    rename(Population = Cat,
           Gha = Moy)
  tab <- rbind(tab, GhaPla)

  # ------------------- CV GB+BM 2 essences principales
  GhaPla <- Gha %>%
    mutate(Cat = cut(Classe, breaks=c(0, 17.5, 27.5, 47.5, 200),
                     labels=c("PER","PB","BM", "GB"))) %>%
    filter(espar %in% ListeEss[1:2] & (Cat=="GB"| Cat=="BM")) %>%
    group_by(idp, libelle) %>%
    summarise(Gtot = sum(Gha)) %>%
    group_by(libelle) %>%
    summarise(Moy = sum(Gtot)/n,
              Sd = ((n*sum(Gtot^2)-sum(Gtot)^2)/n/(n-1))^0.5,
              Cv = Sd/Moy) %>%
    mutate(Population = paste(libelle, "GB + BM")) %>%
    dplyr::select(Population, Moy, Cv) %>%
    rename(Gha = Moy)

  tab <- tab %>%
    rbind(GhaPla) %>%
    mutate(Gha = round(Gha, 2))
}
