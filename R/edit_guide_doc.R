# Installation/Activation des packages nécessaires
library(easypackages)
# suppressMessages(
packages(
  "stringr", "openxlsx", "rmarkdown", "tools",
  "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
  "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
  "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang"
)


##### fonction pour éditer document guide du package PPtools #####
edit_guide_doc <- function(
  template = "Documentation/PPtools.Rnw"
) {
  ##### 1/ Initialisation #####
  # création d'un nouvel environnement
  db = new.env()
  db = global_env()
  
  # sanity checks (permet de s'assurer qu'un objet/package? existe bien)
  # indb <- ensures(all(. %in% names(db)))
  # indb(c(load(arch1), load(arch2)))
  
  ##### / \ #####
  
  ##### 2/ Préparation des données  (voir remarque l.38) #####
  with(db, {
    # -- gestion des dossiers
    rep <- tk_choose.dir(
      caption = "Choix du répertoire de sauvegarde pour le document guide 'PPtools.pdf' (pour l'instant fixé sur le dossier PPtools !)"
    )
    
    # -- année en cours
    year <- as.numeric(format(Sys.time(), "%Y"))
    
    # -- création du dossier de sortie
    output_dir <- dirname(template) # dossier output_dir = Documentation pour l'instant
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # -- définition des arguments nécessaires au knit
    repPdf <- file.path(rep, output_dir)
    repLogos <- file.path(repPdf, "Images/")
    repFigures <- file.path(repPdf, "figures/")
    
    # -- superassignements
    # nom de la sortie en .tex
    output_filename <- paste0("PPtools_guide_", year, ".tex")
    output <<- file.path(repPdf, output_filename)
    
    # Possibilité de faire les calculs en amont de l'édition du pdf/Rmd -> moins lourd ? à voir avec profile
    # # -- building tables needed for edition # TODO : pour RNF, insérer l'interface pour analyse par groupes
    # build_tables(
    #   repAFI = repAFI, repSav = repSav, 
    #   lang = lang, disp = disp, continue
    # )
  })
  ##### /\ #####
  
  ##### 3/ Edition du document guide #####
  # Attention modifs dans "Preambule.Rnw" et dans "PPtools.Rnw", chunk "TarifErreurSch"
  knit2pdf(
    input = template,
    output = output,
    compiler = "pdflatex",
    quiet = TRUE,
    envir = db
  )
  ##### / \ #####
  
  # -- message de fin
  end_msg <- "Edition du document guide termin\u00E9e"
  msg <- tk_messageBox(
    type = "ok", 
    message = end_msg, 
    icon = "info"
  )
}
##### /\ #####

edit_guide_doc() # debug
