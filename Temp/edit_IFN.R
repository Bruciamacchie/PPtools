# Installation/Activation des packages nécessaires
library(easypackages)
packages(
  "rlang","rmarkdown","knitr","tools","here",
  "tidyverse","sf","openxlsx","xtable","ggrepel","ggthemes",
  "gWidgets2", "gWidgets2tcltk",
   "scales","grid","gridExtra"
)

edit_IFNinfos <- function(template = "inst/IFNinfos.Rnw") {
  ##### 1/ Initialisation #####
  rep = dirname(here::here("IFNinfos.rnw"))
  db = new.env()     # création d'un nouvel environnement
  # db = global_env()  # rlang package - debug uniquement
  year <- as.numeric(format(Sys.time(), "%Y")) # année en cours

  # sanity checks (permet de s'assurer qu'un objet/package? existe bien)
  # indb <- ensures(all(. %in% names(db)))
  # indb(c(load(arch1), load(arch2)))
  
  ##### 2/ Préparation des données #####
  with(db, {
    
    # -- Choix du fichier
    file <- tk_choose.files(
      caption = "Choix du périmètre",
      filters = matrix(c(".shp",".shp"), 1, 2, byrow = T)
    )
    if (length(file) > 0) {
      perim <- st_read(file, quiet=T)
    } else { stop("Aucun périmètre sélectionné - Import annulé") }
    
    # -- création du dossier de sortie
    output_dir <- paste(dirname(file),"out",sep="/")
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # -- définition des arguments nécessaires au knit
    repPdf     <- file.path(output_dir)
    repFigures <- file.path(output_dir, "figures/")
    repLogos   <- file.path(rep, "inst/Images/")
    
    # -- superassignements
    # nom de la sortie en .tex
    output_filename <- paste0("IFNinfos_", year, ".tex")
    output <<- file.path(repPdf, output_filename)

  })
  
  ##### 3/ Edition du document guide #####
  # Attention modifs dans "Preambule.Rnw" et dans "PPtools.Rnw", chunk "TarifErreurSch"
  knit2pdf(
    input = template,
    output = output,
    compiler = "pdflatex",
    quiet = TRUE,
    envir = db
  )
  
  # -- message de fin
  end_msg <- "Edition du document guide termin\u00E9e"
  msg <- tk_messageBox(
    type = "ok", 
    message = end_msg, 
    icon = "info"
  )
}

edit_IFNinfos()
