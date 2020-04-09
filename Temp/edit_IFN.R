# Installation/Activation des packages nécessaires
library(easypackages)
# suppressMessages(
packages(
  "rlang","rmarkdown","knitr","tools","here",
  "tidyverse","openxlsx","xtable","ggrepel","ggthemes",
  "gWidgets2", "gWidgets2tcltk",  
   "scales","grid","gridExtra"
)


##### fonction pour éditer document guide du package PPtools #####
edit_guide_doc <- function(template = "inst/IFNinfos.Rnw") {
  ##### 1/ Initialisation #####
  rep = dirname(here::here("IFNinfos.rnw"))
  db = new.env()     # création d'un nouvel environnement
  db = global_env()  # rlang package
  year <- as.numeric(format(Sys.time(), "%Y")) # année en cours
  
  # sanity checks (permet de s'assurer qu'un objet/package? existe bien)
  # indb <- ensures(all(. %in% names(db)))
  # indb(c(load(arch1), load(arch2)))
  
  ##### 2/ Préparation des données  (voir remarque l.38) #####
  with(db, {
    
    # -- Choix du fichier
    file <- tk_choose.files(caption = "Choix du périmètre",
                            filters=matrix(c(".shp",".shp"),1,2, byrow = T))
    if (length(file) > 0) {
      perim <- st_read(file, quiet=T)
    } else { stop("Import annulé") }
    
    # -- création du dossier de sortie
    output_dir <- paste(dirname(file),"out",sep="/")
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    
    # -- définition des arguments nécessaires au knit
    repPdf     <- file.path(output_dir)
    repLogos   <- file.path(rep, "inst/Images/")
    repFigures <- file.path(rep, "inst/figures/")
    
    # -- superassignements
    # nom de la sortie en .tex
    output_filename <- paste0("IFNinfos_", year, ".tex")
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
