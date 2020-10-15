#' Accroissement sur le diamètre
#'
#' @description La fonction renvoie les accroissements sur le diamètre par essence et classe de diamètre
#' en utilisant la base de données arbres de l'IFN.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif,
#' sylvoécorégion, etc.
#'
#' @return La fonction renvoie un tableau des accroissements sur le diamètre par essence et classe de diamètre.
#' Elle intègre les coefficients d'écorce, les accroissements sont donc sur écorce.
#'
#' @param perim = objet au format sf. Si NULL ouverture d'une boîte de dialogue permettant de sélectionner
#' un fichier shp contenant des POLYGON.
#' @param SeuilCircf = seuil minimal de circonférence en cm en dessous duquel l'arbre n'est pas retenu
#' pour le calcul du tarif. Par défaut ce seuil est fixé à 50 cm.
#' @param SeuilNb = seuil minimal du nombre d'arbres dans la base IFN pour qu'une essence soit retenue.
#' Par défaut ce seuil est fixé à 10 tiges.
#' @param UseSer = argument permettant de choisir si le calcul des numéros de tarif doit se faire
#' au sein d'une même sylvoécorégion. Par défaut, UseSer=TRUE.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .csv.
#' Par défaut enreg=FALSE.
#'
#' @import tidyverse
#' @import sf
#' @import tools
#' @import tcltk
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' data("FD")
#' perim = FD %>% filter(IIDTN_FRT == "F10451Y")
#' acct <- IFNacctD(perim)
#' # résultats
#' acct$tab
#' acct$Effectif
#' acct$Graph
#'
#' @export

IFNacctD <- function(perim=NULL, SeuilCircf=50, SeuilNb=10, UseSer=T, enreg=F) {
    # ------------ Gestion du perimetre ----------------------------------------------
  perimetre <- Find_Verif_poly(perim)

  # ------------ Extraction placettes ------------------
  placettes <- ExtractPlac(perimetre) %>%
    dplyr::select(idp:yl93)
  if (UseSer) {
    placettes <- placettes %>% st_intersection(DataForet::ser)
  }

    AcctDs <- IFNarbres %>%
      dplyr::filter(idp %in% placettes$idp) %>%
      dplyr::filter(!is.na(ir5) & c13 >=SeuilCircf) %>%
      dplyr::mutate(mortb = as.numeric(as.character(mortb))) %>%
      dplyr::filter(veget=="0" & mortb<=1 & acci==0) %>%
      dplyr::select(espar,c13,ir5) %>%
      dplyr::mutate(Diam = round(c13/pi,0)) %>%
      dplyr::mutate(Classe=floor(c13/pi/5+0.5)*5) %>%
      dplyr::mutate(espar = as.character(espar)) %>%
      dplyr::left_join(CodesEssIFN, by=c("espar"="code")) %>%
      dplyr::left_join(Ecorces[,c(1,4)], by=c("espar"="codeIFN")) %>%
      dplyr::mutate(AcctD = round(ir5*2/50 /(1-2*pi*b/1000),3))

    df <- AcctDs %>%
      dplyr::group_by(espar) %>%
      dplyr::summarise(Freq = n()) %>%
      filter(Freq >= SeuilNb) %>%
      dplyr::rename(code = espar) %>%
      dplyr::left_join(CodesEssIFN, by = "code") %>%
      dplyr::arrange(desc(Freq)) %>%
      dplyr::select(code,libelle,Freq)

    t2 <- AcctDs %>%
      dplyr::filter(espar %in% df$code) %>% 
      dplyr::filter(!is.na(AcctD))
    

    local=2
    p <- ggplot(t2, aes(x=Diam, y=AcctD)) +
      geom_point() + geom_smooth(span=local, method = 'loess') +
      facet_wrap(~ libelle, ncol=3) +
      theme_bw() +
      coord_cartesian(xlim = c(10, 90))

    ListeEss <- unique(t2[,c("espar","libelle")])
    Xnew <- data.frame(Diam = seq(20,100,5))

    tab <- Xnew
    for(i in 1:dim(ListeEss)[1]) {
      t3 <- t2 %>% dplyr::filter(espar == ListeEss[i, 1] %>% pull() )
      model <- loess(AcctD ~ Diam, data = t3, span=local)
      res <- data.frame(round(predict(model, newdata = Xnew),2))
      names(res) <- ListeEss[i,2]
      tab <- cbind(tab, res)
    }

    if (enreg){
      dir.create("OutIFN", showWarnings = F)
      repOut <- paste(getwd(), "OutIFN", sep="/")
      fichOut <- paste("OutIFN","AcctDifn.csv", sep="/")
      write.csv(tab, fichOut, row.names =F)
      print(paste("Les accroissements sur le diamètre ont été enregistrés dans le répertoire : ", repOut))
    }

    # if (enreg) {
    #   wb <- createWorkbook()
    #   addWorksheet(wb, "data")
    #   writeData(wb, "data", tab)
    #   setwd(dirname(dirname(dirname(perim))))
    #   dir.create("Out", showWarnings = F)
    #   saveWorkbook(wb, "Out/AccD.xlsx", overwrite = T)
    #   print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
    # }
    out <- list(tab, df, p, file)
    names(out) <- c("tab", "Effectif", "Graph", "file")
    return(out)
}
