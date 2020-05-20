#'Tarifs de cubage Chaudé
#'
#' @description Calcul du volume d'un arbre ou d'une grume à l'aide d'un tarif Chaudé.
#'
#' @return La fonction renvoie le volume calculé avec un tarif Chaudé à deux entrées. Le résultat est exprimé en m3.
#'
#' @param num = numéro de tarif. Ce numéro permet d'établir le lien avec la décroissance par classe de diamètre.
#' @param diam = diamètre(s) à 1,3 m (en cm).
#' @param haut = hauteur(s) découpe (en m);
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' Hauts <- c(13,14,14)
#' Diams = c(45,45,50)
#' TarifChaude(13, Diams, Hauts)
#' }
#' @export

# ------ Chaudé -----------------
# Changer le format du fichier decChaude
decChaude2 <- decChaude %>% 
  pivot_longer(-Classe,names_prefix = "Ch", names_to = "NumTarifCh", values_to = "deltaChaude") %>% 
  dplyr::rename(ClassTarifChaude = Classe)

#Changement de la fonction
TarifChaude2 <- function(dataframe, num=13, diam="Diametre", haut="Hauteur") {
  
  if ((length(dataframe %>% pull(diam))+length(dataframe %>% pull(haut))) ==2*length(dataframe %>% pull(diam))) {
    if(class(num)=="numeric"& class(dataframe %>% pull(diam))=="numeric"& class(dataframe %>% pull(haut))=="numeric") {
      
      df2 <- decChaude2 %>% 
        dplyr::filter(NumTarifCh == num)
      
      data2 <- dataframe %>% 
        dplyr::mutate(ClassTarifChaude = floor(get(diam)/5+0.5)*5) %>%
        left_join(df2, by = "ClassTarifChaude") %>% 
        mutate(VolChaude = pi/40000*(get(diam)-deltaChaude*(get(haut)/2-1.3))^2*get(haut))

    } else {print("les arguments doivent tous être numeric.")}
  } else {print("Les arguments doivent avoir même longueur.")}
}
