# ==============================================================================
# MÉMOIRE - Reconnexion des patches de pâturage
# Hungarian Grey Cattle
#
# SCRIPT 01b
#
# TRAITEMENT DES PARCELLES TEMPORAIREMENT EN JACHÈRE
#
# Objectif :
#
# Les parcelles dont crop_name est :
#
#   - GRÜNBRACHE
#   - GRÜNLANDBRACHE
#
# reçoivent temporairement :
#
#   1. le crop_name majoritaire parmi les parcelles voisines
#   2. un gross_marg correspondant à la moyenne des gross margins
#      des parcelles voisines appartenant à cette culture majoritaire
#
# Les données originales ne sont pas modifiées.
#
# Un nouvel objet est créé :
#
#   parcelles_invekos_modele
#
# Cet objet devra être utilisé dans le script 02.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. VÉRIFICATION DES DONNÉES NÉCESSAIRES
# ------------------------------------------------------------------------------

# Le script 01 doit avoir été exécuté avant celui-ci.

if (!exists("parcelles_invekos")) {
  
  stop(
    "ERREUR : l'objet 'parcelles_invekos' n'existe pas.\n",
    "Exécute d'abord le script 01."
  )
  
}


# ------------------------------------------------------------------------------
# 2. CHARGEMENT DES LIBRAIRIES
# ------------------------------------------------------------------------------

library(sf)
library(dplyr)


# ------------------------------------------------------------------------------
# 3. CRÉATION D'UNE COPIE POUR LE MODÈLE
# ------------------------------------------------------------------------------
#
# IMPORTANT :
#
# Nous ne modifions pas directement :
#
# parcelles_invekos
#
# Nous créons une copie spécifique au modèle.

parcelles_invekos_modele <- parcelles_invekos


# ------------------------------------------------------------------------------
# 4. IDENTIFICATION DES PARCELLES EN JACHÈRE
# ------------------------------------------------------------------------------

types_jachere <- c(
  "GRÜNBRACHE",
  "GRÜNLANDBRACHE"
)


parcelles_jachere <- parcelles_invekos_modele %>%
  filter(
    crop_name %in% types_jachere
  )


cat("\n")
cat("====================================================\n")
cat("TRAITEMENT DES PARCELLES EN JACHÈRE\n")
cat("====================================================\n")

cat(
  "Nombre total de parcelles INVEKOS :",
  nrow(parcelles_invekos_modele),
  "\n"
)

cat(
  "Nombre de parcelles GRÜNBRACHE ou GRÜNLANDBRACHE :",
  nrow(parcelles_jachere),
  "\n\n"
)


# Vérification

if (nrow(parcelles_jachere) == 0) {
  
  warning(
    "Aucune parcelle GRÜNBRACHE ou GRÜNLANDBRACHE n'a été trouvée."
  )
  
}


# ------------------------------------------------------------------------------
# 5. CRÉATION DES COLONNES DE SUIVI
# ------------------------------------------------------------------------------
#
# Ces colonnes permettent de conserver une trace du traitement.

parcelles_invekos_modele$crop_name_original <-
  parcelles_invekos_modele$crop_name

parcelles_invekos_modele$gross_marg_original <-
  parcelles_invekos_modele$gross_marg


# Colonne indiquant si la parcelle a été modifiée.

parcelles_invekos_modele$jachere_traitee <- FALSE


# ------------------------------------------------------------------------------
# 6. IDENTIFICATION DES PARCELLES À TRAITER
# ------------------------------------------------------------------------------

indices_jachere <- which(
  parcelles_invekos_modele$crop_name %in% types_jachere
)


# ------------------------------------------------------------------------------
# 7. TRAITEMENT DES PARCELLES EN JACHÈRE
# ------------------------------------------------------------------------------

for (i in indices_jachere) {
  
  
  # --------------------------------------------------------------------------
  # 7.1 Parcelle actuellement traitée
  # --------------------------------------------------------------------------
  
  parcelle_cible <- parcelles_invekos_modele[i, ]
  
  
  # --------------------------------------------------------------------------
  # 7.2 Identification des parcelles voisines
  # --------------------------------------------------------------------------
  #
  # st_touches permet d'identifier les polygones qui partagent une frontière.
  
  voisins <- st_touches(
    parcelle_cible,
    parcelles_invekos_modele
  )[[1]]
  
  
  # --------------------------------------------------------------------------
  # 7.3 Si aucun voisin n'est trouvé
  # --------------------------------------------------------------------------
  
  if (length(voisins) == 0) {
    
    next
    
  }
  
  
  # Extraction des parcelles voisines
  
  parcelles_voisines <- parcelles_invekos_modele[voisins, ]
  
  
  # --------------------------------------------------------------------------
  # 7.4 Sélection des voisins utilisables
  # --------------------------------------------------------------------------
  #
  # On exclut :
  #
  # - GRÜNBRACHE
  # - GRÜNLANDBRACHE
  # - les gross margins NA
  # - les gross margins <= 0
  
  voisins_valides <- parcelles_voisines %>%
    filter(
      !crop_name %in% types_jachere,
      !is.na(crop_name),
      !is.na(gross_marg),
      gross_marg > 0
    )
  
  
  # --------------------------------------------------------------------------
  # 7.5 Si aucun voisin valide n'est disponible
  # --------------------------------------------------------------------------
  
  if (nrow(voisins_valides) == 0) {
    
    next
    
  }
  
  
  # --------------------------------------------------------------------------
  # 7.6 Comptage des cultures présentes chez les voisins
  # --------------------------------------------------------------------------
  
  cultures_voisines <- voisins_valides %>%
    st_drop_geometry() %>%
    count(
      crop_name,
      name = "nombre"
    ) %>%
    arrange(
      desc(nombre)
    )
  
  
  # Culture la plus représentée
  
  culture_majoritaire <- cultures_voisines$crop_name[1]
  
  
  # --------------------------------------------------------------------------
  # 7.7 Sélection des voisins appartenant à cette culture
  # --------------------------------------------------------------------------
  
  voisins_culture_majoritaire <- voisins_valides %>%
    filter(
      crop_name == culture_majoritaire
    )
  
  
  # --------------------------------------------------------------------------
  # 7.8 Calcul du gross margin
  # --------------------------------------------------------------------------
  #
  # On utilise la moyenne des gross margins des parcelles voisines
  # appartenant à la culture majoritaire.
  
  nouveau_gross_marg <- mean(
    voisins_culture_majoritaire$gross_marg,
    na.rm = TRUE
  )
  
  
  # --------------------------------------------------------------------------
  # 7.9 Modification de la parcelle
  # --------------------------------------------------------------------------
  
  parcelles_invekos_modele$crop_name[i] <-
    culture_majoritaire
  
  
  parcelles_invekos_modele$gross_marg[i] <-
    nouveau_gross_marg
  
  
  parcelles_invekos_modele$jachere_traitee[i] <-
    TRUE
  
}


# ------------------------------------------------------------------------------
# 8. RÉSULTATS DU TRAITEMENT
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("RÉSULTATS DU TRAITEMENT\n")
cat("====================================================\n")


nombre_jacheres_initial <- sum(
  parcelles_invekos$crop_name %in% types_jachere
)


nombre_jacheres_traitees <- sum(
  parcelles_invekos_modele$jachere_traitee
)


nombre_jacheres_non_traitees <-
  nombre_jacheres_initial -
  nombre_jacheres_traitees


cat(
  "Nombre initial de parcelles en jachère :",
  nombre_jacheres_initial,
  "\n"
)


cat(
  "Nombre de parcelles traitées :",
  nombre_jacheres_traitees,
  "\n"
)


cat(
  "Nombre de parcelles non traitées :",
  nombre_jacheres_non_traitees,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 9. VÉRIFICATION DES PARCELLES RESTANTES
# ------------------------------------------------------------------------------

jachere_restantes <- parcelles_invekos_modele %>%
  filter(
    crop_name %in% types_jachere
  )


cat(
  "Nombre de parcelles toujours identifiées comme jachère :",
  nrow(jachere_restantes),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 10. RÉSUMÉ DES MODIFICATIONS
# ------------------------------------------------------------------------------

resume_jacheres <- parcelles_invekos_modele %>%
  filter(jachere_traitee) %>%
  st_drop_geometry() %>%
  select(
    fid,
    crop_name_original,
    crop_name,
    gross_marg_original,
    gross_marg
  )


cat(
  "Aperçu des premières parcelles modifiées :\n\n"
)


print(
  head(resume_jacheres)
)


# ------------------------------------------------------------------------------
# 11. CONTRÔLE FINAL
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("CONTRÔLE FINAL\n")
cat("====================================================\n")


cat(
  "Gross margin NA dans l'objet modèle :",
  sum(is.na(parcelles_invekos_modele$gross_marg)),
  "\n"
)


cat(
  "Gross margin <= 0 dans l'objet modèle :",
  sum(
    !is.na(parcelles_invekos_modele$gross_marg) &
      parcelles_invekos_modele$gross_marg <= 0
  ),
  "\n"
)


cat("\n")
cat("====================================================\n")
cat("SCRIPT 01b TERMINÉ\n")
cat("====================================================\n")

cat(
  "Nouvel objet créé : parcelles_invekos_modele\n"
)

cat(
  "Cet objet doit maintenant être utilisé dans le script 02.\n\n"
)




