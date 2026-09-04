# ==============================================================================
# MÉMOIRE - Reconnexion des patches de pâturage
# Hungarian Grey Cattle
# Parc national Neusiedler See-Seewinkel, Autriche
#
# ÉTAPE 4 / 5
# RAFFINEMENT DES CHEMINS SELON LES EXPLOITATIONS AGRICOLES
#
# OBJECTIF :
#
# Intégrer l'identifiant d'exploitation ID_farm_an dans l'analyse
# des chemins de moindre coût.
#
# PRINCIPE :
#
# Plus un chemin implique d'exploitations agricoles différentes,
# moins ce chemin est avantageux.
#
# Le modèle cherchera donc à limiter les transitions entre exploitations,
# puis le nombre d'ID_farm_an distincts sera calculé et comparé entre
# les chemins.
# ==============================================================================


# ==============================================================================
# 1. CHARGEMENT DES LIBRAIRIES
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(igraph)
library(Matrix)
library(ggplot2)


# ==============================================================================
# 2. CONFIGURATION GÉNÉRALE
# ==============================================================================

CRS_PROJET <- 31287


DOSSIER_DONNEES <- "~/Desktop/Data_POM"


DOSSIER_SORTIE_ETAPE4 <- file.path(
  
  DOSSIER_DONNEES,
  
  "Resultats_Etape4_Fermes"
)


if (!dir.exists(DOSSIER_SORTIE_ETAPE4)) {
  
  dir.create(
    
    DOSSIER_SORTIE_ETAPE4,
    
    recursive = TRUE
    
  )
  
}


# ==============================================================================
# 3. VERIFICATION DES OBJETS NECESSAIRES
# ==============================================================================

objets_necessaires <- c(
  
  "grille_cout",
  
  "chemins_finaux",
  
  "parcelles_invekos_modele",
  
  "paires_patches"
  
)


objets_manquants <- objets_necessaires[
  
  !objets_necessaires %in% ls()
  
]


if (length(objets_manquants) > 0) {
  
  stop(
    
    "ERREUR : les objets suivants sont absents de l'environnement R : ",
    
    paste(
      
      objets_manquants,
      
      collapse = ", "
      
    ),
    
    "\n\n",
    
    "Exécute d'abord les scripts précédents dans le même environnement R."
    
  )
  
}


# ==============================================================================
# 4. VERIFICATION DE LA COLONNE ID_FARM_AN
# ==============================================================================

if (!"ID_farm_an" %in% names(parcelles_invekos_modele)) {
  
  stop(
    
    "ERREUR : la colonne 'ID_farm_an' est absente de ",
    
    "parcelles_invekos_modele.\n\n",
    
    "Vérifie le nom EXACT de la colonne dans ta couche INVEKOS."
    
  )
  
}


cat("\n")
cat("====================================================\n")
cat("ETAPE 4 - CONTROLE DES IDENTIFIANTS D'EXPLOITATION\n")
cat("====================================================\n\n")


cat(
  
  "Nombre total de parcelles : ",
  
  nrow(parcelles_invekos_modele),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Nombre de parcelles avec ID_farm_an manquant : ",
  
  sum(
    
    is.na(parcelles_invekos_modele$ID_farm_an)
    
  ),
  
  "\n",
  
  sep = ""
  
)


nombre_fermes_total <- n_distinct(
  
  parcelles_invekos_modele$ID_farm_an,
  
  na.rm = TRUE
  
)


cat(
  
  "Nombre total d'exploitations différentes : ",
  
  nombre_fermes_total,
  
  "\n\n",
  
  sep = ""
  
)

# ==============================================================================
# 5. DIAGNOSTIC DES EXPLOITATIONS TRAVERSEES PAR LES CHEMINS ACTUELS
# ==============================================================================

cat("====================================================\n")
cat("DIAGNOSTIC DES FERMES TRAVERSEES PAR LES 13 CHEMINS\n")
cat("====================================================\n\n")


resultats_fermes_avant <- vector(
  
  "list",
  
  nrow(chemins_finaux)
  
)


for (i in seq_len(nrow(chemins_finaux))) {
  
  
  chemin_i <- chemins_finaux[i, ]
  
  
  id_a <- chemin_i$patch_a
  
  id_b <- chemin_i$patch_b
  
  
  parcelles_traversees <- st_intersects(
    
    chemin_i,
    
    parcelles_invekos_modele
    
  )[[1]]
  
  
  if (length(parcelles_traversees) == 0) {
    
    warning(
      
      "Aucune parcelle INVEKOS détectée pour le chemin ",
      
      id_a,
      
      " -> ",
      
      id_b
      
    )
    
    
    resultats_fermes_avant[[i]] <- data.frame(
      
      patch_a = id_a,
      
      patch_b = id_b,
      
      nombre_parcelles = 0,
      
      nombre_fermes_distinctes = 0,
      
      liste_fermes = NA_character_
      
    )
    
    
    next
    
  }
  
  
  ids_fermes <- parcelles_invekos_modele$ID_farm_an[
    
    parcelles_traversees
    
  ]
  
  
  ids_fermes <- ids_fermes[
    
    !is.na(ids_fermes)
    
  ]
  
  
  fermes_uniques <- unique(
    
    ids_fermes
    
  )
  
  
  resultats_fermes_avant[[i]] <- data.frame(
    
    patch_a = id_a,
    
    patch_b = id_b,
    
    nombre_parcelles = length(parcelles_traversees),
    
    nombre_fermes_distinctes = length(fermes_uniques),
    
    liste_fermes = paste(
      
      fermes_uniques,
      
      collapse = " | "
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
}


resultats_fermes_avant <- bind_rows(
  
  resultats_fermes_avant
  
)


cat(
  
  "RESULTATS AVANT RAFFINEMENT :\n\n"
  
)


print(
  
  resultats_fermes_avant
  
)

# ==============================================================================
# 6. SAUVEGARDE DU DIAGNOSTIC AVANT RAFFINEMENT
# ==============================================================================

fichier_diagnostic_avant <- file.path(
  
  DOSSIER_SORTIE_ETAPE4,
  
  "diagnostic_fermes_avant_raffinement.csv"
  
)


write.csv(
  
  resultats_fermes_avant,
  
  fichier_diagnostic_avant,
  
  row.names = FALSE
  
)


cat(
  
  "\nDiagnostic avant raffinement sauvegarde dans :\n",
  
  fichier_diagnostic_avant,
  
  "\n\n"
)

# ==============================================================================
# 7. CREATION DU RASTER DES EXPLOITATIONS AGRICOLES
# ==============================================================================
#
# OBJECTIF :
#
# Associer chaque cellule accessible de la grille de coût à l'exploitation
# agricole (ID_farm_an) de la parcelle INVEKOS dans laquelle elle se trouve.
#
# IMPORTANT :
#
# ID_farm_an est un identifiant.
#
# Sa valeur numérique ne représente PAS un coût.
#
# Par exemple :
#
# ID 500 n'est pas "plus coûteux" que ID 10.
#
# Le raster créé ici servira uniquement à identifier les changements
# d'exploitation lors du déplacement du chemin.
# ==============================================================================


cat("\n")
cat("====================================================\n")
cat("CREATION DU RASTER DES EXPLOITATIONS AGRICOLES\n")
cat("====================================================\n\n")


# ------------------------------------------------------------------------------
# 7.1 VERIFICATION DE LA GRILLE DE REFERENCE
# ------------------------------------------------------------------------------
#
# Le raster des fermes doit être parfaitement aligné avec grille_cout.
#
# Nous utilisons donc directement grille_cout comme grille de référence.


grille_fermes <- rasterize(
  
  vect(parcelles_invekos_modele),
  
  grille_cout,
  
  field = "ID_farm_an",
  
  background = NA,
  
  touches = FALSE
  
)


# ------------------------------------------------------------------------------
# 7.2 APPLICATION DU MASQUE DES CELLULES ACCESSIBLES
# ------------------------------------------------------------------------------
#
# Une cellule interdite dans grille_cout doit rester interdite ici.
#
# Cela garantit que :
#
# grille_cout = NA
#
# implique également :
#
# grille_fermes = NA
#


grille_fermes <- mask(
  
  grille_fermes,
  
  grille_cout
  
)


# ------------------------------------------------------------------------------
# 7.3 CONTROLE DU RASTER DES FERMES
# ------------------------------------------------------------------------------

valeurs_fermes <- values(
  
  grille_fermes,
  
  na.rm = TRUE
  
)


cat(
  
  "Nombre de cellules avec une exploitation identifiee : ",
  
  length(valeurs_fermes),
  
  "\n",
  
  sep = ""
)


cat(
  
  "Nombre d'exploitations presentes dans le raster : ",
  
  length(unique(valeurs_fermes)),
  
  "\n",
  
  sep = ""
)


# ------------------------------------------------------------------------------
# 7.3.1 CORRECTION DEFINITIVE DES CELLULES ACCESSIBLES SANS EXPLOITATION
# ------------------------------------------------------------------------------
#
# Certaines cellules sont accessibles dans grille_cout mais ne possèdent
# pas d'identifiant ID_farm_an.
#
# Ces cellules correspondent notamment aux corridors artificiellement ouverts
# dans le script 02.
#
# Elles doivent rester accessibles pour garantir la connectivité du modèle.
#
# Elles reçoivent donc un identifiant technique spécial :
#
# -9999 = cellule accessible neutre / corridor / sans exploitation réelle.
#
# IMPORTANT :
#
# -9999 ne représente PAS une exploitation agricole.
# Cette valeur sera ignorée lors du comptage des exploitations distinctes
# et ne devra pas déclencher de pénalité de changement d'exploitation.
# ------------------------------------------------------------------------------


ID_FERME_CORRIDOR <- -9999


# ------------------------------------------------------------------------------
# ETAPE 1 : EXTRACTION DES VALEURS DES DEUX RASTERS
# ------------------------------------------------------------------------------

valeurs_cout <- values(
  
  grille_cout,
  
  mat = FALSE
  
)


valeurs_fermes <- values(
  
  grille_fermes,
  
  mat = FALSE
  
)


# ------------------------------------------------------------------------------
# ETAPE 2 : IDENTIFICATION DES CELLULES ACCESSIBLES
# ------------------------------------------------------------------------------

cellules_accessibles <- !is.na(
  
  valeurs_cout
  
)


# ------------------------------------------------------------------------------
# ETAPE 3 : IDENTIFICATION DES CELLULES ACCESSIBLES SANS FERME
# ------------------------------------------------------------------------------

cellules_sans_ferme_avant <- cellules_accessibles &
  
  is.na(
    
    valeurs_fermes
    
  )


nombre_cellules_sans_ferme_avant <- sum(
  
  cellules_sans_ferme_avant
  
)


cat(
  
  "Nombre de cellules accessibles sans ID_farm_an AVANT correction : ",
  
  nombre_cellules_sans_ferme_avant,
  
  "\n",
  
  sep = ""
)


# ------------------------------------------------------------------------------
# ETAPE 4 : ATTRIBUTION DE L'IDENTIFIANT TECHNIQUE AUX CELLULES CONCERNEES
# ------------------------------------------------------------------------------

valeurs_fermes[
  
  cellules_sans_ferme_avant
  
] <- ID_FERME_CORRIDOR


# ------------------------------------------------------------------------------
# ETAPE 5 : RECONSTRUCTION DU RASTER DES EXPLOITATIONS
# ------------------------------------------------------------------------------

values(
  
  grille_fermes
  
) <- valeurs_fermes


# ------------------------------------------------------------------------------
# ETAPE 6 : CONTROLE FINAL APRES CORRECTION
# ------------------------------------------------------------------------------

valeurs_fermes_finales <- values(
  
  grille_fermes,
  
  mat = FALSE
  
)


cellules_sans_ferme_apres <- cellules_accessibles &
  
  is.na(
    
    valeurs_fermes_finales
    
  )


nombre_cellules_sans_ferme_apres <- sum(
  
  cellules_sans_ferme_apres
  
)


# ------------------------------------------------------------------------------
# ETAPE 7 : NOMBRE DE CELLULES IDENTIFIEES COMME CORRIDOR
# ------------------------------------------------------------------------------

nombre_cellules_corridor <- sum(
  
  valeurs_fermes_finales == ID_FERME_CORRIDOR,
  
  na.rm = TRUE
  
)


cat(
  
  "Identifiant technique des corridors : ",
  
  ID_FERME_CORRIDOR,
  
  "\n",
  
  sep = ""
)


cat(
  
  "Nombre de cellules marquees comme corridor : ",
  
  nombre_cellules_corridor,
  
  "\n",
  
  sep = ""
)


cat(
  
  "Nombre de cellules accessibles sans ID_farm_an APRES correction : ",
  
  nombre_cellules_sans_ferme_apres,
  
  "\n\n",
  
  sep = ""
)


# ------------------------------------------------------------------------------
# ETAPE 8 : VERIFICATION DEFINITIVE
# ------------------------------------------------------------------------------

if (nombre_cellules_sans_ferme_apres > 0) {
  
  stop(
    
    "ERREUR DEFINITIVE : ",
    
    nombre_cellules_sans_ferme_apres,
    
    " cellules accessibles ne possèdent toujours pas ",
    
    "d'identifiant dans grille_fermes."
    
  )
  
}


cat(
  
  "VERIFICATION REUSSIE : toutes les cellules accessibles ",
  
  "possedent maintenant une valeur dans grille_fermes.\n\n"
)

cat(
  
  "Nombre total de cellules accessibles : ",
  
  sum(
    
    cellules_accessibles
    
  ),
  
  "\n",
  
  sep = ""
)


cat(
  
  "Nombre de cellules avec une vraie exploitation : ",
  
  sum(
    
    cellules_accessibles &
      valeurs_fermes_finales != ID_FERME_CORRIDOR,
    
    na.rm = TRUE
    
  ),
  
  "\n",
  
  sep = ""
)


cat(
  
  "Nombre de cellules de corridor : ",
  
  sum(
    
    valeurs_fermes_finales == ID_FERME_CORRIDOR,
    
    na.rm = TRUE
    
  ),
  
  "\n\n",
  
  sep = ""
)

# ------------------------------------------------------------------------------
# 7.4 TRAITEMENT DES CELLULES ACCESSIBLES SANS EXPLOITATION
# ------------------------------------------------------------------------------
#
# Certaines cellules sont accessibles dans grille_cout mais ne possèdent
# pas d'ID_farm_an.
#
# Elles correspondent principalement aux cellules artificiellement ouvertes
# lors de la création des corridors dans le script 02.
#
# Ces cellules doivent rester accessibles pour garantir la connectivité
# spatiale entre les patches.
#
# Elles ne représentent cependant PAS une exploitation agricole.
#
# On leur attribue donc un identifiant technique spécial.
#
# IMPORTANT :
#
# Cette valeur ne sera jamais comptée comme une véritable ferme.
# Elle servira uniquement à identifier les cellules de transition /
# corridor dans les calculs ultérieurs.
# ------------------------------------------------------------------------------


ID_FERME_CORRIDOR <- -9999


# Extraction des valeurs actuelles du raster.

valeurs_fermes_completes <- values(
  
  grille_fermes,
  
  mat = FALSE
  
)


# Extraction des cellules accessibles dans la grille de coût.

cellules_accessibles <- !is.na(
  
  values(
    
    grille_cout,
    
    mat = FALSE
    
  )
  
)


# Identification précise des cellules :
#
# - accessibles ;
# - mais sans exploitation.

cellules_accessibles_sans_ferme <-
  
  cellules_accessibles &
  
  is.na(
    
    valeurs_fermes_completes
    
  )


# Attribution de l'identifiant technique uniquement à ces cellules.

valeurs_fermes_completes[
  
  cellules_accessibles_sans_ferme
  
] <- ID_FERME_CORRIDOR


# Réinjection des valeurs dans le raster.

values(
  
  grille_fermes
  
) <- valeurs_fermes_completes


# ------------------------------------------------------------------------------
# VERIFICATION APRES CORRECTION
# ------------------------------------------------------------------------------

valeurs_cout_verification <- values(
  
  grille_cout,
  
  mat = FALSE
  
)


valeurs_fermes_verification <- values(
  
  grille_fermes,
  
  mat = FALSE
  
)


cellules_accessibles_verification <- !is.na(
  
  valeurs_cout_verification
  
)


cellules_accessibles_sans_ferme_apres <-
  
  cellules_accessibles_verification &
  
  is.na(
    
    valeurs_fermes_verification
    
  )


nombre_cellules_sans_ferme_apres <- sum(
  
  cellules_accessibles_sans_ferme_apres,
  
  na.rm = TRUE
  
)


nombre_cellules_corridor_ferme <- sum(
  
  valeurs_fermes_verification == ID_FERME_CORRIDOR,
  
  na.rm = TRUE
  
)


cat(
  
  "====================================================\n"
  
)


cat(
  
  "CORRECTION DES CELLULES SANS EXPLOITATION\n"
  
)


cat(
  
  "====================================================\n"
  
)


cat(
  
  "Identifiant technique utilise pour les corridors : ",
  
  ID_FERME_CORRIDOR,
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Nombre de cellules marquees comme corridor : ",
  
  nombre_cellules_corridor_ferme,
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Nombre de cellules accessibles sans ID apres correction : ",
  
  nombre_cellules_sans_ferme_apres,
  
  "\n\n",
  
  sep = ""
  
)


# Vérification finale obligatoire.

if (nombre_cellules_sans_ferme_apres > 0) {
  
  stop(
    
    "ERREUR : certaines cellules accessibles ne possèdent toujours ",
    
    "pas de valeur dans grille_fermes."
    
  )
  
}

# ------------------------------------------------------------------------------
# 7.4 SAUVEGARDE DU RASTER DES FERMES
# ------------------------------------------------------------------------------

fichier_grille_fermes <- file.path(
  
  DOSSIER_SORTIE_ETAPE4,
  
  "grille_ID_farm_an.tif"
  
)


writeRaster(
  
  grille_fermes,
  
  fichier_grille_fermes,
  
  overwrite = TRUE
  
)


cat(
  
  "Raster des exploitations sauvegarde dans :\n",
  
  fichier_grille_fermes,
  
  "\n\n"
)

# ==============================================================================
# 8. PARAMETRE DE PENALITE LIE AUX TRANSITIONS ENTRE EXPLOITATIONS
# ==============================================================================
#
# PRINCIPE :
#
# - rester dans la même exploitation :
#       pénalité = 0
#
# - passer dans une exploitation différente :
#       pénalité > 0
#
# IMPORTANT :
#
# Cette valeur est un paramètre de sensibilité du modèle.
#
# Elle pourra être testée avec plusieurs valeurs lors de l'analyse finale.
# ==============================================================================


PENALITE_CHANGEMENT_FERME <- 0.5


cat("====================================================\n")
cat("PARAMETRE EXPLOITATIONS\n")
cat("====================================================\n")


cat(
  
  "Penalite pour un changement d'exploitation : ",
  
  PENALITE_CHANGEMENT_FERME,
  
  "\n\n",
  
  sep = ""
)


# ==============================================================================
# 9. IDENTIFICATION DES ZONES DE TRANSITION ENTRE EXPLOITATIONS
# ==============================================================================
#
# OBJECTIF :
#
# Identifier les cellules accessibles situées à proximité immédiate
# d'une autre exploitation agricole.
#
# Une cellule située dans une exploitation donnée reçoit une pénalité
# supplémentaire lorsqu'elle se trouve sur une frontière avec une
# exploitation différente.
#
# IMPORTANT :
#
# ID_farm_an est une variable catégorielle.
#
# Nous ne comparons donc jamais la valeur numérique des identifiants.
#
# Exemple :
#
# 481 n'est PAS considéré comme plus coûteux que 61.
#
# La seule information utilisée est :
#
# même ID_farm_an
#
# ou
#
# ID_farm_an différent.
# ==============================================================================


cat("\n")
cat("====================================================\n")
cat("IDENTIFICATION DES FRONTIERES ENTRE EXPLOITATIONS\n")
cat("====================================================\n\n")


# ------------------------------------------------------------------------------
# 9.1 VERIFICATION DE L'ALIGNEMENT DES RASTERS
# ------------------------------------------------------------------------------
#
# grille_cout et grille_fermes doivent posséder exactement :
#
# - la même emprise ;
# - la même résolution ;
# - le même nombre de lignes ;
# - le même nombre de colonnes.
#
# Sans cela, les comparaisons entre cellules seraient incorrectes.
# ------------------------------------------------------------------------------

if (
  
  !compareGeom(
    
    grille_cout,
    
    grille_fermes,
    
    stopOnError = FALSE
    
  )
  
) {
  
  stop(
    
    "ERREUR : grille_cout et grille_fermes ne sont pas parfaitement ",
    
    "alignées.\n\n",
    
    "Les deux rasters doivent posséder exactement la même géométrie."
    
  )
  
}


cat(
  
  "Verification geometrie des rasters : OK\n\n"
  
)


# ------------------------------------------------------------------------------
# 9.2 CREATION DES DEPLACEMENTS VERS LES CELLULES VOISINES
# ------------------------------------------------------------------------------
#
# Nous allons comparer chaque cellule avec ses voisines.
#
# directions = 8 signifie que nous considérons :
#
# - Nord ;
# - Sud ;
# - Est ;
# - Ouest ;
# - les 4 diagonales.
#
# Cela est cohérent avec la logique de connectivité utilisée précédemment
# dans le modèle.
# ------------------------------------------------------------------------------

# Matrice des valeurs des exploitations.

matrice_fermes <- as.matrix(
  
  grille_fermes,
  
  wide = TRUE
  
)


# Nombre de lignes et de colonnes.

nombre_lignes <- nrow(
  
  matrice_fermes
  
)


nombre_colonnes <- ncol(
  
  matrice_fermes
  
)


# ------------------------------------------------------------------------------
# 9.3 CREATION D'UNE MATRICE DES FRONTIERES AGRICOLES
# ------------------------------------------------------------------------------
#
# Valeur initiale :
#
# 0 = cellule ne nécessitant pas de pénalité
#
# 1 = cellule située sur ou immédiatement à proximité d'une transition
#     entre deux exploitations différentes.
# ------------------------------------------------------------------------------

matrice_frontieres_fermes <- matrix(
  
  0,
  
  nrow = nombre_lignes,
  
  ncol = nombre_colonnes
  
)


# ------------------------------------------------------------------------------
# 9.4 COMPARAISON HORIZONTALE
# ------------------------------------------------------------------------------
#
# Comparaison de chaque cellule avec la cellule immédiatement située
# à sa droite.
#
# Si les deux cellules possèdent :
#
# - un ID_farm_an non manquant ;
# - deux ID_farm_an différents ;
#
# alors les deux cellules sont marquées comme appartenant à une zone
# de transition entre exploitations.
# ------------------------------------------------------------------------------

fermes_gauche <- matrice_fermes[
  ,
  1:(nombre_colonnes - 1),
  drop = FALSE
]

fermes_droite <- matrice_fermes[
  ,
  2:nombre_colonnes,
  drop = FALSE
]


transition_horizontale <- (
  !is.na(fermes_gauche) &
    !is.na(fermes_droite) &
    fermes_gauche != fermes_droite &
    fermes_gauche != ID_FERME_CORRIDOR &
    fermes_droite != ID_FERME_CORRIDOR
)

# Marquage des cellules situées à gauche de la frontière.

sous_matrice_gauche <- matrice_frontieres_fermes[
  ,
  1:(nombre_colonnes - 1),
  drop = FALSE
]


sous_matrice_gauche[
  transition_horizontale
] <- 1


matrice_frontieres_fermes[
  ,
  1:(nombre_colonnes - 1)
] <- sous_matrice_gauche


# Marquage des cellules situées à droite de la frontière.

sous_matrice_droite <- matrice_frontieres_fermes[
  ,
  2:nombre_colonnes,
  drop = FALSE
]


sous_matrice_droite[
  transition_horizontale
] <- 1


matrice_frontieres_fermes[
  ,
  2:nombre_colonnes
] <- sous_matrice_droite


# ------------------------------------------------------------------------------
# 9.5 COMPARAISON VERTICALE
# ------------------------------------------------------------------------------
#
# Comparaison de chaque cellule avec la cellule immédiatement située
# en dessous.
# ------------------------------------------------------------------------------

fermes_haut <- matrice_fermes[
  1:(nombre_lignes - 1),
  ,
  drop = FALSE
]


fermes_bas <- matrice_fermes[
  2:nombre_lignes,
  ,
  drop = FALSE
]


transition_verticale <- (
  !is.na(fermes_haut) &
    !is.na(fermes_bas) &
    fermes_haut != fermes_bas &
    fermes_haut != ID_FERME_CORRIDOR &
    fermes_bas != ID_FERME_CORRIDOR
)

# Marquage des cellules situées au-dessus de la frontière.

sous_matrice_haut <- matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  ,
  drop = FALSE
]


sous_matrice_haut[
  transition_verticale
] <- 1


matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  
] <- sous_matrice_haut


# Marquage des cellules situées en dessous de la frontière.

sous_matrice_bas <- matrice_frontieres_fermes[
  2:nombre_lignes,
  ,
  drop = FALSE
]


sous_matrice_bas[
  transition_verticale
] <- 1


matrice_frontieres_fermes[
  2:nombre_lignes,
  
] <- sous_matrice_bas


# ------------------------------------------------------------------------------
# 9.6 COMPARAISON DIAGONALE PRINCIPALE
# ------------------------------------------------------------------------------
#
# Comparaison :
#
# cellule (ligne, colonne)
#
# avec :
#
# cellule (ligne + 1, colonne + 1)
# ------------------------------------------------------------------------------

fermes_diag1_a <- matrice_fermes[
  1:(nombre_lignes - 1),
  1:(nombre_colonnes - 1),
  drop = FALSE
]


fermes_diag1_b <- matrice_fermes[
  2:nombre_lignes,
  2:nombre_colonnes,
  drop = FALSE
]


transition_diag1 <- (
  !is.na(fermes_diag1_a) &
    !is.na(fermes_diag1_b) &
    fermes_diag1_a != fermes_diag1_b &
    fermes_diag1_a != ID_FERME_CORRIDOR &
    fermes_diag1_b != ID_FERME_CORRIDOR
)

# Première partie de la diagonale.

sous_matrice_diag1_a <- matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  1:(nombre_colonnes - 1),
  drop = FALSE
]


sous_matrice_diag1_a[
  transition_diag1
] <- 1


matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  1:(nombre_colonnes - 1)
] <- sous_matrice_diag1_a


# Seconde partie de la diagonale.

sous_matrice_diag1_b <- matrice_frontieres_fermes[
  2:nombre_lignes,
  2:nombre_colonnes,
  drop = FALSE
]


sous_matrice_diag1_b[
  transition_diag1
] <- 1


matrice_frontieres_fermes[
  2:nombre_lignes,
  2:nombre_colonnes
] <- sous_matrice_diag1_b


# ------------------------------------------------------------------------------
# 9.7 COMPARAISON DE LA SECONDE DIAGONALE
# ------------------------------------------------------------------------------
#
# Comparaison :
#
# cellule (ligne, colonne + 1)
#
# avec :
#
# cellule (ligne + 1, colonne)
# ------------------------------------------------------------------------------

fermes_diag2_a <- matrice_fermes[
  1:(nombre_lignes - 1),
  2:nombre_colonnes,
  drop = FALSE
]


fermes_diag2_b <- matrice_fermes[
  2:nombre_lignes,
  1:(nombre_colonnes - 1),
  drop = FALSE
]


transition_diag2 <- (
  !is.na(fermes_diag2_a) &
    !is.na(fermes_diag2_b) &
    fermes_diag2_a != fermes_diag2_b &
    fermes_diag2_a != ID_FERME_CORRIDOR &
    fermes_diag2_b != ID_FERME_CORRIDOR
)

# Première partie.

sous_matrice_diag2_a <- matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  2:nombre_colonnes,
  drop = FALSE
]


sous_matrice_diag2_a[
  transition_diag2
] <- 1


matrice_frontieres_fermes[
  1:(nombre_lignes - 1),
  2:nombre_colonnes
] <- sous_matrice_diag2_a


# Seconde partie.

sous_matrice_diag2_b <- matrice_frontieres_fermes[
  2:nombre_lignes,
  1:(nombre_colonnes - 1),
  drop = FALSE
]


sous_matrice_diag2_b[
  transition_diag2
] <- 1


matrice_frontieres_fermes[
  2:nombre_lignes,
  1:(nombre_colonnes - 1)
] <- sous_matrice_diag2_b

# ------------------------------------------------------------------------------
# 9.8 CONVERSION EN RASTER
# ------------------------------------------------------------------------------

raster_frontieres_fermes <- rast(
  
  grille_fermes
  
)


values(
  
  raster_frontieres_fermes
  
) <- as.vector(
  
  matrice_frontieres_fermes
  
)


# Les cellules hors de la zone accessible doivent rester interdites.

raster_frontieres_fermes <- mask(
  
  raster_frontieres_fermes,
  
  grille_cout
  
)


# ------------------------------------------------------------------------------
# 9.9 CONTROLE
# ------------------------------------------------------------------------------

valeurs_frontieres <- values(
  
  raster_frontieres_fermes,
  
  mat = FALSE
  
)


nombre_cellules_frontieres <- sum(
  
  valeurs_frontieres == 1,
  
  na.rm = TRUE
  
)


nombre_cellules_non_frontieres <- sum(
  
  valeurs_frontieres == 0,
  
  na.rm = TRUE
  
)


cat(
  
  "Nombre de cellules situees sur une transition entre fermes : ",
  
  nombre_cellules_frontieres,
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Nombre de cellules sans transition immediate : ",
  
  nombre_cellules_non_frontieres,
  
  "\n\n",
  
  sep = ""
  
)

# ==============================================================================
# 10. CREATION DE LA PENALITE AGRICOLE
# ==============================================================================
#
# Les cellules identifiées comme situées sur une frontière entre deux
# exploitations différentes reçoivent une pénalité.
#
# Cette pénalité est nulle à l'intérieur d'une même exploitation.
#
# Elle est égale à :
#
# PENALITE_CHANGEMENT_FERME
#
# sur les zones de transition.
# ==============================================================================


cat("====================================================\n")
cat("CREATION DE LA PENALITE AGRICOLE\n")
cat("====================================================\n\n")


penalite_fermes <- ifel(
  
  raster_frontieres_fermes == 1,
  
  PENALITE_CHANGEMENT_FERME,
  
  0
  
)


# Application finale du masque.
#
# Les cellules interdites doivent rester NA.

penalite_fermes <- mask(
  
  penalite_fermes,
  
  grille_cout
  
)


# ------------------------------------------------------------------------------
# CONTROLE DES VALEURS
# ------------------------------------------------------------------------------

valeurs_penalite <- values(
  
  penalite_fermes,
  
  na.rm = TRUE
  
)


cat(
  
  "Penalite minimale : ",
  
  min(valeurs_penalite),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Penalite maximale : ",
  
  max(valeurs_penalite),
  
  "\n\n",
  
  sep = ""
  
)

# ==============================================================================
# 11. CONSTRUCTION DE LA SURFACE DE COUT RAFFINEE
# ==============================================================================
#
# Nouvelle formule :
#
# cout_raffine =
#
# cout_initial
#
# +
#
# penalite_transition_ferme
#
#
# RAPPEL :
#
# cout_initial contient déjà :
#
# - la dimension économique liée au gross margin ;
# - la dimension écologique liée aux occurrences ;
# - les corridors éventuellement ajoutés.
#
# Nous ajoutons maintenant uniquement le critère agricole.
# ==============================================================================


cat("====================================================\n")
cat("CONSTRUCTION DE LA SURFACE DE COUT RAFFINEE\n")
cat("====================================================\n\n")


grille_cout_raffinee <- (
  
  grille_cout +
    
    penalite_fermes
  
)


# ------------------------------------------------------------------------------
# APPLICATION FINALE DU MASQUE
# ------------------------------------------------------------------------------

grille_cout_raffinee <- mask(
  
  grille_cout_raffinee,
  
  grille_cout
  
)


# ------------------------------------------------------------------------------
# VERIFICATION DES COUTS
# ------------------------------------------------------------------------------

valeurs_cout_raffine <- values(
  
  grille_cout_raffinee,
  
  na.rm = TRUE
  
)


if (
  
  any(
    
    valeurs_cout_raffine <= 0
    
  )
  
) {
  
  stop(
    
    "ERREUR : certaines cellules accessibles possèdent un coût <= 0 ",
    
    "après ajout de la pénalité agricole."
    
  )
  
}


cat(
  
  "Cout minimum apres raffinement : ",
  
  min(valeurs_cout_raffine),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Cout maximum apres raffinement : ",
  
  max(valeurs_cout_raffine),
  
  "\n\n",
  
  sep = ""
  
)

# ==============================================================================
# 12. SAUVEGARDE DES RASTERS INTERMEDIAIRES
# ==============================================================================


# ------------------------------------------------------------------------------
# Raster des zones de transition entre exploitations
# ------------------------------------------------------------------------------

fichier_frontieres_fermes <- file.path(
  
  DOSSIER_SORTIE_ETAPE4,
  
  "frontieres_entre_exploitations.tif"
  
)


writeRaster(
  
  raster_frontieres_fermes,
  
  fichier_frontieres_fermes,
  
  overwrite = TRUE
  
)


# ------------------------------------------------------------------------------
# Raster de pénalité agricole
# ------------------------------------------------------------------------------

fichier_penalite_fermes <- file.path(
  
  DOSSIER_SORTIE_ETAPE4,
  
  "penalite_transitions_fermes.tif"
  
)


writeRaster(
  
  penalite_fermes,
  
  fichier_penalite_fermes,
  
  overwrite = TRUE
  
)


# ------------------------------------------------------------------------------
# Nouvelle surface de coût
# ------------------------------------------------------------------------------

fichier_grille_cout_raffinee <- file.path(
  
  DOSSIER_SORTIE_ETAPE4,
  
  "grille_cout_raffinee_fermes.tif"
  
)


writeRaster(
  
  grille_cout_raffinee,
  
  fichier_grille_cout_raffinee,
  
  overwrite = TRUE
  
)


cat(
  
  "Rasters de l'etape 4 sauvegardes avec succes.\n\n"
  
)

# ==============================================================================
# 13. CARTE DE CONTROLE DES FRONTIERES AGRICOLES
# ==============================================================================


frontieres_fermes_df <- as.data.frame(
  
  raster_frontieres_fermes,
  
  xy = TRUE,
  
  na.rm = TRUE
  
)


names(
  
  frontieres_fermes_df
  
)[3] <- "frontiere"


ggplot() +
  
  geom_raster(
    
    data = frontieres_fermes_df,
    
    aes(
      
      x = x,
      
      y = y,
      
      fill = factor(frontiere)
      
    )
    
  ) +
  
  geom_sf(
    
    data = patches,
    
    fill = NA,
    
    color = "black",
    
    linewidth = 0.3
    
  ) +
  
  coord_sf() +
  
  labs(
    
    title =
      "Zones de transition entre exploitations agricoles",
    
    subtitle =
      paste0(
        
        "Les cellules situees a une interface entre deux ID_farm_an ",
        
        "differents recoivent une penalite de ",
        
        PENALITE_CHANGEMENT_FERME
        
      ),
    
    fill =
      "Transition agricole"
    
  ) +
  
  theme_minimal()


# ==============================================================================
# 14. VERIFICATION DES OBJETS NECESSAIRES (issus du script 03)
# ==============================================================================
# resultats_points et calculer_chemin_moindre_cout sont créés dans le
# script 03. Ils doivent encore être présents dans ta session R.

objets_necessaires_etape4b <- c(
  "resultats_points",
  "calculer_chemin_moindre_cout",
  "NOMBRE_VOISINS"
)

objets_manquants_etape4b <- objets_necessaires_etape4b[
  !objets_necessaires_etape4b %in% ls()
]

if (length(objets_manquants_etape4b) > 0) {
  stop(
    "ERREUR : les objets suivants sont absents de l'environnement R : ",
    paste(objets_manquants_etape4b, collapse = ", "),
    "\n\nCes objets viennent du script 03. Exécute 01 -> 02 -> 03 -> 04 ",
    "dans la même session R, sans redémarrer RStudio entre les deux."
  )
}


# ==============================================================================
# 15. RECALCUL DES CHEMINS SUR LA GRILLE DE COUT RAFFINEE
# ==============================================================================
# Jusqu'ici, seule la SURFACE de coût a été raffinée. Les chemins eux-mêmes
# (chemins_finaux) sont encore ceux du script 03, calculés SANS la pénalité
# de changement d'exploitation. On recalcule maintenant les 13 chemins sur
# grille_cout_raffinee, en réutilisant les MÊMES cellules de départ/arrivée
# que le script 03 (resultats_points), pour que la comparaison avant/après
# porte uniquement sur l'effet de la pénalité - pas sur un changement de
# points de départ/arrivée.
# ==============================================================================

cat("\n")
cat("====================================================\n")
cat("RECALCUL DES CHEMINS SUR LA GRILLE RAFFINEE\n")
cat("====================================================\n\n")

# 15.1 - Surface de conductance raffinée (même logique que le script 03)
grille_conductance_raffinee <- terra::ifel(
  !is.na(grille_cout_raffinee),
  1 / grille_cout_raffinee,
  NA
)

# 15.2 - Matrice de conductance
surface_conductance_raffinee <- leastcostpath::create_cs(
  x = grille_conductance_raffinee,
  neighbours = NOMBRE_VOISINS
)

# 15.3 - Graphe de déplacement raffiné
cm_graph_raffine <- igraph::graph_from_adjacency_matrix(
  surface_conductance_raffinee$conductanceMatrix,
  mode = "directed",
  weighted = TRUE
)

igraph::E(cm_graph_raffine)$weight <- 1 / igraph::E(cm_graph_raffine)$weight

cat("Graphe raffiné créé.\n\n")

# 15.4 - Recalcul des 13 chemins (mêmes cellules départ/arrivée qu'au script 03)
liste_chemins_raffines <- vector("list", nrow(paires_patches))

for (i in seq_len(nrow(paires_patches))) {
  
  id_a <- paires_patches$patch_a[i]
  id_b <- paires_patches$patch_b[i]
  
  cellule_depart  <- resultats_points$cellule_depart[i]
  cellule_arrivee <- resultats_points$cellule_arrivee[i]
  
  resultat_chemin <- calculer_chemin_moindre_cout(
    cellule_depart  = cellule_depart,
    cellule_arrivee = cellule_arrivee,
    grille_cout     = grille_cout_raffinee,
    cm_graph        = cm_graph_raffine
  )
  
  if (is.null(resultat_chemin)) {
    stop(
      "ERREUR : aucun chemin raffiné n'a pu être calculé pour la paire ",
      id_a, " - ", id_b
    )
  }
  
  chemin_sf <- st_sf(
    patch_a         = id_a,
    patch_b         = id_b,
    cellule_depart  = cellule_depart,
    cellule_arrivee = cellule_arrivee,
    nombre_cellules = length(resultat_chemin$cellules),
    cout_accumule   = resultat_chemin$cout_accumule,
    geometry        = st_sfc(resultat_chemin$geometrie, crs = CRS_PROJET)
  )
  
  liste_chemins_raffines[[i]] <- chemin_sf
  
  cat(
    "Chemin raffiné ", i, "/", nrow(paires_patches),
    " (patch ", id_a, " -> ", id_b, ") : cout accumule = ",
    round(resultat_chemin$cout_accumule, 4), "\n",
    sep = ""
  )
}

chemins_finaux_raffines <- do.call(rbind, liste_chemins_raffines)
rownames(chemins_finaux_raffines) <- NULL

cat("\nNombre de chemins raffinés créés :", nrow(chemins_finaux_raffines), "\n\n")


# ==============================================================================
# 16. DIAGNOSTIC DES FERMES APRES RAFFINEMENT
# ==============================================================================
# Même logique que la section 5, appliquée cette fois à chemins_finaux_raffines.

resultats_fermes_apres <- vector("list", nrow(chemins_finaux_raffines))

for (i in seq_len(nrow(chemins_finaux_raffines))) {
  
  chemin_i <- chemins_finaux_raffines[i, ]
  id_a <- chemin_i$patch_a
  id_b <- chemin_i$patch_b
  
  parcelles_traversees <- st_intersects(
    chemin_i, parcelles_invekos_modele
  )[[1]]
  
  if (length(parcelles_traversees) == 0) {
    resultats_fermes_apres[[i]] <- data.frame(
      patch_a = id_a, patch_b = id_b,
      nombre_parcelles = 0, nombre_fermes_distinctes = 0,
      liste_fermes = NA_character_
    )
    next
  }
  
  ids_fermes <- parcelles_invekos_modele$ID_farm_an[parcelles_traversees]
  ids_fermes <- ids_fermes[!is.na(ids_fermes)]
  fermes_uniques <- unique(ids_fermes)
  
  resultats_fermes_apres[[i]] <- data.frame(
    patch_a = id_a, patch_b = id_b,
    nombre_parcelles = length(parcelles_traversees),
    nombre_fermes_distinctes = length(fermes_uniques),
    liste_fermes = paste(fermes_uniques, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

resultats_fermes_apres <- bind_rows(resultats_fermes_apres)

cat("RESULTATS APRES RAFFINEMENT :\n\n")
print(resultats_fermes_apres)

fichier_diagnostic_apres <- file.path(
  DOSSIER_SORTIE_ETAPE4, "diagnostic_fermes_apres_raffinement.csv"
)
write.csv(resultats_fermes_apres, fichier_diagnostic_apres, row.names = FALSE)

cat("\nDiagnostic apres raffinement sauvegarde dans :\n",
    fichier_diagnostic_apres, "\n\n")


# ==============================================================================
# 17. COMPARAISON AVANT / APRES RAFFINEMENT
# ==============================================================================

comparaison_fermes <- resultats_fermes_avant %>%
  select(patch_a, patch_b, fermes_avant = nombre_fermes_distinctes) %>%
  left_join(
    resultats_fermes_apres %>%
      select(patch_a, patch_b, fermes_apres = nombre_fermes_distinctes),
    by = c("patch_a", "patch_b")
  ) %>%
  mutate(difference = fermes_apres - fermes_avant)

cat("\n=== COMPARAISON AVANT / APRES ===\n\n")
print(comparaison_fermes)

cat(
  "\nChemins avec MOINS de fermes apres raffinement : ",
  sum(comparaison_fermes$difference < 0, na.rm = TRUE), "\n",
  "Chemins INCHANGES : ",
  sum(comparaison_fermes$difference == 0, na.rm = TRUE), "\n",
  "Chemins avec PLUS de fermes apres raffinement : ",
  sum(comparaison_fermes$difference > 0, na.rm = TRUE), "\n\n",
  sep = ""
)

fichier_comparaison <- file.path(
  DOSSIER_SORTIE_ETAPE4, "comparaison_fermes_avant_apres.csv"
)
write.csv(comparaison_fermes, fichier_comparaison, row.names = FALSE)

cat("Comparaison sauvegardee dans :\n", fichier_comparaison, "\n\n")


# ==============================================================================
# 18. SAUVEGARDE DES CHEMINS RAFFINES
# ==============================================================================

fichier_chemins_raffines <- file.path(
  DOSSIER_SORTIE_ETAPE4, "chemins_finaux_raffines.gpkg"
)

st_write(
  chemins_finaux_raffines,
  fichier_chemins_raffines,
  layer = "chemins_finaux_raffines",
  delete_layer = TRUE
)

cat("Chemins raffines sauvegardes dans :\n", fichier_chemins_raffines, "\n\n")

cat("====================================================\n")
cat("SCRIPT 04 TERMINE (recalcul + comparaison inclus)\n")
cat("====================================================\n\n")