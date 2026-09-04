# ==============================================================================
# MÉMOIRE - Reconnexion des patches de pâturage
# Hungarian Grey Cattle
# Parc national Neusiedler See-Seewinkel, Autriche
#
# ÉTAPE 3 / 5
# CALCUL DES 13 CHEMINS DE MOINDRE COÛT
#
# OBJECTIFS DU SCRIPT :
#
# 1. Vérifier et récupérer la grille de coût finale.
# 2. Vérifier et récupérer les 13 paires de patches.
# 3. Calculer les points de bordure les plus proches entre chaque paire.
# 4. Rattacher ces points aux cellules accessibles du raster.
# 5. Vérifier une dernière fois la connectivité de chaque paire.
# 6. Calculer le chemin de moindre coût pour chacune des 13 paires.
# 7. Vérifier que les chemins utilisent uniquement le réseau de cellules
#    autorisées.
# 8. Sauvegarder individuellement chacun des 13 chemins.
# 9. Créer une couche unique contenant l'ensemble des chemins.
# 10. Calculer et sauvegarder les statistiques de chaque chemin.
# 11. Produire des cartes de contrôle des résultats.
#
# IMPORTANT :
#
# - Les centroïdes ne sont PAS utilisés pour calculer les chemins.
# - Chaque paire possède ses propres points de départ et d'arrivée.
# - Ces points correspondent aux deux bordures les plus proches.
# - Les cellules NA de grille_cout sont strictement interdites.
# - Les corridors <= 50 m créés dans le Script 02 font déjà partie de la
#   surface accessible.
# - AUCUN nouveau corridor n'est créé dans ce Script 03.
# ==============================================================================



# ==============================================================================
# 0. INSTALLATION DES PACKAGES
# ==============================================================================
#
# À exécuter UNE SEULE FOIS si nécessaire.
# Ensuite, commenter ou supprimer ces lignes.
#
# install.packages("sf")
# install.packages("terra")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("igraph")
# install.packages("Matrix")
# install.packages("leastcostpath")
# ==============================================================================



# ==============================================================================
# 1. CHARGEMENT DES LIBRAIRIES
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(igraph)
library(Matrix)
library(leastcostpath)



# ==============================================================================
# 2. CONFIGURATION GÉNÉRALE
# ==============================================================================

# ------------------------------------------------------------------------------
# CRS DU PROJET
# ------------------------------------------------------------------------------

CRS_PROJET <- 31287


# ------------------------------------------------------------------------------
# DOSSIER DES DONNÉES
# ------------------------------------------------------------------------------

DOSSIER_DONNEES <- "~/Desktop/Data_POM"


# ------------------------------------------------------------------------------
# DOSSIER DE SORTIE DES CHEMINS
# ------------------------------------------------------------------------------

DOSSIER_SORTIE_CHEMINS <- file.path(
  DOSSIER_DONNEES,
  "Resultats_Chemins"
)


# Création du dossier s'il n'existe pas

if (!dir.exists(DOSSIER_SORTIE_CHEMINS)) {
  
  dir.create(
    DOSSIER_SORTIE_CHEMINS,
    recursive = TRUE
  )
  
}


# ------------------------------------------------------------------------------
# PARAMÈTRE DE SNAP
# ------------------------------------------------------------------------------
#
# Distance maximale déjà validée lors du Diagnostic 2.
#
# La distance est évaluée par rapport à la SURFACE réelle de la cellule,
# et non uniquement à son centre.
# ------------------------------------------------------------------------------

DISTANCE_MAX_SNAP <- 50


# ------------------------------------------------------------------------------
# VOISINAGE DU MODÈLE
# ------------------------------------------------------------------------------
#
# 8 voisins :
#
# - horizontal
# - vertical
# - diagonal
#
# Cohérent avec les diagnostics précédents.
# ------------------------------------------------------------------------------

NOMBRE_VOISINS <- 8



# ==============================================================================
# 3. VÉRIFICATION DES OBJETS NÉCESSAIRES
# ==============================================================================

cat("\n")
cat("====================================================\n")
cat("SCRIPT 03 - CALCUL DES CHEMINS DE MOINDRE COUT\n")
cat("====================================================\n\n")


objets_necessaires <- c(
  
  "grille_cout",
  "patches",
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


# Vérification supplémentaire du raster

if (!inherits(grille_cout, "SpatRaster")) {
  
  stop(
    "ERREUR : grille_cout doit être un objet terra::SpatRaster."
  )
  
}


# Vérification des patches

if (!inherits(patches, "sf")) {
  
  stop(
    "ERREUR : patches doit être un objet sf."
  )
  
}


# Vérification de patch_id

if (!"patch_id" %in% names(patches)) {
  
  stop(
    
    "ERREUR : la colonne patch_id est absente de la couche patches.\n",
    
    "Elle doit être créée par le Script 01."
    
  )
  
}


# Vérification de la table des paires

colonnes_paires <- c(
  "patch_a",
  "patch_b"
)


colonnes_manquantes <- colonnes_paires[
  
  !colonnes_paires %in% names(paires_patches)
  
]


if (length(colonnes_manquantes) > 0) {
  
  stop(
    
    "ERREUR : les colonnes suivantes sont absentes de paires_patches : ",
    
    paste(
      colonnes_manquantes,
      collapse = ", "
    )
    
  )
  
}


# Vérification du nombre de paires

if (nrow(paires_patches) != 13) {
  
  warning(
    
    "ATTENTION : le script a été conçu pour les 13 paires actuellement ",
    "définies. Le nombre de paires détectées est : ",
    
    nrow(paires_patches)
    
  )
  
}


cat(
  "Nombre de paires à reconnecter :",
  nrow(paires_patches),
  "\n\n"
)



# ==============================================================================
# 4. VÉRIFICATION DE LA GRILLE DE COÛT FINALE
# ==============================================================================
#
# La logique du modèle est :
#
# - valeur numérique > 0 = cellule accessible ;
# - NA = cellule interdite.
#
# ------------------------------------------------------------------------------

valeurs_grille_cout <- terra::values(
  
  grille_cout,
  
  mat = FALSE
  
)


nombre_cellules_accessibles <- sum(
  
  !is.na(valeurs_grille_cout)
  
)


nombre_cellules_interdites <- sum(
  
  is.na(valeurs_grille_cout)
  
)


nombre_couts_non_positifs <- sum(
  
  !is.na(valeurs_grille_cout) &
    valeurs_grille_cout <= 0
  
)


cat("====================================================\n")
cat("VERIFICATION DE LA GRILLE DE COUT\n")
cat("====================================================\n\n")


cat(
  "Nombre total de cellules :",
  terra::ncell(grille_cout),
  "\n"
)


cat(
  "Nombre de cellules accessibles :",
  nombre_cellules_accessibles,
  "\n"
)


cat(
  "Nombre de cellules interdites :",
  nombre_cellules_interdites,
  "\n"
)


cat(
  "Nombre de cellules accessibles avec cout <= 0 :",
  nombre_couts_non_positifs,
  "\n\n"
)


# Un coût nul ou négatif provoquerait une conductance infinie ou négative.

if (nombre_couts_non_positifs > 0) {
  
  stop(
    
    "ERREUR : certaines cellules accessibles possèdent encore un coût ",
    "<= 0.\n\n",
    
    "Toutes les cellules accessibles doivent avoir un coût strictement ",
    "positif avant le calcul des chemins."
    
  )
  
}



# ==============================================================================
# 5. IDENTIFICATION DES CELLULES ACCESSIBLES
# ==============================================================================

cellules_accessibles <- which(
  
  !is.na(valeurs_grille_cout)
  
)


coordonnees_accessibles <- terra::xyFromCell(
  
  grille_cout,
  
  cellules_accessibles
  
)


points_accessibles <- st_as_sf(
  
  data.frame(
    
    cellule = cellules_accessibles,
    
    x = coordonnees_accessibles[, 1],
    
    y = coordonnees_accessibles[, 2]
    
  ),
  
  coords = c(
    "x",
    "y"
  ),
  
  crs = CRS_PROJET
  
)


cat("====================================================\n")
cat("RESEAU DE CELLULES ACCESSIBLES\n")
cat("====================================================\n\n")


cat(
  "Nombre de cellules accessibles disponibles pour les chemins :",
  nrow(points_accessibles),
  "\n\n"
)



# ==============================================================================
# 6. FONCTION : OBTENIR LES POINTS DE BORDURE LES PLUS PROCHES
# ==============================================================================
#
# Pour chaque paire :
#
# 1. on calcule la ligne la plus courte entre les deux patches ;
# 2. la première extrémité correspond au point de départ sur le patch A ;
# 3. la dernière extrémité correspond au point d'arrivée sur le patch B.
#
# IMPORTANT :
#
# Cette ligne n'est PAS le chemin final.
#
# Elle sert uniquement à déterminer les deux points de bordure entre lesquels
# le chemin de moindre coût devra être calculé.
# ------------------------------------------------------------------------------

obtenir_points_bordure <- function(
    
  patch_a,
  patch_b
  
) {
  
  
  ligne_plus_courte <- st_nearest_points(
    
    st_geometry(patch_a),
    
    st_geometry(patch_b)
    
  )
  
  
  coordonnees_ligne <- st_coordinates(
    
    ligne_plus_courte
    
  )
  
  
  if (nrow(coordonnees_ligne) < 2) {
    
    stop(
      
      "Impossible de déterminer deux points de bordure."
      
    )
    
  }
  
  
  point_depart <- st_sf(
    
    geometry = st_sfc(
      
      st_point(
        
        coordonnees_ligne[
          1,
          c("X", "Y")
        ]
        
      ),
      
      crs = CRS_PROJET
      
    )
    
  )
  
  
  point_arrivee <- st_sf(
    
    geometry = st_sfc(
      
      st_point(
        
        coordonnees_ligne[
          nrow(coordonnees_ligne),
          c("X", "Y")
        ]
        
      ),
      
      crs = CRS_PROJET
      
    )
    
  )
  
  
  return(
    
    list(
      
      depart = point_depart,
      
      arrivee = point_arrivee
      
    )
    
  )
  
}



trouver_cellule_accessible <- function(
    
  point_sf,
  
  points_accessibles,
  
  composantes,
  
  grille_reference,
  
  distance_max = 50
  
) {
  
  
  # --------------------------------------------------------------------------
  # 1. CELLULE ACCESSIBLE LA PLUS PROCHE
  # --------------------------------------------------------------------------
  
  index_proche <- st_nearest_feature(
    
    point_sf,
    
    points_accessibles
    
  )
  
  
  # Numéro unique de la cellule raster
  
  cellule_proche <- as.integer(
    
    points_accessibles$cellule[
      index_proche
    ]
    
  )
  
  
  # Vérification de sécurité
  
  if (
    length(cellule_proche) != 1 ||
    is.na(cellule_proche)
  ) {
    
    stop(
      "ERREUR : impossible d'identifier une cellule accessible unique."
    )
    
  }
  
  
  # Point correspondant au centre de cette cellule
  
  point_cellule <- points_accessibles[
    
    index_proche,
    
    drop = FALSE
    
  ]
  
  
  # --------------------------------------------------------------------------
  # 2. COORDONNEES DU POINT DE BORDURE
  # --------------------------------------------------------------------------
  
  coord_point <- st_coordinates(
    
    point_sf
    
  )
  
  
  # Sécurité : une seule géométrie point
  
  coord_point <- coord_point[
    
    1,
    
    c("X", "Y"),
    
    drop = FALSE
    
  ]
  
  
  # --------------------------------------------------------------------------
  # 3. COORDONNEES DU CENTRE DE LA CELLULE
  # --------------------------------------------------------------------------
  
  coord_centre <- st_coordinates(
    
    point_cellule
    
  )
  
  
  coord_centre <- coord_centre[
    
    1,
    
    c("X", "Y"),
    
    drop = FALSE
    
  ]
  
  
  # --------------------------------------------------------------------------
  # 4. RESOLUTION DU RASTER
  # --------------------------------------------------------------------------
  
  resolution_xy <- terra::res(
    
    grille_reference
    
  )
  
  
  demi_resolution_x <- resolution_xy[1] / 2
  
  demi_resolution_y <- resolution_xy[2] / 2
  
  
  # --------------------------------------------------------------------------
  # 5. DISTANCE REELLE ENTRE LE POINT ET LA CELLULE
  # --------------------------------------------------------------------------
  
  distance_x <- max(
    
    abs(
      
      coord_point[1, "X"] -
        coord_centre[1, "X"]
      
    ) -
      demi_resolution_x,
    
    0
    
  )
  
  
  distance_y <- max(
    
    abs(
      
      coord_point[1, "Y"] -
        coord_centre[1, "Y"]
      
    ) -
      demi_resolution_y,
    
    0
    
  )
  
  
  distance_reelle <- sqrt(
    
    distance_x^2 +
      distance_y^2
    
  )
  
  
  # --------------------------------------------------------------------------
  # 6. DISTANCE AU CENTRE DE LA CELLULE
  # --------------------------------------------------------------------------
  
  distance_centre <- sqrt(
    
    (
      coord_point[1, "X"] -
        coord_centre[1, "X"]
    )^2 +
      
      (
        coord_point[1, "Y"] -
          coord_centre[1, "Y"]
      )^2
    
  )
  
  
  # --------------------------------------------------------------------------
  # 7. RECUPERATION DE LA COMPOSANTE CONNECTEE
  # --------------------------------------------------------------------------
  #
  # IMPORTANT :
  #
  # On récupère d'abord toutes les valeurs du raster sous forme de vecteur,
  # puis on sélectionne EXACTEMENT la position correspondant au numéro
  # de cellule.
  #
  # Cela garantit que composante_id contient une seule valeur.
  
  valeurs_composantes <- terra::values(
    
    composantes,
    
    mat = FALSE
    
  )
  
  
  composante_id <- as.integer(
    
    valeurs_composantes[
      cellule_proche
    ]
    
  )
  
  
  # --------------------------------------------------------------------------
  # 8. VERIFICATION DE SECURITE
  # --------------------------------------------------------------------------
  
  if (
    length(composante_id) != 1 ||
    is.na(composante_id)
  ) {
    
    stop(
      paste0(
        "ERREUR : aucune composante valide trouvée pour la cellule ",
        cellule_proche,
        "."
      )
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # 9. TEST DE LA DISTANCE MAXIMALE
  # --------------------------------------------------------------------------
  
  dans_distance_max <- as.logical(
    
    distance_reelle <= distance_max
    
  )
  
  
  # --------------------------------------------------------------------------
  # 10. RETOUR
  # --------------------------------------------------------------------------
  #
  # Toutes les colonnes doivent obligatoirement avoir UNE seule valeur.
  
  resultat <- data.frame(
    
    cellule =
      as.integer(cellule_proche),
    
    distance_snap_m =
      as.numeric(distance_reelle),
    
    distance_centre_m =
      as.numeric(distance_centre),
    
    composante_id =
      as.integer(composante_id),
    
    dans_distance_max =
      as.logical(dans_distance_max)
    
  )
  
  
  return(
    resultat
  )
  
}


raster_accessible <- terra::ifel(
  
  !is.na(grille_cout),
  
  1,
  
  NA
  
)


composantes <- terra::patches(
  
  raster_accessible,
  
  directions = NOMBRE_VOISINS,
  
  zeroAsNA = TRUE,
  
  allowGaps = FALSE
  
)


nombre_composantes <- terra::global(
  
  composantes,
  
  "max",
  
  na.rm = TRUE
  
)[1, 1]


cat("====================================================\n")
cat("VERIFICATION DES COMPOSANTES CONNECTEES\n")
cat("====================================================\n\n")


cat(
  "Nombre de composantes accessibles :",
  nombre_composantes,
  "\n\n"
)



# ==============================================================================
# TEST COMPLET - PREMIERE PAIRE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. IDENTIFICATION DE LA PREMIERE PAIRE
# ------------------------------------------------------------------------------

id_a <- paires_patches$patch_a[1]

id_b <- paires_patches$patch_b[1]


cat(
  "Test de la paire :",
  id_a,
  "-",
  id_b,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 2. RECUPERATION DES DEUX PATCHES
# ------------------------------------------------------------------------------

patch_a <- patches[
  patches$patch_id == id_a,
]

patch_b <- patches[
  patches$patch_id == id_b,
]


# Verification

cat(
  "Nombre de géométries trouvées pour patch A :",
  nrow(patch_a),
  "\n"
)

cat(
  "Nombre de géométries trouvées pour patch B :",
  nrow(patch_b),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 3. CALCUL DES POINTS DE BORDURE
# ------------------------------------------------------------------------------

points_test <- obtenir_points_bordure(
  
  patch_a,
  
  patch_b
  
)


# Vérification des objets

cat(
  "Point de départ créé :",
  !is.null(points_test$depart),
  "\n"
)

cat(
  "Point d'arrivée créé :",
  !is.null(points_test$arrivee),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 4. TEST DU POINT DE DEPART
# ------------------------------------------------------------------------------

cat(
  "Test du point de départ...\n"
)


resultat_test_depart <- trouver_cellule_accessible(
  
  point_sf = points_test$depart,
  
  points_accessibles = points_accessibles,
  
  composantes = composantes,
  
  grille_reference = grille_cout,
  
  distance_max = DISTANCE_MAX_SNAP
  
)


cat(
  "Point de départ traité avec succès.\n\n"
)


# ------------------------------------------------------------------------------
# 5. TEST DU POINT D'ARRIVEE
# ------------------------------------------------------------------------------

cat(
  "Test du point d'arrivée...\n"
)


resultat_test_arrivee <- trouver_cellule_accessible(
  
  point_sf = points_test$arrivee,
  
  points_accessibles = points_accessibles,
  
  composantes = composantes,
  
  grille_reference = grille_cout,
  
  distance_max = DISTANCE_MAX_SNAP
  
)


cat(
  "Point d'arrivée traité avec succès.\n\n"
)


# ------------------------------------------------------------------------------
# 6. AFFICHAGE DES RESULTATS
# ------------------------------------------------------------------------------

cat(
  "====================================================\n"
)

cat(
  "RESULTAT DEPART\n"
)

cat(
  "====================================================\n"
)

print(
  resultat_test_depart
)


cat(
  "\n"
)

cat(
  "====================================================\n"
)

cat(
  "RESULTAT ARRIVEE\n"
)

cat(
  "====================================================\n"
)

print(
  resultat_test_arrivee
)


# ------------------------------------------------------------------------------
# 7. CONTROLE DES LONGUEURS
# ------------------------------------------------------------------------------

cat(
  "\n--- CONTROLE DES LONGUEURS ---\n\n"
)


cat(
  "Longueur cellule départ :",
  length(resultat_test_depart$cellule),
  "\n"
)

cat(
  "Longueur composante départ :",
  length(resultat_test_depart$composante_id),
  "\n"
)

cat(
  "Nombre de lignes départ :",
  nrow(resultat_test_depart),
  "\n\n"
)


cat(
  "Longueur cellule arrivée :",
  length(resultat_test_arrivee$cellule),
  "\n"
)

cat(
  "Longueur composante arrivée :",
  length(resultat_test_arrivee$composante_id),
  "\n"
)

cat(
  "Nombre de lignes arrivée :",
  nrow(resultat_test_arrivee),
  "\n"
)


cat(
  "\n====================================================\n"
)

cat(
  "TEST TERMINE\n"
)

cat(
  "====================================================\n"
)



# ==============================================================================
# 9. CALCUL DES POINTS DE BORDURE ET SNAP POUR LES 13 PAIRES
# ==============================================================================

cat("====================================================\n")
cat("PREPARATION DES POINTS DE DEPART ET D'ARRIVEE\n")
cat("====================================================\n\n")


resultats_points <- data.frame(
  
  patch_a = integer(0),
  
  patch_b = integer(0),
  
  cellule_depart = integer(0),
  
  cellule_arrivee = integer(0),
  
  distance_depart_m = numeric(0),
  
  distance_arrivee_m = numeric(0),
  
  composante_depart = integer(0),
  
  composante_arrivee = integer(0),
  
  connectable = logical(0)
  
)


liste_points_depart_snap <- vector(
  
  "list",
  
  nrow(paires_patches)
  
)


liste_points_arrivee_snap <- vector(
  
  "list",
  
  nrow(paires_patches)
  
)


for (i in seq_len(nrow(paires_patches))) {
  
  
  cat(
    
    "Preparation de la paire ",
    
    paires_patches$patch_a[i],
    
    " - ",
    
    paires_patches$patch_b[i],
    
    "\n",
    
    sep = ""
    
  )
  
  
  id_a <- paires_patches$patch_a[i]
  
  id_b <- paires_patches$patch_b[i]
  
  
  # --------------------------------------------------------------------------
  # RECUPERATION DES PATCHES
  # --------------------------------------------------------------------------
  
  patch_a <- patches[
    
    patches$patch_id == id_a,
    
  ]
  
  
  patch_b <- patches[
    
    patches$patch_id == id_b,
    
  ]
  
  
  if (nrow(patch_a) != 1) {
    
    stop(
      
      "ERREUR : patch ",
      
      id_a,
      
      " introuvable ou présent plusieurs fois."
      
    )
    
  }
  
  
  if (nrow(patch_b) != 1) {
    
    stop(
      
      "ERREUR : patch ",
      
      id_b,
      
      " introuvable ou présent plusieurs fois."
      
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # POINTS DE BORDURE
  # --------------------------------------------------------------------------
  
  points_bordure_paire <- obtenir_points_bordure(
    
    patch_a = patch_a,
    
    patch_b = patch_b
    
  )
  
  
  point_depart_bordure <- points_bordure_paire$depart
  
  point_arrivee_bordure <- points_bordure_paire$arrivee
  
  
  # --------------------------------------------------------------------------
  # SNAP DU POINT DE DEPART
  # --------------------------------------------------------------------------
  
  resultat_depart <- trouver_cellule_accessible(
    
    point_sf = point_depart_bordure,
    
    points_accessibles = points_accessibles,
    
    composantes = composantes,
    
    grille_reference = grille_cout,
    
    distance_max = DISTANCE_MAX_SNAP
    
  )
  
  
  # --------------------------------------------------------------------------
  # SNAP DU POINT D'ARRIVEE
  # --------------------------------------------------------------------------
  
  resultat_arrivee <- trouver_cellule_accessible(
    
    point_sf = point_arrivee_bordure,
    
    points_accessibles = points_accessibles,
    
    composantes = composantes,
    
    grille_reference = grille_cout,
    
    distance_max = DISTANCE_MAX_SNAP
    
  )
  
  
  # --------------------------------------------------------------------------
  # TEST DE CONNECTIVITE
  # --------------------------------------------------------------------------
  
  connectable <- FALSE
  
  
  if (
    
    isTRUE(
      resultat_depart$dans_distance_max
    ) &&
    
    isTRUE(
      resultat_arrivee$dans_distance_max
    )
    
  ) {
    
    
    connectable <-
      
      resultat_depart$composante_id ==
      resultat_arrivee$composante_id
    
  }
  
  
  # --------------------------------------------------------------------------
  # COORDONNEES DES CENTRES DES CELLULES SNAPPEES
  # --------------------------------------------------------------------------
  
  coord_depart_snap <- terra::xyFromCell(
    
    grille_cout,
    
    resultat_depart$cellule
    
  )
  
  
  coord_arrivee_snap <- terra::xyFromCell(
    
    grille_cout,
    
    resultat_arrivee$cellule
    
  )
  
  
  # --------------------------------------------------------------------------
  # CREATION DES POINTS SNAP
  # --------------------------------------------------------------------------
  
  point_depart_snap <- st_sf(
    
    patch_a = id_a,
    
    patch_b = id_b,
    
    cellule = resultat_depart$cellule,
    
    geometry = st_sfc(
      
      st_point(
        
        coord_depart_snap[1, ]
        
      ),
      
      crs = CRS_PROJET
      
    )
    
  )
  
  
  point_arrivee_snap <- st_sf(
    
    patch_a = id_a,
    
    patch_b = id_b,
    
    cellule = resultat_arrivee$cellule,
    
    geometry = st_sfc(
      
      st_point(
        
        coord_arrivee_snap[1, ]
        
      ),
      
      crs = CRS_PROJET
      
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # STOCKAGE DES POINTS
  # --------------------------------------------------------------------------
  
  liste_points_depart_snap[[i]] <- point_depart_snap
  
  liste_points_arrivee_snap[[i]] <- point_arrivee_snap
  
  
  # --------------------------------------------------------------------------
  # AJOUT AU TABLEAU
  # --------------------------------------------------------------------------
  
  resultats_points <- rbind(
    
    resultats_points,
    
    data.frame(
      
      patch_a = id_a,
      
      patch_b = id_b,
      
      cellule_depart =
        resultat_depart$cellule,
      
      cellule_arrivee =
        resultat_arrivee$cellule,
      
      distance_depart_m =
        resultat_depart$distance_snap_m,
      
      distance_arrivee_m =
        resultat_arrivee$distance_snap_m,
      
      composante_depart =
        resultat_depart$composante_id,
      
      composante_arrivee =
        resultat_arrivee$composante_id,
      
      connectable =
        connectable
      
    )
    
  )
  
}


cat("\n")


print(
  
  resultats_points
  
)



# ==============================================================================
# 10. CONTROLE FINAL DE CONNECTIVITE
# ==============================================================================

nombre_connectables <- sum(
  
  resultats_points$connectable
  
)


nombre_non_connectables <- nrow(
  
  resultats_points
  
) - nombre_connectables


cat("\n")
cat("====================================================\n")
cat("CONTROLE FINAL DE CONNECTIVITE\n")
cat("====================================================\n\n")


cat(
  
  "Nombre total de paires :",
  
  nrow(resultats_points),
  
  "\n"
  
)


cat(
  
  "Nombre de paires connectables :",
  
  nombre_connectables,
  
  "\n"
  
)


cat(
  
  "Nombre de paires non connectables :",
  
  nombre_non_connectables,
  
  "\n\n"
  
)


if (nombre_non_connectables > 0) {
  
  stop(
    
    "ERREUR : certaines paires ne sont pas connectables.\n\n",
    
    "Le calcul des chemins est volontairement interrompu afin d'éviter ",
    "la production de chemins incomplets ou géométriquement invalides."
    
  )
  
}


cat(
  
  "SUCCES : toutes les paires sont connectables.\n\n"
  
)



# ==============================================================================
# 11. CREATION DE LA SURFACE DE CONDUCTANCE
# ==============================================================================
#
# grille_cout est une surface de COUT :
#
# faible coût = déplacement préférable
# coût élevé = déplacement défavorable
#
# leastcostpath travaille avec une CONDUCTANCE :
#
# conductance élevée = déplacement facile
# conductance faible = déplacement difficile
#
# Nous transformons donc :
#
# conductance = 1 / coût
#
# Les cellules interdites restent NA.
# ==============================================================================

cat("====================================================\n")
cat("CREATION DE LA SURFACE DE CONDUCTANCE\n")
cat("====================================================\n\n")


grille_conductance <- terra::ifel(
  
  !is.na(grille_cout),
  
  1 / grille_cout,
  
  NA
  
)


valeurs_conductance <- terra::values(
  
  grille_conductance,
  
  mat = FALSE
  
)


if (
  
  any(
    
    !is.na(valeurs_conductance) &
    (
      !is.finite(valeurs_conductance) |
      valeurs_conductance <= 0
    )
    
  )
  
) {
  
  stop(
    
    "ERREUR : la surface de conductance contient des valeurs ",
    "non valides."
    
  )
  
}


cat(
  
  "Surface de conductance créée.\n\n"
  
)



# ==============================================================================
# 12. CREATION DE LA MATRICE DE CONDUCTANCE
# ==============================================================================
#
# ATTENTION :
#
# Cette étape peut être relativement longue et demander beaucoup de mémoire.
#
# Elle ne doit être effectuée qu'UNE SEULE FOIS.
#
# La matrice sera ensuite réutilisée pour les 13 chemins.
# ==============================================================================

cat("====================================================\n")
cat("CREATION DE LA MATRICE DE CONDUCTANCE\n")
cat("====================================================\n\n")


cat(
  
  "Cette étape peut prendre du temps selon la taille de la grille.\n",
  
  "La matrice sera créée une seule fois pour les 13 chemins.\n\n"
  
)


surface_conductance <- leastcostpath::create_cs(
  
  x = grille_conductance,
  
  neighbours = NOMBRE_VOISINS
  
)


cat(
  
  "Matrice de conductance créée.\n\n"
  
)



# ==============================================================================
# 13. CREATION DU GRAPHE UNE SEULE FOIS
# ==============================================================================
#
# On évite de reconstruire le graphe pour chaque paire.
#
# Cela est beaucoup plus efficace pour les 13 calculs.
# ==============================================================================

cat("====================================================\n")
cat("CREATION DU GRAPHE DE DEPLACEMENT\n")
cat("====================================================\n\n")


cm_graph <- igraph::graph_from_adjacency_matrix(
  
  surface_conductance$conductanceMatrix,
  
  mode = "directed",
  
  weighted = TRUE
  
)


igraph::E(
  
  cm_graph
  
)$weight <-
  
  1 /
  igraph::E(
    cm_graph
  )$weight


cat(
  
  "Graphe de déplacement créé.\n\n"
  
)



# ==============================================================================
# 14. FONCTION : CALCUL D'UN CHEMIN DE MOINDRE COUT
# ==============================================================================
#
# Cette fonction :
#
# 1. utilise les deux cellules accessibles déjà validées ;
# 2. calcule le chemin de moindre coût avec Dijkstra ;
# 3. transforme la succession de cellules en ligne sf ;
# 4. calcule le coût accumulé ;
# 5. vérifie que toutes les cellules utilisées sont accessibles.
# ------------------------------------------------------------------------------

calculer_chemin_moindre_cout <- function(
    
  cellule_depart,
  
  cellule_arrivee,
  
  grille_cout,
  
  cm_graph
  
) {
  
  # --------------------------------------------------------------------------
  # VERIFICATION DES CELLULES
  # --------------------------------------------------------------------------
  
  # On récupère directement les valeurs du raster dans l'ordre
  # exact des cellules raster.
  
  valeurs_grille <- terra::values(
    grille_cout,
    mat = FALSE
  )
  
  
  # Valeur exacte de la cellule de départ
  
  valeur_depart <- valeurs_grille[
    cellule_depart
  ]
  
  
  # Valeur exacte de la cellule d'arrivée
  
  valeur_arrivee <- valeurs_grille[
    cellule_arrivee
  ]
  
  
  # --------------------------------------------------------------------------
  # VERIFICATION DE LA CELLULE DE DEPART
  # --------------------------------------------------------------------------
  
  if (
    length(cellule_depart) != 1 ||
    is.na(cellule_depart) ||
    cellule_depart < 1 ||
    cellule_depart > terra::ncell(grille_cout)
  ) {
    
    stop(
      "ERREUR : numéro de cellule de départ invalide : ",
      cellule_depart
    )
  }
  
  
  if (
    length(valeur_depart) != 1 ||
    is.na(valeur_depart) ||
    !is.finite(valeur_depart) ||
    valeur_depart <= 0
  ) {
    
    stop(
      "ERREUR : la cellule de départ ",
      cellule_depart,
      " possède une valeur invalide ou interdite : ",
      valeur_depart
    )
  }
  
  
 

  
  # --------------------------------------------------------------------------
  # VERIFICATION DE LA CORRESPONDANCE CELLULE RASTER / GRAPHE
  # --------------------------------------------------------------------------
  
  nombre_sommets <- igraph::vcount(
    cm_graph
  )
  
  
  if (
    cellule_depart < 1 ||
    cellule_depart > nombre_sommets
  ) {
    
    stop(
      "ERREUR : la cellule de départ ",
      cellule_depart,
      " n'existe pas dans le graphe. ",
      "Le graphe contient seulement ",
      nombre_sommets,
      " sommets."
    )
  }
  
  
  if (
    cellule_arrivee < 1 ||
    cellule_arrivee > nombre_sommets
  ) {
    
    stop(
      "ERREUR : la cellule d'arrivée ",
      cellule_arrivee,
      " n'existe pas dans le graphe. ",
      "Le graphe contient seulement ",
      nombre_sommets,
      " sommets."
    )
  }
  
  
 
  
  # --------------------------------------------------------------------------
  # CALCUL DU CHEMIN
  # --------------------------------------------------------------------------
  
  resultat_chemin <- igraph::shortest_paths(
    
    graph = cm_graph,
    
    from = cellule_depart,
    
    to = cellule_arrivee,
    
    mode = "out",
    
    weights = igraph::E(cm_graph)$weight,
    
    output = "vpath",
    
    algorithm = "dijkstra"
    
  )
  
  
  cellules_chemin <- as.integer(
    resultat_chemin$vpath[[1]]
  )
  
  if (
    length(cellules_chemin) == 0 ||
    anyNA(cellules_chemin)
  ) {
    
    return(NULL)
    
  }
  
  if (
    cellules_chemin[1] != cellule_depart ||
    cellules_chemin[length(cellules_chemin)] != cellule_arrivee
  ) {
    
    stop(
      "ERREUR CRITIQUE : le chemin calculé ne commence ou ne termine ",
      "pas sur les cellules demandées."
    )
  }
  # --------------------------------------------------------------------------
  # VERIFICATION D'EXISTENCE
  # --------------------------------------------------------------------------
  
  if (
    length(cellules_chemin) == 0 ||
    anyNA(cellules_chemin)
  ) {
    
    return(NULL)
    
  }
  
  # --------------------------------------------------------------------------
  # VERIFICATION DES CELLULES UTILISEES
  # --------------------------------------------------------------------------
  
  valeurs_chemin <- terra::values(
    grille_cout,
    mat = FALSE
  )[
    cellules_chemin
  ]
  
  if (any(is.na(valeurs_chemin))) {
    
    stop(
      
      "ERREUR CRITIQUE : le chemin contient une cellule interdite."
      
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # COORDONNEES DES CELLULES DU CHEMIN
  # --------------------------------------------------------------------------
  
  coordonnees_chemin <- terra::xyFromCell(
    
    grille_cout,
    
    cellules_chemin
    
  )
  
  
  # --------------------------------------------------------------------------
  # CREATION DE LA GEOMETRIE
  # --------------------------------------------------------------------------
  
  geometrie_chemin <- st_linestring(
    
    coordonnees_chemin
    
  )
  
  
  # --------------------------------------------------------------------------
  # CALCUL DU COUT ACCUMULE
  # --------------------------------------------------------------------------
  
  cout_accumule <- as.numeric(
    
    igraph::distances(
      
      graph = cm_graph,
      
      v = cellule_depart,
      
      to = cellule_arrivee,
      
      mode = "out",
      
      weights = igraph::E(cm_graph)$weight,
      
      algorithm = "dijkstra"
      
    )[1, 1]
    
  )
  
  
  # --------------------------------------------------------------------------
  # RESULTAT
  # --------------------------------------------------------------------------
  
  return(
    
    list(
      
      cellules = cellules_chemin,
      
      valeurs_cout = valeurs_chemin,
      
      geometrie = geometrie_chemin,
      
      cout_accumule = cout_accumule
      
    )
    
  )
  
}



# ==============================================================================
# 15. CALCUL DES 13 CHEMINS
# ==============================================================================

cat("====================================================\n")
cat("CALCUL DES CHEMINS DE MOINDRE COUT\n")
cat("====================================================\n\n")


liste_chemins <- vector(
  
  "list",
  
  nrow(paires_patches)
  
)


resultats_statistiques <- data.frame(
  
  patch_a = integer(0),
  
  patch_b = integer(0),
  
  cellule_depart = integer(0),
  
  cellule_arrivee = integer(0),
  
  nombre_cellules = integer(0),
  
  longueur_m = numeric(0),
  
  cout_accumule = numeric(0),
  
  cout_moyen = numeric(0),
  
  cout_min = numeric(0),
  
  cout_max = numeric(0),
  
  cellules_interdites = integer(0)
  
)


for (i in seq_len(nrow(paires_patches))) {
  
  
  id_a <- paires_patches$patch_a[i]
  
  id_b <- paires_patches$patch_b[i]
  
  
  cat("\n")
  cat("----------------------------------------------------\n")
  cat(
    
    "CALCUL DU CHEMIN ",
    
    i,
    
    " / ",
    
    nrow(paires_patches),
    
    " : PATCH ",
    
    id_a,
    
    " -> PATCH ",
    
    id_b,
    
    "\n",
    
    sep = ""
    
  )
  cat("----------------------------------------------------\n")
  
  
  cellule_depart <- resultats_points$cellule_depart[i]
  
  cellule_arrivee <- resultats_points$cellule_arrivee[i]
  
  if (
    length(cellule_depart) != 1 ||
    is.na(cellule_depart) ||
    cellule_depart < 1 ||
    cellule_depart > terra::ncell(grille_cout)
  ) {
    
    stop(
      "ERREUR : cellule de départ invalide pour la paire ",
      id_a,
      " - ",
      id_b,
      "."
    )
  }
  
  
  if (
    length(cellule_arrivee) != 1 ||
    is.na(cellule_arrivee) ||
    cellule_arrivee < 1 ||
    cellule_arrivee > terra::ncell(grille_cout)
  ) {
    
    stop(
      "ERREUR : cellule d'arrivée invalide pour la paire ",
      id_a,
      " - ",
      id_b,
      "."
    )
  }
  
  # --------------------------------------------------------------------------
  # CALCUL DU CHEMIN
  # --------------------------------------------------------------------------
  
  resultat_chemin <- calculer_chemin_moindre_cout(
    
    cellule_depart = cellule_depart,
    
    cellule_arrivee = cellule_arrivee,
    
    grille_cout = grille_cout,
    
    cm_graph = cm_graph
    
  )
  
  
  # --------------------------------------------------------------------------
  # VERIFICATION DU RESULTAT
  # --------------------------------------------------------------------------
  
  if (is.null(resultat_chemin)) {
    
    stop(
      
      "ERREUR : aucun chemin n'a pu être calculé pour la paire ",
      
      id_a,
      
      " - ",
      
      id_b
      
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # CREATION DU CHEMIN SF
  # --------------------------------------------------------------------------
  
  chemin_sf <- st_sf(
    
    patch_a = id_a,
    
    patch_b = id_b,
    
    cellule_depart = cellule_depart,
    
    cellule_arrivee = cellule_arrivee,
    
    nombre_cellules =
      length(
        resultat_chemin$cellules
      ),
    
    cout_accumule =
      resultat_chemin$cout_accumule,
    
    geometry = st_sfc(
      
      resultat_chemin$geometrie,
      
      crs = CRS_PROJET
      
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # CALCUL DE LA LONGUEUR
  # --------------------------------------------------------------------------
  
  longueur_m <- as.numeric(
    
    st_length(
      chemin_sf
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # CONTROLE DES CELLULES INTERDITES
  # --------------------------------------------------------------------------
  
  nombre_cellules_interdites <- sum(
    
    is.na(
      resultat_chemin$valeurs_cout
    )
    
  )
  
  
  if (nombre_cellules_interdites > 0) {
    
    stop(
      
      "ERREUR CRITIQUE : le chemin ",
      
      id_a,
      
      " - ",
      
      id_b,
      
      " utilise ",
      
      nombre_cellules_interdites,
      
      " cellule(s) interdite(s)."
      
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # STATISTIQUES DE COUT
  # --------------------------------------------------------------------------
  
  cout_moyen <- mean(
    
    resultat_chemin$valeurs_cout
    
  )
  
  
  cout_min <- min(
    
    resultat_chemin$valeurs_cout
    
  )
  
  
  cout_max <- max(
    
    resultat_chemin$valeurs_cout
    
  )
  
  
  # --------------------------------------------------------------------------
  # AJOUT DES STATISTIQUES AU CHEMIN
  # --------------------------------------------------------------------------
  
  chemin_sf$longueur_m <- longueur_m
  
  chemin_sf$cout_moyen <- cout_moyen
  
  chemin_sf$cout_min <- cout_min
  
  chemin_sf$cout_max <- cout_max
  
  chemin_sf$cellules_interdites <- nombre_cellules_interdites
  
  
  # --------------------------------------------------------------------------
  # STOCKAGE DU CHEMIN
  # --------------------------------------------------------------------------
  
  liste_chemins[[i]] <- chemin_sf
  
  
  # --------------------------------------------------------------------------
  # AJOUT AU TABLEAU DE STATISTIQUES
  # --------------------------------------------------------------------------
  
  resultats_statistiques <- rbind(
    
    resultats_statistiques,
    
    data.frame(
      
      patch_a = id_a,
      
      patch_b = id_b,
      
      cellule_depart = cellule_depart,
      
      cellule_arrivee = cellule_arrivee,
      
      nombre_cellules =
        length(
          resultat_chemin$cellules
        ),
      
      longueur_m = longueur_m,
      
      cout_accumule =
        resultat_chemin$cout_accumule,
      
      cout_moyen = cout_moyen,
      
      cout_min = cout_min,
      
      cout_max = cout_max,
      
      cellules_interdites =
        nombre_cellules_interdites
      
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # AFFICHAGE DU RESULTAT
  # --------------------------------------------------------------------------
  
  cat(
    
    "Nombre de cellules :",
    
    length(
      resultat_chemin$cellules
    ),
    
    "\n"
    
  )
  
  
  cat(
    
    "Longueur du chemin (m) :",
    
    round(
      longueur_m,
      2
    ),
    
    "\n"
    
  )
  
  
  cat(
    
    "Cout accumule :",
    
    round(
      resultat_chemin$cout_accumule,
      4
    ),
    
    "\n"
    
  )
  
  
  cat(
    
    "Cout moyen :",
    
    round(
      cout_moyen,
      4
    ),
    
    "\n"
    
  )
  
  
  cat(
    
    "Cellules interdites :",
    
    nombre_cellules_interdites,
    
    "\n"
    
  )
  
}



# ==============================================================================
# 16. CREATION DE LA COUCHE UNIQUE DES 13 CHEMINS
# ==============================================================================

cat("\n")
cat("====================================================\n")
cat("CREATION DE LA COUCHE UNIQUE DES CHEMINS\n")
cat("====================================================\n\n")


chemins_finaux <- do.call(
  
  rbind,
  
  liste_chemins
  
)


rownames(
  
  chemins_finaux
  
) <- NULL


cat(
  
  "Nombre total de chemins créés :",
  
  nrow(chemins_finaux),
  
  "\n\n"
)


if (nrow(chemins_finaux) != nrow(paires_patches)) {
  
  stop(
    
    "ERREUR : le nombre de chemins créés ne correspond pas ",
    "au nombre de paires."
    
  )
  
}



# ==============================================================================
# 17. CONTROLE GLOBAL DES CHEMINS
# ==============================================================================

cat("====================================================\n")
cat("CONTROLE GLOBAL DES CHEMINS\n")
cat("====================================================\n\n")


nombre_chemins_invalides <- sum(
  
  chemins_finaux$cellules_interdites > 0
  
)


cat(
  
  "Nombre de chemins utilisant une cellule interdite :",
  
  nombre_chemins_invalides,
  
  "\n"
  
)


if (nombre_chemins_invalides > 0) {
  
  stop(
    
    "ERREUR CRITIQUE : au moins un chemin utilise des cellules interdites."
    
  )
  
}


cat(
  
  "SUCCES : tous les chemins utilisent uniquement des cellules accessibles.\n\n"
  
)

# ==============================================================================
# 17B. CRÉATION DES CORRIDORS DE 6 MÈTRES DE LARGEUR
# ==============================================================================

# IMPORTANT :
#
# Le modèle de moindre coût calcule une ligne centrale.
# Cette ligne représente l'axe optimal du déplacement.
#
# Pour obtenir une emprise spatiale réaliste et applicable sur le terrain,
# chaque chemin est transformé en corridor de 6 mètres de largeur totale.
#
# 6 m de largeur totale =
# 3 m de chaque côté de la ligne centrale.
#
# La ligne centrale "chemins_finaux" est conservée.
# Le corridor de 6 m est créé dans un nouvel objet :
# "chemins_6m".
#
# Cela permet de conserver séparément :
# - le chemin de moindre coût original ;
# - son emprise spatiale de 6 m de largeur.

LARGEUR_CHEMIN_M <- 6

RAYON_CHEMIN_M <- LARGEUR_CHEMIN_M / 2


cat("====================================================\n")
cat("CREATION DES CORRIDORS DE 6 M\n")
cat("====================================================\n\n")


# Vérification de sécurité

if (!inherits(chemins_finaux, "sf")) {
  
  stop(
    "ERREUR : chemins_finaux doit être un objet sf."
  )
  
}


if (!all(
  sf::st_geometry_type(chemins_finaux) %in%
  c("LINESTRING", "MULTILINESTRING")
)) {
  
  stop(
    "ERREUR : chemins_finaux doit contenir des géométries de type ",
    "LINESTRING ou MULTILINESTRING."
  )
  
}


# --------------------------------------------------------------------------
# CREATION DU BUFFER DE 3 M DE CHAQUE COTE
# --------------------------------------------------------------------------

chemins_6m <- sf::st_buffer(
  
  chemins_finaux,
  
  dist = RAYON_CHEMIN_M
  
)


# --------------------------------------------------------------------------
# CONTROLE DE LA LARGEUR DEMANDEE
# --------------------------------------------------------------------------

cat(
  "Largeur totale des chemins :",
  LARGEUR_CHEMIN_M,
  "m\n"
)

cat(
  "Distance de chaque côté de la ligne centrale :",
  RAYON_CHEMIN_M,
  "m\n"
)

cat(
  "Nombre de corridors de 6 m créés :",
  nrow(chemins_6m),
  "\n\n"
)


# --------------------------------------------------------------------------
# CONTROLE DES GEOMETRIES
# --------------------------------------------------------------------------

if (nrow(chemins_6m) != nrow(chemins_finaux)) {
  
  stop(
    "ERREUR : le nombre de corridors de 6 m ne correspond pas ",
    "au nombre de chemins de moindre coût."
  )
  
}


cat(
  "SUCCES : les 13 chemins disposent maintenant d'une emprise ",
  "spatiale de 6 m de largeur.\n\n"
)

# ==============================================================================
# 18. SAUVEGARDE INDIVIDUELLE DES 13 CHEMINS
# ==============================================================================

cat("====================================================\n")
cat("SAUVEGARDE INDIVIDUELLE DES CHEMINS\n")
cat("====================================================\n\n")


for (i in seq_len(nrow(chemins_finaux))) {
  
  
  id_a <- chemins_finaux$patch_a[i]
  
  id_b <- chemins_finaux$patch_b[i]
  
  
  nom_fichier <- paste0(
    
    "chemin_patch_",
    
    id_a,
    
    "_vers_",
    
    id_b,
    
    ".gpkg"
    
  )
  
  
  chemin_fichier <- file.path(
    
    DOSSIER_SORTIE_CHEMINS,
    
    nom_fichier
    
  )
  
  
  # Suppression de l'ancien fichier si nécessaire
  
  if (file.exists(chemin_fichier)) {
    
    file.remove(
      chemin_fichier
    )
    
  }
  
  
  st_write(
    
    chemins_finaux[i, ],
    
    chemin_fichier,
    
    quiet = TRUE
    
  )
  
  
  cat(
    
    "Chemin sauvegarde : ",
    
    nom_fichier,
    
    "\n",
    
    sep = ""
    
  )
  
}


cat("\n")



# ==============================================================================
# 19. SAUVEGARDE DE LA COUCHE COMPLETE
# ==============================================================================

fichier_chemins_complet <- file.path(
  
  DOSSIER_SORTIE_CHEMINS,
  
  "chemins_finaux_13_paires.gpkg"
  
)


if (file.exists(fichier_chemins_complet)) {
  
  file.remove(
    fichier_chemins_complet
  )
  
}


st_write(
  
  chemins_finaux,
  
  fichier_chemins_complet,
  
  layer = "chemins_finaux",
  
  quiet = TRUE
  
)


cat(
  
  "Couche complete sauvegardee :\n",
  
  fichier_chemins_complet,
  
  "\n\n"
)

# ==============================================================================
# 19B. SAUVEGARDE DE LA COUCHE DES CORRIDORS DE 6 M
# ==============================================================================

fichier_chemins_6m <- file.path(
  
  DOSSIER_SORTIE_CHEMINS,
  
  "chemins_6m_13_paires.gpkg"
  
)


if (file.exists(fichier_chemins_6m)) {
  
  file.remove(
    fichier_chemins_6m
  )
  
}


st_write(
  
  chemins_6m,
  
  fichier_chemins_6m,
  
  layer = "chemins_6m",
  
  quiet = TRUE
  
)


cat(
  "Couche des corridors de 6 m sauvegardee :\n",
  fichier_chemins_6m,
  "\n\n"
)

# ==============================================================================
# 20. SAUVEGARDE DES STATISTIQUES
# ==============================================================================

cat("====================================================\n")
cat("SAUVEGARDE DES STATISTIQUES\n")
cat("====================================================\n\n")


fichier_statistiques <- file.path(
  
  DOSSIER_SORTIE_CHEMINS,
  
  "statistiques_13_chemins.csv"
  
)


write.csv(
  
  resultats_statistiques,
  
  fichier_statistiques,
  
  row.names = FALSE
  
)


print(
  
  resultats_statistiques
  
)


cat(
  
  "\nStatistiques sauvegardees dans :\n",
  
  fichier_statistiques,
  
  "\n\n"
)



# ==============================================================================
# 21. CARTE GLOBALE DES 13 CHEMINS
# ==============================================================================
#
# La carte utilise terra pour éviter de convertir l'ensemble du raster
# en un énorme data.frame.
# ------------------------------------------------------------------------------

cat("====================================================\n")
cat("CREATION DE LA CARTE GLOBALE\n")
cat("====================================================\n\n")


fichier_carte_globale <- file.path(
  
  DOSSIER_SORTIE_CHEMINS,
  
  "carte_globale_13_chemins.png"
  
)


png(
  
  filename = fichier_carte_globale,
  
  width = 2000,
  
  height = 1800,
  
  res = 200
  
)


terra::plot(
  
  grille_cout,
  
  main =
    "Surface de cout et 13 chemins de moindre cout"
  
)


# Affichage de l'emprise réelle des corridors de 6 m

terra::polys(
  
  terra::vect(
    chemins_6m
  )
  
)


# Affichage de l'axe central du chemin de moindre coût

terra::lines(
  
  terra::vect(
    chemins_finaux
  ),
  
  lwd = 2
  
)

terra::lines(
  
  terra::vect(
    patches
  ),
  
  lwd = 1
  
)


dev.off()


cat(
  
  "Carte globale sauvegardee :\n",
  
  fichier_carte_globale,
  
  "\n\n"
)



# ==============================================================================
# 22. CARTES INDIVIDUELLES DES CHEMINS
# ==============================================================================

cat("====================================================\n")
cat("CREATION DES CARTES INDIVIDUELLES\n")
cat("====================================================\n\n")


for (i in seq_len(nrow(chemins_finaux))) {
  
  
  id_a <- chemins_finaux$patch_a[i]
  
  id_b <- chemins_finaux$patch_b[i]
  
  
  fichier_carte <- file.path(
    
    DOSSIER_SORTIE_CHEMINS,
    
    paste0(
      
      "carte_chemin_",
      
      id_a,
      
      "_",
      
      id_b,
      
      ".png"
      
    )
    
  )
  
  
  png(
    
    filename = fichier_carte,
    
    width = 1600,
    
    height = 1400,
    
    res = 180
    
  )
  
  
  terra::plot(
    
    grille_cout,
    
    main = paste(
      
      "Chemin de moindre cout : patch",
      
      id_a,
      
      "vers patch",
      
      id_b
      
    )
    
  )
  
  
  terra::lines(
    
    terra::vect(
      chemins_finaux[i, ]
    ),
    
    lwd = 3
    
  )
  
  
  terra::lines(
    
    terra::vect(
      patches[
        patches$patch_id %in% c(id_a, id_b),
      ]
    ),
    
    lwd = 2
    
  )
  
  
  dev.off()
  
  
  cat(
    
    "Carte créée : chemin ",
    
    id_a,
    
    " - ",
    
    id_b,
    
    "\n",
    
    sep = ""
    
  )
  
}



# ==============================================================================
# 23. CARTE DE CONTROLE AFFICHÉE DANS RSTUDIO
# ==============================================================================

terra::plot(
  
  grille_cout,
  
  main =
    "Controle final - 13 chemins de moindre cout"
  
)


terra::lines(
  
  terra::vect(
    chemins_finaux
  ),
  
  col = "chartreuse",
  
  lwd = 3
  
)

terra::lines(
  
  terra::vect(
    patches
  ),
  
  lwd = 1
  
)



# ==============================================================================
# 24. RESUME FINAL
# ==============================================================================

cat("\n")
cat("====================================================\n")
cat("RESUME FINAL DU SCRIPT 03\n")
cat("====================================================\n\n")


cat(
  
  "Nombre de paires demandees :",
  
  nrow(paires_patches),
  
  "\n"
  
)


cat(
  
  "Nombre de paires connectables :",
  
  nombre_connectables,
  
  "\n"
  
)


cat(
  
  "Nombre de chemins calcules :",
  
  nrow(chemins_finaux),
  
  "\n"
  
)


cat(
  
  "Nombre de chemins utilisant des cellules interdites :",
  
  nombre_chemins_invalides,
  
  "\n"
  
)


cat(
  
  "Dossier des resultats :\n",
  
  DOSSIER_SORTIE_CHEMINS,
  
  "\n\n"
)


cat(
  
  "====================================================\n"
)


cat(
  
  "SCRIPT 03 TERMINE AVEC SUCCES\n"
)


cat(
  
  "====================================================\n"
)


cat(
  
  "\nOBJETS PRINCIPAUX CREES :\n\n"
)


cat(
  
  "- chemins_finaux\n"
)


cat(
  
  "- resultats_statistiques\n"
)


cat(
  
  "- resultats_points\n"
)


cat(
  
  "- surface_conductance\n"
)


cat(
  
  "- cm_graph\n"
)


cat(
  
  "\n"
)
