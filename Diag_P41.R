# ==============================================================================
# DIAGNOSTIC 3 - ANALYSE SPECIFIQUE DES POINTS DE DEPART DU PATCH 41
# ==============================================================================
#
# Objectif :
#
# Comprendre pourquoi les paires :
#
#   41 - 12
#   41 - 13
#
# ne trouvent pas de cellule accessible pour leur point de départ.
#
# Le diagnostic recherche la cellule accessible la plus proche SANS appliquer
# immédiatement la limite de 50 m.
#
# Cela permet de mesurer la distance réelle entre le point de bordure du
# patch 41 et la surface accessible.
# ==============================================================================


cat("\n")
cat("====================================================\n")
cat("DIAGNOSTIC 3 - ANALYSE DU PATCH 41\n")
cat("====================================================\n\n")


# ------------------------------------------------------------------------------
# 1. VERIFICATION DES OBJETS NECESSAIRES
# ------------------------------------------------------------------------------

objets_necessaires <- c(
  "grille_cout",
  "patches",
  "paires_patches",
  "points_accessibles",
  "CRS_PROJET"
)


objets_manquants <- objets_necessaires[
  !objets_necessaires %in% ls()
]


if (length(objets_manquants) > 0) {
  
  stop(
    "ERREUR : objets manquants : ",
    paste(objets_manquants, collapse = ", ")
  )
  
}


# ------------------------------------------------------------------------------
# 2. SELECTION DES PAIRES CONCERNANT LE PATCH 41
# ------------------------------------------------------------------------------

paires_patch_41 <- paires_patches[
  
  paires_patches$patch_A == 41 |
    paires_patches$patch_B == 41,
  
]


cat(
  "Nombre de paires contenant le patch 41 :",
  nrow(paires_patch_41),
  "\n\n"
)


print(
  paires_patch_41
)


# ------------------------------------------------------------------------------
# 3. FONCTION POUR OBTENIR LES DEUX POINTS DE BORDURE
# ------------------------------------------------------------------------------

obtenir_points_bordure_diagnostic <- function(
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
      "Impossible de calculer les points de bordure."
    )
    
  }
  
  
  point_a <- st_sfc(
    
    st_point(
      coordonnees_ligne[
        1,
        c("X", "Y")
      ]
    ),
    
    crs = CRS_PROJET
    
  )
  
  
  point_b <- st_sfc(
    
    st_point(
      coordonnees_ligne[
        nrow(coordonnees_ligne),
        c("X", "Y")
      ]
    ),
    
    crs = CRS_PROJET
    
  )
  
  
  return(
    
    list(
      
      point_A = point_a,
      
      point_B = point_b
      
    )
    
  )
  
}


# ------------------------------------------------------------------------------
# 4. TABLEAU DES RESULTATS
# ------------------------------------------------------------------------------

resultats_patch_41 <- data.frame(
  
  patch_A = integer(0),
  
  patch_B = integer(0),
  
  distance_reelle_m = numeric(0),
  
  cellule_accessible = integer(0),
  
  x_point_depart = numeric(0),
  
  y_point_depart = numeric(0),
  
  x_cellule = numeric(0),
  
  y_cellule = numeric(0)
  
)


# ------------------------------------------------------------------------------
# 5. ANALYSE DES PAIRES
# ------------------------------------------------------------------------------

for (i in seq_len(nrow(paires_patch_41))) {
  
  
  id_a <- paires_patch_41$patch_A[i]
  
  id_b <- paires_patch_41$patch_B[i]
  
  
  cat(
    "Analyse de la paire :",
    id_a,
    "-",
    id_b,
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # RECUPERATION DES PATCHES
  # --------------------------------------------------------------------------
  
  patch_a <- patches[
    
    patches$patch_id == id_a,
    
  ]
  
  
  patch_b <- patches[
    
    patches$patch_id == id_b,
    
  ]
  
  
  # --------------------------------------------------------------------------
  # CALCUL DES POINTS DE BORDURE
  # --------------------------------------------------------------------------
  
  points <- obtenir_points_bordure_diagnostic(
    
    patch_a,
    
    patch_b
    
  )
  
  
  # Dans tes paires actuelles, le patch 41 est patch_A.
  
  point_depart <- st_sf(
    
    patch_id = id_a,
    
    geometry = points$point_A
    
  )
  
  
  # --------------------------------------------------------------------------
  # RECHERCHE DE LA CELLULE ACCESSIBLE LA PLUS PROCHE
  #
  # IMPORTANT :
  #
  # Aucun seuil de distance n'est appliqué ici.
  # On veut connaître la distance réelle.
  # --------------------------------------------------------------------------
  
  index_proche <- st_nearest_feature(
    
    point_depart,
    
    points_accessibles
    
  )
  
  
  cellule_proche <- points_accessibles[
    
    index_proche,
    
  ]
  
  
  distance_reelle <- as.numeric(
    
    st_distance(
      
      point_depart,
      
      cellule_proche
      
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # ENREGISTREMENT DES RESULTATS
  # --------------------------------------------------------------------------
  
  coords_depart <- st_coordinates(
    
    point_depart
    
  )
  
  
  coords_cellule <- st_coordinates(
    
    cellule_proche
    
  )
  
  
  resultats_patch_41 <- rbind(
    
    resultats_patch_41,
    
    data.frame(
      
      patch_A = id_a,
      
      patch_B = id_b,
      
      distance_reelle_m = distance_reelle,
      
      cellule_accessible = cellule_proche$cellule,
      
      x_point_depart = coords_depart[1, "X"],
      
      y_point_depart = coords_depart[1, "Y"],
      
      x_cellule = coords_cellule[1, "X"],
      
      y_cellule = coords_cellule[1, "Y"]
      
    )
    
  )
  
  
}


# ------------------------------------------------------------------------------
# 6. AFFICHAGE DES RESULTATS
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("RESULTATS DU DIAGNOSTIC PATCH 41\n")
cat("====================================================\n\n")


print(
  resultats_patch_41
)


# ------------------------------------------------------------------------------
# 7. INTERPRETATION AUTOMATIQUE
# ------------------------------------------------------------------------------

resultats_patch_41$interpretation <- ifelse(
  
  resultats_patch_41$distance_reelle_m <= 50,
  
  "CELLULE ACCESSIBLE A MOINS OU EGAL A 50 M",
  
  "CELLULE ACCESSIBLE AU-DELA DE 50 M"
  
)


cat("\n")
cat("INTERPRETATION :\n\n")


print(
  resultats_patch_41[
    
    ,
    
    c(
      "patch_A",
      "patch_B",
      "distance_reelle_m",
      "interpretation"
    )
    
  ]
)


# ------------------------------------------------------------------------------
# 8. SAUVEGARDE
# ------------------------------------------------------------------------------

write.csv(
  
  resultats_patch_41,
  
  file.path(
    
    DOSSIER_DONNEES,
    
    "diagnostic_patch_41.csv"
    
  ),
  
  row.names = FALSE
  
)


cat("\n")
cat("Diagnostic terminé.\n")
cat(
  "Résultats sauvegardés dans :\n",
  file.path(
    DOSSIER_DONNEES,
    "diagnostic_patch_41.csv"
  ),
  "\n"
)