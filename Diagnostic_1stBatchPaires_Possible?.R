# ==============================================================================
# DIAGNOSTIC 2 - VÉRIFICATION DE LA CONNECTIVITÉ DES PAIRES DE PATCHES
# ==============================================================================
#
# Objectif :
#
# Vérifier si chacune des paires de patches à reconnecter possède désormais
# un point de départ et un point d'arrivée situés dans la même composante
# accessible.
#
# Une paire peut théoriquement être reliée par un chemin raster uniquement si :
#
#     composante_depart == composante_arrivee
#
# Le script ne calcule PAS encore les chemins de moindre coût.
#
# Il réalise uniquement un diagnostic préalable de connectivité.
# ==============================================================================


cat("\n")
cat("====================================================\n")
cat("DIAGNOSTIC 2 - CONNECTIVITÉ DES PAIRES DE PATCHES\n")
cat("====================================================\n\n")


# ------------------------------------------------------------------------------
# 1. VÉRIFICATION DES OBJETS NÉCESSAIRES
# ------------------------------------------------------------------------------

objets_necessaires <- c(
  "grille_cout",
  "composantes",
  "patches",
  "paires_patches"
)

objets_manquants <- objets_necessaires[
  !objets_necessaires %in% ls()
]


if (length(objets_manquants) > 0) {
  
  stop(
    "ERREUR : les objets suivants sont absents de l'environnement R : ",
    paste(objets_manquants, collapse = ", "),
    "\n\nExécute d'abord les scripts précédents."
  )
  
}


# ------------------------------------------------------------------------------
# 2. PARAMÈTRE DE DISTANCE MAXIMALE POUR LE SNAP
# ------------------------------------------------------------------------------
#
# Les points de bordure des patches ne tombent pas nécessairement exactement
# au centre d'une cellule raster accessible.
#
# Nous autorisons donc une recherche de la cellule accessible la plus proche.
#
# ATTENTION :
#
# Cette étape ne crée PAS de corridor supplémentaire.
#
# Elle sert uniquement à identifier quelle cellule accessible représente
# le point de départ ou d'arrivée du futur calcul de chemin.
# ------------------------------------------------------------------------------

DISTANCE_MAX_SNAP <- 50


cat(
  "Distance maximale autorisée pour le rattachement d'un point à une cellule :",
  DISTANCE_MAX_SNAP,
  "m\n\n"
)


# ------------------------------------------------------------------------------
# 4. FONCTION : TROUVER LA CELLULE ACCESSIBLE LA PLUS PROCHE
# ------------------------------------------------------------------------------
#
# IMPORTANT :
#
# La distance est calculée entre le point de bordure du patch et la SURFACE
# réelle de la cellule raster accessible, et non simplement jusqu'au centre
# de la cellule.
#
# Cette correction est nécessaire car une cellule de 10 m x 10 m possède une
# surface. Mesurer uniquement la distance jusqu'à son centre surestime
# artificiellement la distance réelle à franchir.
# ------------------------------------------------------------------------------

trouver_cellule_accessible <- function(
    point_sf,
    points_accessibles,
    composantes,
    grille_reference,
    distance_max = 50
) {
  
  # --------------------------------------------------------------------------
  # 1. IDENTIFICATION DE LA CELLULE ACCESSIBLE LA PLUS PROCHE
  # --------------------------------------------------------------------------
  
  index_proche <- st_nearest_feature(
    point_sf,
    points_accessibles
  )
  
  
  cellule_proche <- points_accessibles$cellule[
    index_proche
  ]
  
  
  point_cellule <- points_accessibles[
    index_proche,
  ]
  
  
  # --------------------------------------------------------------------------
  # 2. COORDONNÉES DU POINT DE DÉPART
  # --------------------------------------------------------------------------
  
  coord_point <- st_coordinates(
    point_sf
  )[1, c("X", "Y")]
  
  
  # --------------------------------------------------------------------------
  # 3. COORDONNÉES DU CENTRE DE LA CELLULE
  # --------------------------------------------------------------------------
  
  coord_centre <- st_coordinates(
    point_cellule
  )[1, c("X", "Y")]
  
  
  # --------------------------------------------------------------------------
  # 4. RÉSOLUTION RÉELLE DU RASTER
  # --------------------------------------------------------------------------
  
  resolution_xy <- terra::res(
    grille_reference
  )
  
  
  demi_resolution_x <- resolution_xy[1] / 2
  
  demi_resolution_y <- resolution_xy[2] / 2
  
  
  # --------------------------------------------------------------------------
  # 5. DISTANCE RÉELLE ENTRE LE POINT ET LA CELLULE
  # --------------------------------------------------------------------------
  #
  # On calcule la distance minimale entre le point et le rectangle occupé
  # par la cellule raster.
  #
  # Si le point est situé dans l'alignement horizontal ou vertical de la
  # cellule, la distance correspond directement à la distance jusqu'au bord.
  #
  # Si le point est situé en diagonale, on calcule la distance jusqu'au coin
  # le plus proche.
  # --------------------------------------------------------------------------
  
  distance_x <- max(
    abs(coord_point["X"] - coord_centre["X"]) -
      demi_resolution_x,
    0
  )
  
  
  distance_y <- max(
    abs(coord_point["Y"] - coord_centre["Y"]) -
      demi_resolution_y,
    0
  )
  
  
  distance_reelle <- sqrt(
    distance_x^2 +
      distance_y^2
  )
  
  
  # --------------------------------------------------------------------------
  # 6. DISTANCE AU CENTRE
  # --------------------------------------------------------------------------
  #
  # Conservée uniquement pour information et contrôle.
  # --------------------------------------------------------------------------
  
  distance_centre <- sqrt(
    (coord_point["X"] - coord_centre["X"])^2 +
      (coord_point["Y"] - coord_centre["Y"])^2
  )
  
  
  # --------------------------------------------------------------------------
  # 7. IDENTIFICATION DE LA COMPOSANTE CONNECTÉE
  # --------------------------------------------------------------------------
  
  composante <- terra::values(
    composantes,
    mat = FALSE
  )[cellule_proche]
  
  
  # --------------------------------------------------------------------------
  # 8. TEST DE LA DISTANCE MAXIMALE
  # --------------------------------------------------------------------------
  
  dans_distance <- distance_reelle <= distance_max
  
  
  # --------------------------------------------------------------------------
  # 9. RÉSULTAT
  # --------------------------------------------------------------------------
  
  return(
    data.frame(
      
      cellule = cellule_proche,
      
      distance_snap_m = distance_reelle,
      
      distance_centre_m = distance_centre,
      
      composante_id = composante,
      
      dans_distance_max = dans_distance
      
    )
  )
  
}
 
# ------------------------------------------------------------------------------
# 5. CALCUL DIRECT DES POINTS DE BORDURE POUR CHAQUE PAIRE
# ------------------------------------------------------------------------------
#
# IMPORTANT :
#
# Cette version ne dépend pas de l'objet points_bordure.
#
# Pour chaque paire :
#
# 1. on récupère les deux patches ;
# 2. on calcule la ligne la plus courte entre leurs géométries ;
# 3. la première extrémité de cette ligne devient le point de départ ;
# 4. la dernière extrémité devient le point d'arrivée.
#
# Ces points correspondent aux deux points des bordures des patches
# les plus proches l'un de l'autre.
# ------------------------------------------------------------------------------


cat(
  "Calcul direct des points de bordure pour",
  nrow(paires_patches),
  "paires de patches...\n\n"
)


# Vérification de sécurité

if (nrow(paires_patches) == 0) {
  
  stop(
    "ERREUR : paires_patches est vide. ",
    "Les paires de patches à reconnecter doivent être définies avant ",
    "d'exécuter le Diagnostic 2."
  )
  
}


# Listes qui contiendront les géométries des points

liste_points_depart <- vector(
  "list",
  nrow(paires_patches)
)


liste_points_arrivee <- vector(
  "list",
  nrow(paires_patches)
)


# Boucle sur toutes les paires

for (i in seq_len(nrow(paires_patches))) {
  
  
  # Identifiants des deux patches
  
  id_a <- paires_patches$patch_A[i]
  
  id_b <- paires_patches$patch_B[i]
  
  
  # Récupération du patch A
  
  patch_a <- patches[
    patches$patch_id == id_a,
  ]
  
  
  # Récupération du patch B
  
  patch_b <- patches[
    patches$patch_id == id_b,
  ]
  
  
  # Vérification de sécurité
  
  if (nrow(patch_a) != 1) {
    
    stop(
      "ERREUR : le patch ",
      id_a,
      " est introuvable ou apparaît plusieurs fois."
    )
    
  }
  
  
  if (nrow(patch_b) != 1) {
    
    stop(
      "ERREUR : le patch ",
      id_b,
      " est introuvable ou apparaît plusieurs fois."
    )
    
  }
  
  
  # Calcul de la ligne la plus courte entre les deux patches
  
  ligne_plus_courte <- st_nearest_points(
    st_geometry(patch_a),
    st_geometry(patch_b)
  )
  
  
  # Extraction des coordonnées de cette ligne
  
  coordonnees_ligne <- st_coordinates(
    ligne_plus_courte
  )
  
  
  # Vérification
  
  if (nrow(coordonnees_ligne) < 2) {
    
    stop(
      "ERREUR : impossible de déterminer les deux points de bordure ",
      "pour la paire ",
      id_a,
      " - ",
      id_b
    )
    
  }
  
  
  # Premier point de la ligne :
  # point situé sur le patch A
  
  point_depart <- st_point(
    coordonnees_ligne[1, c("X", "Y")]
  )
  
  
  # Dernier point de la ligne :
  # point situé sur le patch B
  
  point_arrivee <- st_point(
    coordonnees_ligne[
      nrow(coordonnees_ligne),
      c("X", "Y")
    ]
  )
  
  
  # Enregistrement
  
  liste_points_depart[[i]] <- point_depart
  
  liste_points_arrivee[[i]] <- point_arrivee
  
}


# ------------------------------------------------------------------------------
# 5B. CRÉATION DES OBJETS SF DE POINTS
# ------------------------------------------------------------------------------

points_depart <- st_sf(
  
  patch_A = paires_patches$patch_A,
  
  patch_B = paires_patches$patch_B,
  
  geometry = st_sfc(
    liste_points_depart,
    crs = CRS_PROJET
  )
  
)


points_arrivee <- st_sf(
  
  patch_A = paires_patches$patch_A,
  
  patch_B = paires_patches$patch_B,
  
  geometry = st_sfc(
    liste_points_arrivee,
    crs = CRS_PROJET
  )
  
)


# ------------------------------------------------------------------------------
# 5C. CONTRÔLES
# ------------------------------------------------------------------------------

cat(
  "Nombre de points de départ créés :",
  nrow(points_depart),
  "\n"
)


cat(
  "Nombre de points d'arrivée créés :",
  nrow(points_arrivee),
  "\n\n"
)


# Vérification finale

if (
  nrow(points_depart) != nrow(paires_patches)
) {
  
  stop(
    "ERREUR : le nombre de points de départ ne correspond pas ",
    "au nombre de paires."
  )
  
}


if (
  nrow(points_arrivee) != nrow(paires_patches)
) {
  
  stop(
    "ERREUR : le nombre de points d'arrivée ne correspond pas ",
    "au nombre de paires."
  )
  
}



# ------------------------------------------------------------------------------
# 6. INITIALISATION DU TABLEAU DE RÉSULTATS
# ------------------------------------------------------------------------------

resultats_connectivite <- data.frame(
  
  patch_A = integer(0),
  patch_B = integer(0),
  
  cellule_depart = integer(0),
  cellule_arrivee = integer(0),
  
  distance_depart_m = numeric(0),
  distance_arrivee_m = numeric(0),
  
  composante_depart = integer(0),
  composante_arrivee = integer(0),
  
  meme_composante = logical(0),
  
  resultat = character(0)
  
)


# ------------------------------------------------------------------------------
# 7. ANALYSE DE CHAQUE PAIRE
# ------------------------------------------------------------------------------

for (i in seq_len(nrow(paires_patches))) {
  
  
  cat(
    "Analyse de la paire :",
    paires_patches$patch_A[i],
    "-",
    paires_patches$patch_B[i],
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # POINT DE DÉPART
  # --------------------------------------------------------------------------
  
  point_depart <- points_depart[i, ]
  
  
  resultat_depart <- trouver_cellule_accessible(
    
    point_sf = point_depart,
    
    points_accessibles = points_accessibles,
    
    composantes = composantes,
    
    grille_reference = grille_cout,
    
    distance_max = DISTANCE_MAX_SNAP
    
  )
  
  # --------------------------------------------------------------------------
  # POINT D'ARRIVÉE
  # --------------------------------------------------------------------------
  
  point_arrivee <- points_arrivee[i, ]
  
  
  resultat_arrivee <- trouver_cellule_accessible(
    
    point_sf = point_arrivee,
    
    points_accessibles = points_accessibles,
    
    composantes = composantes,
    
    grille_reference = grille_cout,
    
    distance_max = DISTANCE_MAX_SNAP
    
  )
  
  # --------------------------------------------------------------------------
  # TEST DE CONNECTIVITÉ
  # --------------------------------------------------------------------------
  
  points_accessibles_ok <-
    
    resultat_depart$dans_distance_max &&
    resultat_arrivee$dans_distance_max
  
  
  meme_composante <- FALSE
  
  
  if (
    isTRUE(points_accessibles_ok) &&
    !is.na(resultat_depart$composante_id) &&
    !is.na(resultat_arrivee$composante_id)
  ) {
    
    meme_composante <-
      resultat_depart$composante_id ==
      resultat_arrivee$composante_id
    
  }
  
  
  # --------------------------------------------------------------------------
  # DÉTERMINATION DU RÉSULTAT
  # --------------------------------------------------------------------------
  
  if (!resultat_depart$dans_distance_max) {
    
    resultat <- "DEPART TROP ELOIGNE"
    
  } else if (!resultat_arrivee$dans_distance_max) {
    
    resultat <- "ARRIVEE TROP ELOIGNEE"
    
  } else if (meme_composante) {
    
    resultat <- "CONNECTABLE"
    
  } else {
    
    resultat <- "COMPOSANTES DIFFERENTES"
    
  }
  
  
  # --------------------------------------------------------------------------
  # AJOUT AU TABLEAU
  # --------------------------------------------------------------------------
  
  resultats_connectivite <- rbind(
    
    resultats_connectivite,
    
    data.frame(
      
      patch_A =
        paires_patches$patch_A[i],
      
      patch_B =
        paires_patches$patch_B[i],
      
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
      
      meme_composante =
        meme_composante,
      
      resultat =
        resultat
      
    )
    
  )
  
  
}


# ------------------------------------------------------------------------------
# 8. AFFICHAGE DU TABLEAU COMPLET
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("RÉSULTATS DE LA CONNECTIVITÉ\n")
cat("====================================================\n\n")


print(
  resultats_connectivite
)


# ------------------------------------------------------------------------------
# 9. RÉSUMÉ
# ------------------------------------------------------------------------------

nombre_total <- nrow(
  resultats_connectivite
)


nombre_connectables <- sum(
  resultats_connectivite$resultat ==
    "CONNECTABLE"
)


nombre_non_connectables <-
  
  nombre_total -
  nombre_connectables


cat("\n")

cat(
  "Nombre total de paires :",
  nombre_total,
  "\n"
)


cat(
  "Paires connectables :",
  nombre_connectables,
  "\n"
)


cat(
  "Paires présentant encore un problème :",
  nombre_non_connectables,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 10. AFFICHAGE DES PAIRES PROBLÉMATIQUES
# ------------------------------------------------------------------------------

paires_problemes <-
  
  resultats_connectivite %>%
  
  filter(
    resultat != "CONNECTABLE"
  )


if (nrow(paires_problemes) > 0) {
  
  
  cat(
    "====================================================\n"
  )
  
  
  cat(
    "PAIRES PRÉSENTANT ENCORE UN PROBLÈME\n"
  )
  
  
  cat(
    "====================================================\n\n"
  )
  
  
  print(
    paires_problemes
  )
  
  
} else {
  
  
  cat(
    "====================================================\n"
  )
  
  
  cat(
    "SUCCÈS : TOUTES LES PAIRES SONT CONNECTABLES\n"
  )
  
  
  cat(
    "====================================================\n"
  )
  
}


# ------------------------------------------------------------------------------
# 11. SAUVEGARDE DES RÉSULTATS
# ------------------------------------------------------------------------------

chemin_resultats <- file.path(
  
  DOSSIER_DONNEES,
  
  "diagnostic_connectivite_paires.csv"
  
)


write.csv(
  
  resultats_connectivite,
  
  chemin_resultats,
  
  row.names = FALSE
  
)


cat("\n")

cat(
  "Résultats sauvegardés dans :\n",
  chemin_resultats,
  "\n\n"
)


cat(
  "====================================================\n"
)


cat(
  "DIAGNOSTIC 2 TERMINÉ\n"
)


cat(
  "====================================================\n"
)