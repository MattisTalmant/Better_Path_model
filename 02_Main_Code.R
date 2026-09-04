# ==============================================================================
# MÉMOIRE - Reconnexion des patches de pâturage (Hungarian Grey Cattle)
# Parc national Neusiedler See-Seewinkel, Autriche
#
# ÉTAPE 1 / 5 : Configuration du projet + import des couches + calcul des
#               points de bordure les plus proches entre patches
#
# CHANGEMENT PAR RAPPORT À LA VERSION PRÉCÉDENTE :
# On n'utilise plus les centroïdes. Chaque chemin doit démarrer/arriver sur
# le point précis de la BORDURE d'un patch le plus proche de la bordure de
# l'autre patch à relier. Ce point est donc spécifique à CHAQUE PAIRE de
# patches, et non plus un point unique par patch.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. INSTALLATION DES PACKAGES (à exécuter une seule fois, puis commente
#    ces lignes avec un # devant chacune, ou supprime-les)
# ------------------------------------------------------------------------------

# install.packages("sf")
# install.packages("terra")
# install.packages("leastcostpath")
# install.packages("dplyr")
# install.packages("ggplot2")


# ------------------------------------------------------------------------------
# 1. CHARGEMENT DES LIBRAIRIES (à exécuter à chaque session)
# ------------------------------------------------------------------------------

library(sf)
library(terra)
library(leastcostpath)
library(dplyr)
library(ggplot2)


# ------------------------------------------------------------------------------
# 2. CONFIGURATION DU PROJET
# ------------------------------------------------------------------------------

CRS_PROJET <- 31287   # MGI / Austria Lambert
DOSSIER_DONNEES <- "~/Desktop/Data_POM"   # MODIFIE ce chemin vers ton dossier de données


# ------------------------------------------------------------------------------
# 3. IMPORT DES COUCHES
# ------------------------------------------------------------------------------
zone_etude          <- st_read(file.path(DOSSIER_DONNEES, "ZOI.shp"))
patches             <- st_read(file.path(DOSSIER_DONNEES, "Grazing_Patches.shp"))
parcelles_invekos   <- st_read(file.path(DOSSIER_DONNEES, "Hull_data.shp"))
occurrences_points     <- st_read(file.path(DOSSIER_DONNEES, "a17_spec_B.gpkg"), layer = "a17_pt_spec")
occurrences_polygones  <- st_read(file.path(DOSSIER_DONNEES, "a17_spec_B.gpkg"), layer = "a17_pol_spec")

# ------------------------------------------------------------------------------
# 4. HARMONISATION DU CRS
# ------------------------------------------------------------------------------
zone_etude          <- st_transform(zone_etude, CRS_PROJET)
patches             <- st_transform(patches, CRS_PROJET)
parcelles_invekos   <- st_transform(parcelles_invekos, CRS_PROJET)
occurrences_points     <- st_transform(occurrences_points, CRS_PROJET)
occurrences_polygones  <- st_transform(occurrences_polygones, CRS_PROJET)

# ------------------------------------------------------------------------------
# 5. IDENTIFIANT UNIQUE ET PERMANENT POUR CHAQUE PATCH
# ------------------------------------------------------------------------------
#
# IMPORTANT :
#
# Les anciens identifiants doivent rester inchangés.
#
# Les nouveaux patches doivent posséder les identifiants 52, 53, 54 et 55.
#
# Le script ne recrée donc JAMAIS les identifiants avec seq_len().
#
# Il récupère les identifiants enregistrés dans la couche QGIS, puis les
# convertit en nombres entiers si le Shapefile les a importés comme texte.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 5.1. Vérification de l'existence de la colonne patch_id
# ------------------------------------------------------------------------------

if (!"patch_id" %in% names(patches)) {
  
  stop(
    "ERREUR : la colonne 'patch_id' est absente de la couche ",
    "'Grazing_Patches.shp'.\n\n",
    "Crée cette colonne dans QGIS et attribue un identifiant permanent ",
    "à chaque patch."
  )
  
}


# ------------------------------------------------------------------------------
# 5.2. Conversion de patch_id en entier numérique
# ------------------------------------------------------------------------------
#
# Cette conversion est indispensable.
#
# Dans un Shapefile, une colonne contenant 1, 2, 3, etc. peut être importée
# comme texte. Par exemple :
#
# "1", "2", "3", ..., "55"
#
# La fonction as.integer(as.character(...)) convertit ces valeurs en :
#
# 1, 2, 3, ..., 55
#
# IMPORTANT :
# Cette opération convertit le type de la colonne.
# Elle ne recrée pas les identifiants et ne modifie pas leur valeur.
# ------------------------------------------------------------------------------

cat(
  "Type de patch_id avant conversion :",
  paste(class(patches$patch_id), collapse = ", "),
  "\n"
)

patches$patch_id <- suppressWarnings(
  as.integer(
    as.character(
      patches$patch_id
    )
  )
)


cat(
  "Type de patch_id après conversion :",
  paste(class(patches$patch_id), collapse = ", "),
  "\n"
)


# ------------------------------------------------------------------------------
# 5.3. Vérification des valeurs non converties
# ------------------------------------------------------------------------------

if (anyNA(patches$patch_id)) {
  
  lignes_problematiques <- which(
    is.na(patches$patch_id)
  )
  
  stop(
    "ERREUR : certaines valeurs de 'patch_id' ne sont pas numériques ",
    "ou sont manquantes.\n\n",
    "Lignes problématiques : ",
    paste(lignes_problematiques, collapse = ", "),
    "\n\n",
    "Vérifie les valeurs de patch_id dans QGIS."
  )
  
}


# ------------------------------------------------------------------------------
# 5.4. Vérification de l'unicité des identifiants
# ------------------------------------------------------------------------------

if (anyDuplicated(patches$patch_id) > 0) {
  
  identifiants_dupliques <- unique(
    patches$patch_id[
      duplicated(patches$patch_id)
    ]
  )
  
  stop(
    "ERREUR : plusieurs patches possèdent le même patch_id.\n\n",
    "Identifiants dupliqués : ",
    paste(identifiants_dupliques, collapse = ", "),
    "\n\n",
    "Chaque patch doit avoir un identifiant unique."
  )
  
}


# ------------------------------------------------------------------------------
# 5.5. Vérification du nombre de patches
# ------------------------------------------------------------------------------

nombre_patches <- nrow(patches)

cat(
  "Nombre de patches importés :",
  nombre_patches,
  "\n"
)

if (nombre_patches != 55) {
  
  stop(
    "ERREUR : le script attend 55 patches après l'ajout des quatre nouveaux ",
    "polygones, mais ",
    nombre_patches,
    " patches ont été importés.\n\n",
    "Vérifie que le fichier Grazing_Patches.shp contient bien les 51 anciens ",
    "patches et les 4 nouveaux."
  )
  
}


# ------------------------------------------------------------------------------
# 5.6. Vérification des identifiants attendus
# ------------------------------------------------------------------------------
#
# Les identifiants attendus sont :
#
# 1 à 51  = anciens patches
# 52 à 55 = nouveaux patches
# ------------------------------------------------------------------------------

identifiants_attendus <- seq_len(55)

identifiants_manquants <- setdiff(
  identifiants_attendus,
  patches$patch_id
)

identifiants_incorrects <- setdiff(
  patches$patch_id,
  identifiants_attendus
)


if (
  length(identifiants_manquants) > 0 ||
  length(identifiants_incorrects) > 0
) {
  
  stop(
    "ERREUR : les identifiants des patches ne correspondent pas à la série ",
    "attendue de 1 à 55.\n\n",
    "Identifiants manquants : ",
    ifelse(
      length(identifiants_manquants) == 0,
      "aucun",
      paste(identifiants_manquants, collapse = ", ")
    ),
    "\n",
    "Identifiants inattendus : ",
    ifelse(
      length(identifiants_incorrects) == 0,
      "aucun",
      paste(identifiants_incorrects, collapse = ", ")
    ),
    "\n\n",
    "Vérifie les valeurs de patch_id dans QGIS."
  )
  
}


# ------------------------------------------------------------------------------
# 5.7. Contrôle spécifique des quatre nouveaux patches
# ------------------------------------------------------------------------------

nouveaux_patch_id <- c(
  52L,
  53L,
  54L,
  55L
)

if (!all(nouveaux_patch_id %in% patches$patch_id)) {
  
  stop(
    "ERREUR : les quatre nouveaux patches doivent posséder les identifiants ",
    "52, 53, 54 et 55."
  )
  
}


# ------------------------------------------------------------------------------
# 5.8. Affichage final des identifiants
# ------------------------------------------------------------------------------

cat(
  "Identifiants des patches :",
  paste(sort(patches$patch_id), collapse = ", "),
  "\n"
)

cat(
  "Contrôle réussi : patch_id est numérique, unique et complet.\n"
)

cat(
  "Les nouveaux patches sont :",
  paste(nouveaux_patch_id, collapse = ", "),
  "\n\n"
)



# ============================================================
# 6. DEFINITION DES PAIRES DE PATCHES
# ============================================================

# ------------------------------------------------------------
# 6A. Anciennes paires : conserver toutes les paires existantes
# ------------------------------------------------------------

paires_initiales <- data.frame(
  patch_a = c(
    11,
    37,
    41,
    41,
    25,
    10,
    48,
    26,
    7,
    7,
    9,
    20,
    40
  ),
  
  patch_b = c(
    17,
    10,
    12,
    13,
    6,
    15,
    19,
    11,
    5,
    9,
    17,
    8,
    15
  )
)

# ------------------------------------------------------------
# 6B. Nouveau chemin à ajouter
#
# 18 → 52 → 53 → 54 → 9 → 55 → 11 → 17
# ------------------------------------------------------------

route_nouveaux_patches <- data.frame(
  ordre = 1:8,
  patch_id = c(
    18L,
    52L,
    53L,
    54L,
    9L,
    55L,
    11L,
    17L
  )
)

# ------------------------------------------------------------
# 6C. Transformation du nouveau chemin en paires successives
# ------------------------------------------------------------

paires_nouveau_chemin <- data.frame(
  patch_a = route_nouveaux_patches$patch_id[
    -nrow(route_nouveaux_patches)
  ],
  patch_b = route_nouveaux_patches$patch_id[
    -1
  ]
)

# ------------------------------------------------------------
# 6D. Combinaison des anciennes paires et du nouveau chemin
# ------------------------------------------------------------

paires_patches <- bind_rows(
  paires_initiales,
  paires_nouveau_chemin
) %>%
  distinct(patch_a, patch_b, .keep_all = TRUE)

# ------------------------------------------------------------
# 6E. Vérifications
# ------------------------------------------------------------

# Vérification des identifiants utilisés
ids_utilises <- unique(
  c(
    paires_patches$patch_a,
    paires_patches$patch_b
  )
)

ids_invalides <- ids_utilises[
  !ids_utilises %in% patches$patch_id
]

if (length(ids_invalides) > 0) {
  stop(
    "Les identifiants suivants utilisés dans les paires ",
    "n'existent pas dans la couche patches : ",
    paste(ids_invalides, collapse = ", ")
  )
}

# Vérification du nouveau chemin
paires_attendues_nouveau_chemin <- data.frame(
  patch_a = c(18L, 52L, 53L, 54L, 9L, 55L, 11L),
  patch_b = c(52L, 53L, 54L, 9L, 55L, 11L, 17L)
)

if (!all(
  apply(
    paires_attendues_nouveau_chemin,
    1,
    function(x) {
      any(
        paires_patches$patch_a == as.integer(x[1]) &
        paires_patches$patch_b == as.integer(x[2])
      )
    }
  )
)) {
  stop(
    "Le nouveau chemin 18-52-53-54-9-55-11-17 ",
    "n'a pas été ajouté correctement."
  )
}

cat("\nNombre total de paires à reconnecter :", nrow(paires_patches), "\n")

cat("\nPaires du nouveau chemin :\n")
print(paires_nouveau_chemin)

cat("\nToutes les paires à reconnecter :\n")
print(paires_patches)
  
 
# Vérification : tous les patch_id mentionnés dans paires_patches doivent
# exister réellement dans la couche patches.

ids_manquants <- setdiff(
  unique(
    c(
      paires_patches$patch_a,
      paires_patches$patch_b
    )
  ),
  patches$patch_id
)

if (length(ids_manquants) > 0) {
  
  stop(
    "ERREUR : les patch_id suivants utilisés dans 'paires_patches' ",
    "n'existent pas dans la couche 'patches' : ",
    paste(ids_manquants, collapse = ", ")
  )
}

cat("Nombre de paires de patches à connecter :", nrow(paires_patches), "\n\n")


# ------------------------------------------------------------------------------
# 7. FONCTION : POINTS DE BORDURE LES PLUS PROCHES ENTRE DEUX PATCHES
# ------------------------------------------------------------------------------

obtenir_points_bordure <- function(patch_a_geom, patch_b_geom) {
  
  # st_nearest_points crée une ligne reliant les deux points
  # les plus proches entre les deux patches
  ligne_plus_courte <- st_nearest_points(
    patch_a_geom,
    patch_b_geom
  )
  
  # Extraction des coordonnées des deux extrémités de la ligne
  coordonnees <- st_coordinates(ligne_plus_courte)
  
  # Premier point = point situé sur le patch A
  point_depart <- st_sfc(
    st_point(coordonnees[1, 1:2]),
    crs = st_crs(patch_a_geom)
  )
  
  # Dernier point = point situé sur le patch B
  point_arrivee <- st_sfc(
    st_point(coordonnees[nrow(coordonnees), 1:2]),
    crs = st_crs(patch_b_geom)
  )
  
  list(
    depart = point_depart,
    arrivee = point_arrivee
  )
}

# ------------------------------------------------------------------------------
# 7B. VÉRIFICATION DE LA CORRESPONDANCE PATCH_ID / GÉOMÉTRIE
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("VÉRIFICATION DES PATCHES UTILISÉS\n")
cat("====================================================\n")

for (id in unique(
  c(
    paires_patches$patch_a,
    paires_patches$patch_b
  )
)) {
  
  nombre_geometries <- sum(
    patches$patch_id == id
  )
  
  cat(
    "patch_id ",
    id,
    " : ",
    nombre_geometries,
    " géométrie(s)\n",
    sep = ""
  )
  
  if (nombre_geometries != 1) {
    
    stop(
      "ERREUR : le patch_id ",
      id,
      " correspond à ",
      nombre_geometries,
      " géométrie(s) dans la couche 'patches'. ",
      "Chaque patch_id utilisé doit correspondre exactement à un polygone."
    )
  }
}

cat(
  "Contrôle patch_id / géométrie réussi.\n\n"
)

# ------------------------------------------------------------------------------
# 8. CALCUL DES POINTS DE BORDURE POUR CHAQUE PAIRE
# ------------------------------------------------------------------------------

if (nrow(paires_patches) > 0) {
  
  liste_points_depart <- vector(
    mode = "list",
    length = nrow(paires_patches)
  )
  
  liste_points_arrivee <- vector(
    mode = "list",
    length = nrow(paires_patches)
  )
  
  
  for (i in seq_len(nrow(paires_patches))) {
    
    id_a <- paires_patches$patch_a[i]
    id_b <- paires_patches$patch_b[i]
    
    
    # Sélection du patch A
    geom_a <- patches[
      patches$patch_id == id_a,
    ]
    
    
    # Sélection du patch B
    geom_b <- patches[
      patches$patch_id == id_b,
    ]
    
    
    # Vérification de sécurité
    if (nrow(geom_a) != 1) {
      
      stop(
        "ERREUR : le patch_a ",
        id_a,
        " ne correspond pas à un seul polygone. ",
        "Nombre de géométries trouvées : ",
        nrow(geom_a)
      )
      
    }
    
    if (nrow(geom_b) != 1) {
      
      stop(
        "ERREUR : le patch_b ",
        id_b,
        " ne correspond pas à un seul polygone. ",
        "Nombre de géométries trouvées : ",
        nrow(geom_b)
      )
      
    }
    
    # Calcul des deux points de bordure
    points <- obtenir_points_bordure(
      st_geometry(geom_a),
      st_geometry(geom_b)
    )
    
    
    # Extraction du premier point
    liste_points_depart[[i]] <- points$depart[[1]]
    
    
    # Extraction du second point
    liste_points_arrivee[[i]] <- points$arrivee[[1]]
    
  }
  
  
  # Conversion des listes de points en objets sfc_POINT
  
  geometries_depart <- st_sfc(
    liste_points_depart,
    crs = CRS_PROJET
  )
  
  geometries_arrivee <- st_sfc(
    liste_points_arrivee,
    crs = CRS_PROJET
  )
  
  
  # Création de la couche sf finale
  
  points_bordure <- st_sf(
    
    patch_a = as.integer(
      paires_patches$patch_a
    ),
    
    patch_b = as.integer(
      paires_patches$patch_b
    ),
    
    geometry_depart = geometries_depart,
    
    geometry_arrivee = geometries_arrivee,
    
    crs = CRS_PROJET
    
  )
  
  cat(
    "Points de bordure calculés pour",
    nrow(points_bordure),
    "paires de patches.\n\n"
  )
  
  
} else {
  
  points_bordure <- NULL
  
  cat(
    "paires_patches est vide : aucun point de bordure à calculer.\n"
  )
  
}

# ------------------------------------------------------------------------------
# 9. VÉRIFICATIONS
# ------------------------------------------------------------------------------

cat("=== VÉRIFICATION DES CRS ===\n")
cat("zone_etude :", st_crs(zone_etude)$input, "\n")
cat("patches :", st_crs(patches)$input, "\n")
cat("parcelles_invekos :", st_crs(parcelles_invekos)$input, "\n")
cat("occurrences_points :", st_crs(occurrences_points)$input, "\n")
cat("occurrences_polygones :", st_crs(occurrences_polygones)$input, "\n\n")

cat("=== COLONNES DE LA COUCHE INVEKOS ===\n")
print(names(parcelles_invekos))
cat("\n-> Repère ici les noms EXACTS des colonnes pour : gross margin, type\n")
cat("   de culture, statut bio/conventionnel, farm ID, classification d'usage.\n")
cat("   On les utilisera tels quels à l'étape 2.\n\n")


# ------------------------------------------------------------------------------
# 10A. CARTE D'IDENTIFICATION DES PATCH_ID
# ------------------------------------------------------------------------------
# Cette carte n'affiche QUE les patches avec leur patch_id en étiquette.
# Elle sert à identifier visuellement quels patches tu veux relier, pour
# ensuite remplir la liste "paires_patches" à l'étape 6 ci-dessus.

centroides_labels <- st_centroid(patches)   # uniquement pour PLACER le texte,
# n'a aucun rôle dans le modèle

ggplot() +
  geom_sf(data = zone_etude, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = patches, fill = "forestgreen", alpha = 0.5) +
  geom_sf_text(data = centroides_labels, aes(label = patch_id), size = 3, color = "black") +
  labs(title = "Identification des patches",
       subtitle = "Utilise ces patch_id pour construire ta liste de paires à l'étape 6") +
  theme_minimal()


# ------------------------------------------------------------------------------
# 10B. VISUALISATION DE CONTRÔLE DES POINTS DE BORDURE
# ------------------------------------------------------------------------------
#
# Cette carte affiche :
#
# - les patches ;
# - les segments entre les points de bordure ;
# - les points de départ ;
# - les points d'arrivée ;
# - les occurrences ponctuelles.
#
# Les segments orange ne sont pas encore les chemins de moindre coût.
# Ils servent uniquement à contrôler les points de départ et d'arrivée.
# ------------------------------------------------------------------------------

if (
  nrow(paires_patches) > 0 &&
  !is.null(points_bordure)
) {
  
  
  # ---------------------------------------------------------------------------
  # 10B.1. Création des lignes de contrôle
  # ---------------------------------------------------------------------------
  
  liste_segments_controle <- vector(
    mode = "list",
    length = nrow(points_bordure)
  )
  
  
  for (i in seq_len(nrow(points_bordure))) {
    
    depart <- st_coordinates(
      points_bordure$geometry_depart[i]
    )
    
    arrivee <- st_coordinates(
      points_bordure$geometry_arrivee[i]
    )
    
    
    # Chaque ligne possède deux sommets :
    #
    # 1. le point de bordure du patch A ;
    # 2. le point de bordure du patch B.
    
    coordonnees_ligne <- rbind(
      depart[1, 1:2],
      arrivee[1, 1:2]
    )
    
    
    liste_segments_controle[[i]] <- st_linestring(
      coordonnees_ligne
    )
    
  }
  
  
  # Conversion de la liste en objet sfc_LINESTRING
  
  geometries_segments <- st_sfc(
    liste_segments_controle,
    crs = CRS_PROJET
  )
  
  
  # Création de la couche sf des segments
  
  segments_controle <- st_sf(
    
    patch_a = points_bordure$patch_a,
    
    patch_b = points_bordure$patch_b,
    
    geometry = geometries_segments
    
  )
  
  # ---------------------------------------------------------------------------
  # 10B.2. Carte de contrôle
  # ---------------------------------------------------------------------------
  
  ggplot() +
    
    geom_sf(
      data = zone_etude,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    
    geom_sf(
      data = patches,
      fill = "forestgreen",
      alpha = 0.5
    ) +
    
    geom_sf(
      data = segments_controle,
      color = "orange",
      linewidth = 0.4,
      linetype = "dashed"
    ) +
    
    geom_sf(
      data = st_as_sf(
        points_bordure,
        sf_column_name = "geometry_depart"
      ),
      color = "darkgreen",
      size = 1
    ) +
    
    geom_sf(
      data = st_as_sf(
        points_bordure,
        sf_column_name = "geometry_arrivee"
      ),
      color = "blue",
      size = 1
    ) +
    
    geom_sf(
      data = occurrences_points,
      color = "red",
      size = 1,
      shape = 4
    ) +
    
    labs(
      title =
        "Contrôle : points de bordure les plus proches entre paires de patches",
      
      subtitle =
        "Pointillés oranges = liaison directe entre les bordures, pas le chemin final"
    ) +
    
    theme_minimal()
  
  
} else {
  
  cat(
    "paires_patches est vide : aucune carte de contrôle à produire.\n"
  )
  
}
# ==============================================================================
# PROCHAINE ÉTAPE (script 02) : jointure spatiale patches/INVEKOS, puis
# normalisation des variables de coût, en vue de la grille de coût pour
# leastcostpath. Les points "depart"/"arrivee" calculés ici serviront
# d'origin/destination pour create_lcp() sur cette grille.
# ==============================================================================



# ==============================================================================
# MÉMOIRE - Reconnexion des patches de pâturage
# Hungarian Grey Cattle
# Parc national Neusiedler See-Seewinkel, Autriche
#
# ÉTAPE 2 / 5 : Construction de la surface de coût
#
# VERSION ACTUELLE DU MODÈLE :
#
# 1. Le chemin doit rester EXCLUSIVEMENT sur des parcelles INVEKOS.
#
# 2. Les parcelles avec :
#       - gross_marg = NA
#       - gross_marg <= 0
#    sont temporairement INTERDITES au passage.
#
# 3. Les occurrences polygonales ne sont PAS utilisées.
#
# 4. Les occurrences ponctuelles reçoivent un buffer de 10 m.
#
# 5. Une zone proche d'un plus grand nombre d'occurrences reçoit
#    un coût plus faible.
#
# IMPORTANT :
# Nb_F (nombre d'exploitations traversées) n'est PAS calculé ici.
# Cette variable dépend du chemin complet et sera calculée après la
# génération des chemins.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. CHARGEMENT DES LIBRAIRIES
# ------------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(ggplot2)


# ------------------------------------------------------------------------------
# 2. VÉRIFICATION DES DONNÉES NÉCESSAIRES
# ------------------------------------------------------------------------------
# Le script 01 doit avoir été exécuté avant ce script.

objets_necessaires <- c(
  "zone_etude",
  "patches",
  "parcelles_invekos_modele",
  "occurrences_points",
  "DOSSIER_DONNEES",
  "CRS_PROJET"
)

objets_manquants <- objets_necessaires[
  !objets_necessaires %in% ls()
]

if (length(objets_manquants) > 0) {
  
  stop(
    "ERREUR : les objets suivants sont absents de l'environnement R : ",
    paste(objets_manquants, collapse = ", "),
    "\n\nExécute d'abord le script 01."
  )
  
}

# ------------------------------------------------------------------------------
# UTILISATION DES PARCELLES PRÉPARÉES POUR LE MODÈLE
# ------------------------------------------------------------------------------

# Le script 01b traite les parcelles GRÜNBRACHE et GRÜNLANDBRACHE
# et crée l'objet parcelles_invekos_modele.

parcelles_modele <- parcelles_invekos_modele
# ------------------------------------------------------------------------------
# 3. PARAMÈTRES DU MODÈLE
# ------------------------------------------------------------------------------

# Taille des cellules du raster
RESOLUTION <- 10

# Distance autour de chaque occurrence ponctuelle
BUFFER_OCCURRENCES <- 10


# ------------------------------------------------------------------------------
# 4. POIDS DE PONDÉRATION
# ------------------------------------------------------------------------------
#
# ⚠️ À DÉFINIR PLUS TARD
#
# Pour que le script puisse fonctionner pendant les tests, les deux poids
# sont temporairement identiques.
#
# IMPORTANT :
# Ces valeurs ne constituent PAS encore un choix méthodologique définitif.

POIDS_GROSS_MARGIN <- 0.5
POIDS_OCCURRENCES  <- 0.5


# ------------------------------------------------------------------------------
# 5. CONTRÔLE DES GROSS MARGINS
# ------------------------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("CONTRÔLE DES GROSS MARGINS\n")
cat("====================================================\n")

cat(
  "Nombre total de parcelles INVEKOS :",
  nrow(parcelles_invekos_modele),
  "\n"
)
cat(
  "Parcelles avec gross_marg NA :",
  sum(is.na(parcelles_invekos_modele$gross_marg)),
  "\n"
)

cat(
  "Parcelles avec gross_marg <= 0 :",
  sum(
    !is.na(parcelles_invekos_modele$gross_marg) &
      parcelles_invekos_modele$gross_marg <= 0
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. CRÉATION DES PARCELLES AUTORISÉES
# ------------------------------------------------------------------------------
#
# Une parcelle est utilisable uniquement si :
#
# gross_marg > 0
#
# Les parcelles NA, nulles ou négatives sont exclues.

parcelles_autorisees <- parcelles_invekos_modele %>%
  filter(
    !is.na(gross_marg),
    gross_marg > 0
  )

cat(
  "Nombre de parcelles AUTORISÉES :",
  nrow(parcelles_autorisees),
  "\n"
)

cat(
  "Nombre de parcelles INTERDITES :",
  nrow(parcelles_invekos_modele) -
    nrow(parcelles_autorisees),
  "\n\n"
)


# Vérification de sécurité

if (nrow(parcelles_autorisees) == 0) {
  
  stop(
    "ERREUR : aucune parcelle autorisée n'a été trouvée."
  )
  
}


# ------------------------------------------------------------------------------
# 7. CRÉATION DU RASTER DE RÉFÉRENCE
# ------------------------------------------------------------------------------
#
# Le raster couvre toute la zone d'étude.
#
# Mais seules les cellules correspondant aux parcelles autorisées
# seront conservées à la fin.

emprise <- ext(vect(zone_etude))

grille_reference <- rast(
  emprise,
  resolution = RESOLUTION,
  crs = paste0("EPSG:", CRS_PROJET)
)


cat("====================================================\n")
cat("GRILLE DE RÉFÉRENCE\n")
cat("====================================================\n")

cat(
  "Nombre de lignes :",
  nrow(grille_reference),
  "\n"
)

cat(
  "Nombre de colonnes :",
  ncol(grille_reference),
  "\n"
)

cat(
  "Nombre total de cellules :",
  ncell(grille_reference),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 8. CRÉATION DU MASQUE DES ZONES AUTORISÉES
# ------------------------------------------------------------------------------
#
# Toutes les parcelles autorisées reçoivent la valeur 1.
#
# Les zones hors des parcelles autorisées restent NA.
#
# IMPORTANT :
# Les cellules NA seront interdites au futur chemin.

parcelles_autorisees_vect <- vect(parcelles_autorisees)

masque_invekos <- rasterize(
  parcelles_autorisees_vect,
  grille_reference,
  field = 1,
  background = NA,
  touches = FALSE
)


# Comptage des cellules autorisées

nombre_cellules_autorisees <- global(
  !is.na(masque_invekos),
  "sum",
  na.rm = TRUE
)[1, 1]


cat(
  "Nombre de cellules autorisées :",
  nombre_cellules_autorisees,
  "\n"
)


# ------------------------------------------------------------------------------
# 9. RASTERISATION DU GROSS MARGIN
# ------------------------------------------------------------------------------
#
# Chaque cellule reçoit le gross margin de la parcelle dans laquelle
# elle se trouve.

raster_gross_margin <- rasterize(
  parcelles_autorisees_vect,
  grille_reference,
  field = "gross_marg",
  background = NA,
  touches = FALSE
)


# On applique explicitement le masque.
#
# Ainsi, même en cas de comportement inattendu lors de la rasterisation,
# aucune cellule extérieure à une parcelle autorisée ne peut recevoir
# une valeur utilisable.

raster_gross_margin <- mask(
  raster_gross_margin,
  masque_invekos
)


# Contrôle

valeurs_gm <- values(
  raster_gross_margin,
  na.rm = TRUE
)


cat("\n")
cat("====================================================\n")
cat("GROSS MARGIN\n")
cat("====================================================\n")

cat(
  "Minimum :",
  min(valeurs_gm),
  "\n"
)

cat(
  "Maximum :",
  max(valeurs_gm),
  "\n"
)


# ------------------------------------------------------------------------------
# 10. NORMALISATION DU GROSS MARGIN
# ------------------------------------------------------------------------------
#
# Formule :
#
# (valeur - minimum) / (maximum - minimum)
#
# Résultat :
#
# minimum = 0
# maximum = 1

gm_min <- min(valeurs_gm)
gm_max <- max(valeurs_gm)


if (gm_min == gm_max) {
  
  stop(
    "ERREUR : toutes les valeurs de gross margin sont identiques."
  )
  
}


gross_margin_normalise <- (
  raster_gross_margin - gm_min
) / (
  gm_max - gm_min
)


# Le masque est appliqué une nouvelle fois pour garantir que
# les zones interdites restent NA.

gross_margin_normalise <- mask(
  gross_margin_normalise,
  masque_invekos
)


# ------------------------------------------------------------------------------
# 11. CRÉATION DES BUFFERS AUTOUR DES OCCURRENCES
# ------------------------------------------------------------------------------
#
# Les occurrences polygonales sont volontairement ignorées.
#
# Chaque occurrence ponctuelle reçoit un buffer de 10 mètres.

cat("\n")
cat("====================================================\n")
cat("OCCURRENCES\n")
cat("====================================================\n")

cat(
  "Nombre d'occurrences ponctuelles :",
  nrow(occurrences_points),
  "\n"
)


buffers_occurrences <- st_buffer(
  occurrences_points,
  dist = BUFFER_OCCURRENCES
)


# ------------------------------------------------------------------------------
# 12. RASTERISATION DES OCCURRENCES
# ------------------------------------------------------------------------------
#
# Nous voulons compter le nombre de buffers qui touchent chaque cellule.
#
# Une cellule :
#
# 0 = aucune occurrence proche
# 1 = proche d'une occurrence
# 2 = proche de deux occurrences
# etc.

buffers_occurrences_vect <- vect(buffers_occurrences)


raster_occurrences <- rasterize(
  buffers_occurrences_vect,
  grille_reference,
  field = 1,
  fun = "sum",
  background = 0,
  touches = TRUE
)


# On applique le masque INVEKOS.
#
# Les occurrences situées hors des zones autorisées n'ont donc
# aucun effet sur le calcul du coût.

raster_occurrences <- mask(
  raster_occurrences,
  masque_invekos
)


# Contrôle

valeurs_occ <- values(
  raster_occurrences,
  na.rm = TRUE
)


cat(
  "Nombre maximal d'occurrences dans une cellule :",
  max(valeurs_occ),
  "\n"
)


# ------------------------------------------------------------------------------
# 13. NORMALISATION DES OCCURRENCES
# ------------------------------------------------------------------------------
#
# 0 occurrence = 0
#
# Nombre maximal observé = 1

occ_min <- min(valeurs_occ)
occ_max <- max(valeurs_occ)


if (occ_max == occ_min) {
  
  cat(
    "\nATTENTION : aucune variation dans les occurrences.\n"
  )
  
  occurrences_normalise <- raster_occurrences * 0
  
} else {
  
  occurrences_normalise <- (
    raster_occurrences - occ_min
  ) / (
    occ_max - occ_min
  )
  
}


occurrences_normalise <- mask(
  occurrences_normalise,
  masque_invekos
)


# ------------------------------------------------------------------------------
# 14. CONSTRUCTION DE LA SURFACE DE COÛT
# ------------------------------------------------------------------------------
#
# VERSION ACTUELLE :
#
# coût =
#
# poids_GM × gross_margin_normalisé
#
# +
#
# poids_occurrences × (1 - occurrences_normalisées)
#
#
# Donc :
#
# Gross margin élevé
# → coût élevé
#
# Beaucoup d'occurrences
# → coût plus faible

grille_cout <- (
  POIDS_GROSS_MARGIN *
    gross_margin_normalise
) +
  (
    POIDS_OCCURRENCES *
      (1 - occurrences_normalise)
  )


# ------------------------------------------------------------------------------
# 15. APPLICATION FINALE DU MASQUE
# ------------------------------------------------------------------------------
#
# Cette étape garantit définitivement :
#
# hors parcelles INVEKOS autorisées = NA
#
# Donc :
#
# passage impossible.

grille_cout <- mask(
  grille_cout,
  masque_invekos
)

# ------------------------------------------------------------------------------
# 15A. GARANTIE D'UN COÛT STRICTEMENT POSITIF
# ------------------------------------------------------------------------------
#
# leastcostpath ne doit recevoir aucune cellule accessible avec un coût
# inférieur ou égal à zéro.
#
# Les cellules interdites restent NA et ne sont donc PAS modifiées.
#
# Seules les cellules accessibles ayant éventuellement un coût égal à zéro
# reçoivent un très faible coût positif.

COUT_MINIMUM <- 0.001


# Identification des cellules accessibles ayant un coût <= 0

grille_cout <- ifel(
  
  !is.na(grille_cout) &
    grille_cout <= 0,
  
  COUT_MINIMUM,
  
  grille_cout
  
)


# Réapplication du masque par sécurité.
#
# Les zones interdites restent obligatoirement NA.

grille_cout <- mask(
  
  grille_cout,
  
  masque_invekos
  
)


# Contrôle immédiat

valeurs_cout_controle <- values(
  
  grille_cout,
  
  na.rm = TRUE
  
)


if (any(valeurs_cout_controle <= 0)) {
  
  stop(
    "ERREUR : certaines cellules accessibles possèdent encore un coût <= 0 ",
    "après correction."
  )
  
}


cat(
  "\nCoût minimum strictement positif garanti :",
  min(valeurs_cout_controle),
  "\n\n"
)

# ==============================================================================
# 15B. CONNEXION DES COMPOSANTES ACCESSIBLES SÉPARÉES PAR DE PETITS OBSTACLES
# ==============================================================================
#
# OBJECTIF
# --------
#
# Certaines zones INVEKOS autorisées sont séparées par de petits espaces
# actuellement interdits au passage :
#
# - routes ;
# - fossés ;
# - bandes techniques ;
# - petits espaces non couverts par une parcelle INVEKOS ;
# - autres discontinuités étroites.
#
# Ces espaces apparaissent comme des cellules NA dans la grille de coût.
#
# Le modèle autorise ici la création de corridors artificiels minimaux
# UNIQUEMENT lorsque deux composantes accessibles sont séparées par une
# distance maximale de 50 mètres.
#
# Une séparation supérieure à 50 mètres reste infranchissable.
#
# IMPORTANT :
#
# Cette version utilise une recherche spatiale avec index spatial afin
# d'éviter de comparer chaque composante à toutes les autres.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. PARAMÈTRES
# ------------------------------------------------------------------------------

# Distance maximale permettant la création d'un corridor artificiel.

DISTANCE_MAX_CORRIDOR <- 50


# Coût attribué aux cellules artificiellement ouvertes.
#
# La surface de coût normale est comprise approximativement entre :
#
# 0.001 et 1
#
# Le coût 2 est donc volontairement plus élevé.
#
# Le modèle privilégiera les parcelles INVEKOS normales et n'utilisera
# un corridor artificiel que lorsque celui-ci est nécessaire.

COUT_CORRIDOR <- 2


# ------------------------------------------------------------------------------
# 2. IDENTIFICATION DES CELLULES ACCESSIBLES
# ------------------------------------------------------------------------------
#
# Toute cellule dont le coût n'est pas NA est actuellement accessible.

zones_accessibles <- ifel(
  !is.na(grille_cout),
  1,
  NA
)


# ------------------------------------------------------------------------------
# 3. IDENTIFICATION DES COMPOSANTES CONNECTÉES
# ------------------------------------------------------------------------------
#
# Chaque groupe continu de cellules accessibles reçoit un identifiant.
#
# directions = 8 signifie que les cellules sont considérées comme connectées
# lorsqu'elles se touchent :
#
# - horizontalement ;
# - verticalement ;
# - diagonalement.

composantes <- patches(
  zones_accessibles,
  directions = 8
)


# Récupération des identifiants de composantes.

ids_composantes <- unique(
  values(
    composantes,
    na.rm = TRUE
  )
)


nombre_composantes_initial <- length(
  ids_composantes
)


cat("\n")
cat("====================================================\n")
cat("CONNEXION DES COMPOSANTES ACCESSIBLES\n")
cat("====================================================\n")

cat(
  "Nombre initial de composantes accessibles :",
  nombre_composantes_initial,
  "\n"
)


# ------------------------------------------------------------------------------
# 4. CONVERSION DES COMPOSANTES EN POLYGONES
# ------------------------------------------------------------------------------
#
# Chaque composante raster est transformée en un polygone.
#
# dissolve = TRUE permet de regrouper toutes les cellules appartenant
# à une même composante.

composantes_vect <- as.polygons(
  composantes,
  dissolve = TRUE,
  values = TRUE,
  na.rm = TRUE
)


# Conversion vers sf.

composantes_sf <- st_as_sf(
  composantes_vect
)


# ------------------------------------------------------------------------------
# 5. IDENTIFICATION DE LA COLONNE CONTENANT L'ID DES COMPOSANTES
# ------------------------------------------------------------------------------
#
# terra attribue automatiquement une colonne contenant la valeur de chaque
# composante.
#
# On récupère ici son nom automatiquement.

colonnes_attributs <- setdiff(
  names(composantes_sf),
  attr(composantes_sf, "sf_column")
)


# Vérification de sécurité.

if (length(colonnes_attributs) == 0) {
  
  stop(
    "ERREUR : aucune colonne d'identification des composantes n'a été trouvée."
  )
  
}


nom_colonne_composante <- colonnes_attributs[1]


# Création d'un identifiant explicite.

composantes_sf$component_id <- seq_len(
  nrow(composantes_sf)
)


cat(
  "Nombre de composantes transformées en polygones :",
  nrow(composantes_sf),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. RECHERCHE RAPIDE DES COMPOSANTES PROCHES
# ------------------------------------------------------------------------------
#
# Au lieu de calculer la distance entre TOUTES les composantes, on utilise
# st_is_within_distance().
#
# Cette fonction utilise la recherche spatiale pour identifier directement
# les composantes situées à DISTANCE_MAX_CORRIDOR mètres ou moins.
#
# Cela évite les boucles exhaustives très coûteuses.

voisins_proches <- st_is_within_distance(
  composantes_sf,
  composantes_sf,
  dist = DISTANCE_MAX_CORRIDOR,
  sparse = TRUE
)


# ------------------------------------------------------------------------------
# 7. CONSTRUCTION DE LA LISTE UNIQUE DES PAIRES PROCHES
# ------------------------------------------------------------------------------
#
# Une même relation apparaît deux fois :
#
# A -> B
# B -> A
#
# Nous conservons uniquement les paires uniques.

paires_proches <- list()


compteur_paires <- 0


for (i in seq_along(voisins_proches)) {
  
  voisins_i <- voisins_proches[[i]]
  
  
  # Suppression :
  #
  # - de la composante elle-même ;
  # - des paires déjà traitées.
  
  voisins_i <- voisins_i[
    voisins_i > i
  ]
  
  
  if (length(voisins_i) > 0) {
    
    for (j in voisins_i) {
      
      compteur_paires <- compteur_paires + 1
      
      
      paires_proches[[compteur_paires]] <- c(
        i,
        j
      )
    }
  }
}


cat(
  "Nombre de paires de composantes situées à <= ",
  DISTANCE_MAX_CORRIDOR,
  " m : ",
  compteur_paires,
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 8. CRÉATION DES CORRIDORS MINIMAUX
# ------------------------------------------------------------------------------
#
# Pour chaque paire proche :
#
# 1. on calcule la ligne entre les deux points les plus proches ;
# 2. cette ligne représente le corridor minimal ;
# 3. on vérifie une nouvelle fois sa longueur.
#
# La deuxième vérification est volontairement conservée comme sécurité.

corridors_liste <- list()


compteur_corridors <- 0


if (compteur_paires > 0) {
  
  for (k in seq_len(compteur_paires)) {
    
    i <- paires_proches[[k]][1]
    j <- paires_proches[[k]][2]
    
    
    geom_i <- st_geometry(
      composantes_sf[i, ]
    )
    
    
    geom_j <- st_geometry(
      composantes_sf[j, ]
    )
    
    
    # Ligne reliant les deux points les plus proches.
    
    ligne_corridor <- st_nearest_points(
      geom_i,
      geom_j
    )
    
    
    # Calcul de la longueur réelle du corridor.
    
    longueur_corridor <- as.numeric(
      st_length(
        ligne_corridor
      )
    )
    
    
    # Sécurité :
    #
    # Le corridor est conservé uniquement si sa longueur est :
    #
    # > 0
    # <= 50 m
    
    if (
      !is.na(longueur_corridor) &&
      longueur_corridor > 0 &&
      longueur_corridor <= DISTANCE_MAX_CORRIDOR
    ) {
      
      compteur_corridors <- compteur_corridors + 1
      
      
      corridors_liste[[compteur_corridors]] <-
        ligne_corridor[[1]]
    }
  }
}


cat(
  "Nombre de corridors effectivement créés :",
  compteur_corridors,
  "\n"
)


# ------------------------------------------------------------------------------
# 9. CONVERSION DES CORRIDORS EN OBJET SF
# ------------------------------------------------------------------------------

if (compteur_corridors > 0) {
  
  
  corridors_sf <- st_sf(
    
    corridor_id = seq_len(
      compteur_corridors
    ),
    
    geometry = st_sfc(
      corridors_liste,
      crs = st_crs(composantes_sf)
    )
  )
  
  
} else {
  
  
  corridors_sf <- NULL
  
}


# ------------------------------------------------------------------------------
# 10. RASTERISATION DES CORRIDORS
# ------------------------------------------------------------------------------
#
# Les corridors sont convertis en cellules raster sur la même grille que
# grille_cout.
#
# touches = TRUE permet de sélectionner toutes les cellules traversées
# ou touchées par un corridor.

if (!is.null(corridors_sf)) {
  
  
  corridors_vect <- vect(
    corridors_sf
  )
  
  
  raster_corridors <- rasterize(
    corridors_vect,
    grille_cout,
    field = 1,
    background = NA,
    touches = TRUE
  )
  
  
  # ---------------------------------------------------------------------------
  # 11. OUVERTURE UNIQUEMENT DES CELLULES ACTUELLEMENT INTERDITES
  # ---------------------------------------------------------------------------
  #
  # Les cellules déjà accessibles conservent leur coût d'origine.
  #
  # Seules les cellules NA traversées par un corridor deviennent accessibles.
  
  cellules_a_ouvrir <- ifel(
    
    is.na(grille_cout) &
      !is.na(raster_corridors),
    
    1,
    
    0
  )
  
  
  # Attribution du coût des corridors.
  
  grille_cout <- ifel(
    
    cellules_a_ouvrir == 1,
    
    COUT_CORRIDOR,
    
    grille_cout
  )
  
  
} else {
  
  
  cat(
    "Aucun corridor n'a été ajouté à la grille de coût.\n"
  )
}


# ------------------------------------------------------------------------------
# 12. VÉRIFICATION DU NOMBRE DE CELLULES OUVERTES
# ------------------------------------------------------------------------------

nombre_cellules_corridor <- sum(
  values(
    grille_cout
  ) == COUT_CORRIDOR,
  
  na.rm = TRUE
)


cat(
  "Nombre de cellules artificiellement ouvertes :",
  nombre_cellules_corridor,
  "\n"
)


cat(
  "Distance maximale autorisée :",
  DISTANCE_MAX_CORRIDOR,
  "m\n"
)


cat(
  "Coût attribué aux corridors :",
  COUT_CORRIDOR,
  "\n"
)


# ------------------------------------------------------------------------------
# 13. VÉRIFICATION DU NOMBRE DE COMPOSANTES APRÈS CORRIDORS
# ------------------------------------------------------------------------------
#
# Cette vérification est importante.
#
# Elle permet de savoir si les corridors ont réellement reconnecté certaines
# zones précédemment isolées.

zones_accessibles_apres_corridors <- ifel(
  !is.na(grille_cout),
  1,
  NA
)


composantes_apres_corridors <- patches(
  zones_accessibles_apres_corridors,
  directions = 8
)


ids_composantes_apres <- unique(
  values(
    composantes_apres_corridors,
    na.rm = TRUE
  )
)


nombre_composantes_apres <- length(
  ids_composantes_apres
)


cat(
  "Nombre de composantes après création des corridors :",
  nombre_composantes_apres,
  "\n"
)


cat(
  "Réduction du nombre de composantes :",
  nombre_composantes_initial -
    nombre_composantes_apres,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 14. CARTE DE CONTRÔLE DES CORRIDORS
# ------------------------------------------------------------------------------

grille_cout_controle_df <- as.data.frame(
  grille_cout,
  xy = TRUE,
  na.rm = TRUE
)


names(
  grille_cout_controle_df
)[3] <- "cout"


ggplot() +
  
  geom_raster(
    
    data = grille_cout_controle_df,
    
    aes(
      x = x,
      y = y,
      fill = cout
    )
  ) +
  
  geom_sf(
    
    data = patches,
    
    fill = NA,
    
    color = "white",
    
    linewidth = 0.3
  ) +
  
  {
    
    if (!is.null(corridors_sf)) {
      
      geom_sf(
        
        data = corridors_sf,
        
        color = "cyan",
        
        linewidth = 0.4
      )
    }
  } +
  
  coord_sf() +
  
  labs(
    
    title =
      "Surface de coût après connexion des petites discontinuités",
    
    subtitle =
      paste0(
        "Corridors créés uniquement lorsque deux composantes sont séparées par <= ",
        DISTANCE_MAX_CORRIDOR,
        " m"
      ),
    
    fill = "Coût"
  ) +
  
  theme_minimal()


# ------------------------------------------------------------------------------
# 15. FIN DU TRAITEMENT DES CORRIDORS
# ------------------------------------------------------------------------------

cat("====================================================\n")
cat("CORRIDORS TERMINÉS\n")
cat("====================================================\n\n")

# ==============================================================================
# DIAGNOSTIC 1 - COMPOSANTES CONNECTÉES APRÈS CRÉATION DES CORRIDORS
# ==============================================================================
#
# Objectif :
# Identifier les zones accessibles qui restent séparées les unes des autres
# après la création des corridors artificiels de <= 50 m.
#
# Une composante = un ensemble continu de cellules accessibles.
#
# Les cellules NA restent interdites et ne participent pas aux composantes.
# ==============================================================================


cat("\n")
cat("====================================================\n")
cat("DIAGNOSTIC DES COMPOSANTES ACCESSIBLES\n")
cat("====================================================\n")


# ------------------------------------------------------------------------------
# 1. CRÉATION D'UN RASTER BINAIRE D'ACCESSIBILITÉ
# ------------------------------------------------------------------------------
#
# 1 = cellule accessible
# NA = cellule interdite
#

raster_accessible <- ifel(
  !is.na(grille_cout),
  1,
  NA
)


# ------------------------------------------------------------------------------
# 2. IDENTIFICATION DES COMPOSANTES CONNECTÉES
# ------------------------------------------------------------------------------
#
# directions = 8 signifie qu'une cellule peut être connectée :
#
# - horizontalement
# - verticalement
# - diagonalement
#
# Ce choix est cohérent avec un déplacement spatial en raster.
#

composantes <- patches(
  raster_accessible,
  directions = 8,
  zeroAsNA = TRUE,
  allowGaps = FALSE
)


# ------------------------------------------------------------------------------
# 3. NOMBRE TOTAL DE COMPOSANTES
# ------------------------------------------------------------------------------

nombre_composantes <- global(
  composantes,
  "max",
  na.rm = TRUE
)[1, 1]


cat(
  "Nombre de composantes accessibles restantes :",
  nombre_composantes,
  "\n\n"
)

# ------------------------------------------------------------------------------
# 5D. CONSTRUCTION DE LA COUCHE DES CELLULES ACCESSIBLES
# ------------------------------------------------------------------------------
# Chaque cellule non-NA de la grille de coût devient un point (son centre),
# avec son numéro de cellule conservé. C'est sur cette couche que
# st_nearest_feature() cherchera, pour chaque point de bordure, la cellule
# accessible la plus proche.

toutes_valeurs   <- terra::values(grille_cout, mat = FALSE)
cellules_valides <- which(!is.na(toutes_valeurs))
coords_valides   <- terra::xyFromCell(grille_cout, cellules_valides)

points_accessibles <- st_as_sf(
  data.frame(
    cellule = cellules_valides,
    x = coords_valides[, 1],
    y = coords_valides[, 2]
  ),
  coords = c("x", "y"),
  crs = CRS_PROJET
)

cat("Nombre de cellules accessibles disponibles pour le rattachement :",
    nrow(points_accessibles), "\n\n")

# ------------------------------------------------------------------------------
# 4. NOMBRE DE CELLULES PAR COMPOSANTE
# ------------------------------------------------------------------------------

# terra::freq() renvoie les fréquences des valeurs présentes dans le raster.
#
# Dans notre cas :
#
# - value = identifiant de la composante
# - count = nombre de cellules appartenant à cette composante
#
# La colonne éventuelle "layer" est supprimée car elle n'est pas utile
# pour le diagnostic.

table_composantes <- as.data.frame(
  terra::freq(
    composantes
  )
)


# On conserve uniquement :
#
# - l'identifiant de la composante
# - le nombre de cellules

table_composantes <- table_composantes %>%
  select(
    composante_id = value,
    nombre_cellules = count
  )

# ------------------------------------------------------------------------------
# 5. CALCUL DE LA SURFACE
# ------------------------------------------------------------------------------
#
# La résolution est de 10 m.
#
# Une cellule = 10 x 10 = 100 m².
#

surface_cellule_m2 <- RESOLUTION^2


table_composantes$surface_m2 <-
  table_composantes$nombre_cellules *
  surface_cellule_m2


table_composantes$surface_ha <-
  table_composantes$surface_m2 / 10000


# ------------------------------------------------------------------------------
# 6. TRI DE LA PLUS GRANDE À LA PLUS PETITE
# ------------------------------------------------------------------------------

table_composantes <- table_composantes %>%
  arrange(
    desc(nombre_cellules)
  )


# ------------------------------------------------------------------------------
# 7. AFFICHAGE DES RÉSULTATS
# ------------------------------------------------------------------------------

cat("TAILLE DES COMPOSANTES ACCESSIBLES :\n\n")

print(
  table_composantes
)


# ------------------------------------------------------------------------------
# 8. IDENTIFICATION DE LA COMPOSANTE PRINCIPALE
# ------------------------------------------------------------------------------

composante_principale <-
  table_composantes$composante_id[1]


cat("\n")

cat(
  "Composante principale :",
  composante_principale,
  "\n"
)

cat(
  "Nombre de cellules :",
  table_composantes$nombre_cellules[1],
  "\n"
)

cat(
  "Surface approximative (ha) :",
  round(
    table_composantes$surface_ha[1],
    2
  ),
  "\n\n"
)

# ------------------------------------------------------------------------------
# 9. CARTE DES COMPOSANTES RESTANTES
# ------------------------------------------------------------------------------

composantes_df <- as.data.frame(
  composantes,
  xy = TRUE,
  na.rm = TRUE
)


names(composantes_df)[3] <- "composante_id"


ggplot() +
  
  geom_raster(
    data = composantes_df,
    aes(
      x = x,
      y = y,
      fill = factor(composante_id)
    )
  ) +
  
  geom_sf(
    data = patches,
    fill = NA,
    color = "black",
    linewidth = 0.25
  ) +
  
  coord_sf() +
  
  labs(
    title =
      "Composantes accessibles après création des corridors",
    
    subtitle =
      paste(
        nombre_composantes,
        "composantes accessibles restantes"
      ),
    
    fill =
      "Composante"
  ) +
  
  theme_minimal()

# ------------------------------------------------------------------------------
# 16. VÉRIFICATION DES VALEURS FINALES
# ------------------------------------------------------------------------------

valeurs_cout <- values(
  grille_cout,
  na.rm = TRUE
)


cat("\n")
cat("====================================================\n")
cat("SURFACE DE COÛT FINALE\n")
cat("====================================================\n")

cat(
  "Coût minimum :",
  min(valeurs_cout),
  "\n"
)

cat(
  "Coût maximum :",
  max(valeurs_cout),
  "\n"
)

cat(
  "Nombre de cellules accessibles :",
  sum(!is.na(values(grille_cout))),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 17. CARTE 1 : PARCELLES AUTORISÉES ET INTERDITES
# ------------------------------------------------------------------------------

parcelles_invekos_modele$statut_modele <- ifelse(
  !is.na(parcelles_invekos_modele$gross_marg) &
    parcelles_invekos_modele$gross_marg > 0,
  "Autorisee",
  "Interdite"
)

ggplot() +
  
  geom_sf(
    data = parcelles_invekos_modele,
    aes(fill = statut_modele),
    color = NA
  ) +
  
  geom_sf(
    data = patches,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  
  coord_sf() +
  
  labs(
    title = "Parcelles accessibles au modèle",
    subtitle =
      "Accessible : gross margin > 0 | Interdite : NA ou gross margin <= 0",
    fill = "Statut"
  ) +
  
  theme_minimal()


# ------------------------------------------------------------------------------
# 18. CARTE 2 : GROSS MARGIN NORMALISÉ
# ------------------------------------------------------------------------------

gross_margin_df <- as.data.frame(
  gross_margin_normalise,
  xy = TRUE,
  na.rm = TRUE
)

names(gross_margin_df)[3] <- "gross_margin_normalise"


ggplot() +
  
  geom_raster(
    data = gross_margin_df,
    aes(
      x = x,
      y = y,
      fill = gross_margin_normalise
    )
  ) +
  
  geom_sf(
    data = patches,
    fill = NA,
    color = "white",
    linewidth = 0.3
  ) +
  
  scale_fill_viridis_c(
    name = "Gross margin\normalisé"
  ) +
  
  coord_sf() +
  
  labs(
    title = "Gross margin normalisé",
    subtitle =
      "0 = coût économique faible | 1 = coût économique élevé"
  ) +
  
  theme_minimal()


# ------------------------------------------------------------------------------
# 19. CARTE 3 : OCCURRENCES ET BUFFERS
# ------------------------------------------------------------------------------

ggplot() +
  
  geom_sf(
    data = parcelles_autorisees,
    fill = "grey90",
    color = NA
  ) +
  
  geom_sf(
    data = buffers_occurrences,
    fill = NA,
    color = "blue",
    linewidth = 0.1,
    alpha = 0.3
  ) +
  
  geom_sf(
    data = occurrences_points,
    color = "red",
    size = 0.3
  ) +
  
  coord_sf() +
  
  labs(
    title = "Occurrences ponctuelles et buffers",
    subtitle =
      paste(
        "Buffer de",
        BUFFER_OCCURRENCES,
        "mètres autour de chaque occurrence"
      )
  ) +
  
  theme_minimal()


# ------------------------------------------------------------------------------
# 20. CARTE 4 : SURFACE DE COÛT FINALE
# ------------------------------------------------------------------------------

grille_cout_df <- as.data.frame(
  grille_cout,
  xy = TRUE,
  na.rm = TRUE
)

names(grille_cout_df)[3] <- "cout"


ggplot() +
  
  geom_raster(
    data = grille_cout_df,
    aes(
      x = x,
      y = y,
      fill = cout
    )
  ) +
  
  geom_sf(
    data = patches,
    fill = NA,
    color = "white",
    linewidth = 0.4
  ) +
  
  scale_fill_viridis_c(
    name = "Coût",
    option = "magma"
  ) +
  
  coord_sf() +
  
  labs(
    title = "Surface de coût finale",
    subtitle =
      "Seules les parcelles INVEKOS autorisées sont accessibles"
  ) +
  
  theme_minimal()


# ------------------------------------------------------------------------------
# 21. SAUVEGARDE DES RÉSULTATS
# ------------------------------------------------------------------------------

# Surface de coût principale

chemin_grille_cout <- file.path(
  DOSSIER_DONNEES,
  "grille_cout.tif"
)


writeRaster(
  grille_cout,
  chemin_grille_cout,
  overwrite = TRUE
)


# Masque des zones accessibles

chemin_masque <- file.path(
  DOSSIER_DONNEES,
  "masque_invekos_autorise.tif"
)


writeRaster(
  masque_invekos,
  chemin_masque,
  overwrite = TRUE
)


# Gross margin normalisé

chemin_gm <- file.path(
  DOSSIER_DONNEES,
  "gross_margin_normalise.tif"
)


writeRaster(
  gross_margin_normalise,
  chemin_gm,
  overwrite = TRUE
)


# Occurrences normalisées

chemin_occ <- file.path(
  DOSSIER_DONNEES,
  "occurrences_normalisees.tif"
)


writeRaster(
  occurrences_normalise,
  chemin_occ,
  overwrite = TRUE
)


cat("\n")
cat("====================================================\n")
cat("SCRIPT 02 TERMINÉ\n")
cat("====================================================\n")

cat(
  "Surface de coût :",
  chemin_grille_cout,
  "\n"
)

cat(
  "Masque INVEKOS :",
  chemin_masque,
  "\n"
)

cat(
  "Gross margin :",
  chemin_gm,
  "\n"
)

cat(
  "Occurrences :",
  chemin_occ,
  "\n"
)

cat("\n")


# ==============================================================================
# PROCHAINE ÉTAPE :
#
# SCRIPT 03
#
# Calcul des chemins de moindre coût avec leastcostpath.
#
# Les chemins devront rester exclusivement sur les cellules non-NA
# de la grille de coût.
# ==============================================================================

cat(
  "Nombre maximal d'occurrences dans une cellule :",
  max(valeurs_occ),
  
  "\n"
)


