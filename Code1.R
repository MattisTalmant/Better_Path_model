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
# 5. IDENTIFIANT UNIQUE POUR CHAQUE PATCH
# ------------------------------------------------------------------------------
# On crée un identifiant simple et fiable (patch_id = 1, 2, 3...) pour
# pouvoir référencer chaque patch sans ambiguïté dans les étapes suivantes,
# indépendamment de tout ID déjà présent (ou absent) dans ton shapefile.

patches$patch_id <- seq_len(nrow(patches))

cat("Nombre de patches importés :", nrow(patches), "\n\n")


# ------------------------------------------------------------------------------
# 6. PAIRES DE PATCHES À CONNECTER (liste définie par toi)
# ------------------------------------------------------------------------------
# Contrairement à une génération automatique de toutes les combinaisons
# possibles, tu fournis ici la liste précise des paires de patches que tu
# veux relier — probablement issue de ta propre analyse de connectivité
# écologique ou de ta connaissance du terrain.
#
# DEUX FAÇONS DE FOURNIR CETTE LISTE :
#
# OPTION A - directement dans le script (pratique si la liste est courte
# et stable). Utilise les valeurs de patch_id créées à l'étape 5 ci-dessus
# (regarde la carte de contrôle avec les patch_id affichés si besoin de les
# identifier visuellement - voir étape 10).
#
# paires_patches <- data.frame(
#   patch_A = c(1, 1, 3, 5),
#   patch_B = c(2, 4, 4, 6)
# )
#
# OPTION B - depuis un fichier CSV externe (pratique si la liste est longue
# ou amenée à changer souvent). Le CSV doit avoir deux colonnes : patch_A
# et patch_B, contenant les patch_id des deux patches de chaque paire.
#
# paires_patches <- read.csv(file.path(DOSSIER_DONNEES, "paires_a_connecter.csv"))

# ⚠️ REMPLACE la ligne ci-dessous par l'option A ou B ci-dessus, selon ta
# préférence. Je laisse un exemple minimal en attendant pour que le script
# reste exécutable de bout en bout :
paires_patches <- data.frame(
  patch_A = integer(0),
  patch_B = integer(0)
)

# Vérification : tous les patch_id mentionnés dans paires_patches doivent
# exister réellement dans la couche patches (sinon faute de frappe probable).
ids_manquants <- setdiff(
  unique(c(paires_patches$patch_A, paires_patches$patch_B)),
  patches$patch_id
)
if (length(ids_manquants) > 0) {
  warning("Ces patch_id n'existent pas dans la couche 'patches' : ",
          paste(ids_manquants, collapse = ", "))
}

cat("Nombre de paires de patches à connecter :", nrow(paires_patches), "\n\n")


# ------------------------------------------------------------------------------
# 7. FONCTION : POINTS DE BORDURE LES PLUS PROCHES ENTRE DEUX PATCHES
# ------------------------------------------------------------------------------
# st_nearest_points(geom1, geom2) renvoie la LIGNE LA PLUS COURTE entre les
# deux géométries (ici deux polygones). Les deux extrémités de cette ligne
# sont exactement ce que tu cherches : le point de la bordure de A le plus
# proche de B, et le point de la bordure de B le plus proche de A.
#
# ⚠️ IMPORTANT : cette ligne droite n'est PAS le chemin final. Elle sert
# uniquement à définir le point de départ et le point d'arrivée précis pour
# le calcul du chemin de moindre coût (fait à l'étape 4 du projet, sur la
# grille de coût). Le vrai chemin, lui, pourra dévier de cette ligne droite
# en fonction du coût du terrain.

obtenir_points_bordure <- function(patch_a_geom, patch_b_geom) {
  ligne_plus_courte <- st_nearest_points(patch_a_geom, patch_b_geom)
  point_depart  <- st_startpoint(ligne_plus_courte)   # point sur la bordure de A
  point_arrivee <- st_endpoint(ligne_plus_courte)     # point sur la bordure de B
  list(depart = point_depart, arrivee = point_arrivee)
}


# ------------------------------------------------------------------------------
# 8. CALCUL DES POINTS DE BORDURE POUR CHAQUE PAIRE
# ------------------------------------------------------------------------------

if (nrow(paires_patches) > 0) {

  liste_points_depart  <- vector("list", nrow(paires_patches))
  liste_points_arrivee <- vector("list", nrow(paires_patches))

  for (i in seq_len(nrow(paires_patches))) {

    id_a <- paires_patches$patch_A[i]
    id_b <- paires_patches$patch_B[i]

    geom_a <- patches[patches$patch_id == id_a, ]
    geom_b <- patches[patches$patch_id == id_b, ]

    points <- obtenir_points_bordure(st_geometry(geom_a), st_geometry(geom_b))

    liste_points_depart[[i]]  <- points$depart
    liste_points_arrivee[[i]] <- points$arrivee
  }

  points_bordure <- st_sf(
    patch_A = paires_patches$patch_A,
    patch_B = paires_patches$patch_B,
    geometry_depart  = st_sfc(unlist(liste_points_depart, recursive = FALSE), crs = CRS_PROJET),
    geometry_arrivee = st_sfc(unlist(liste_points_arrivee, recursive = FALSE), crs = CRS_PROJET)
  )

  cat("Points de bordure calculés pour", nrow(points_bordure), "paires de patches.\n\n")

} else {

  points_bordure <- NULL
  cat("paires_patches est vide : aucun point de bordure à calculer pour l'instant.\n")
  cat("Remplis ta liste à l'étape 6, puis relance le script.\n\n")

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
# On affiche les patches, et pour chaque paire fournie, un segment reliant
# les deux points de bordure retenus. Ça permet de vérifier visuellement que
# les points tombent bien là où on les attend (sur les bordures, au bon
# endroit). Si paires_patches est vide (exemple par défaut), cette section
# ne produira rien tant que tu n'as pas rempli ta liste à l'étape 6.

if (nrow(paires_patches) > 0) {
  
  segments_controle <- st_sf(
    patch_A = points_bordure$patch_A,
    patch_B = points_bordure$patch_B,
    geometry = mapply(
      function(depart, arrivee) st_sfc(st_linestring(rbind(depart, arrivee)), crs = CRS_PROJET),
      st_geometry(points_bordure$geometry_depart),
      st_geometry(points_bordure$geometry_arrivee),
      SIMPLIFY = FALSE
    ) |> do.call(c, args = _)
  )
  
  ggplot() +
    geom_sf(data = zone_etude, fill = NA, color = "black", linewidth = 0.8) +
    geom_sf(data = patches, fill = "forestgreen", alpha = 0.5) +
    geom_sf(data = segments_controle, color = "orange", linewidth = 0.4, linetype = "dashed") +
    geom_sf(data = points_bordure$geometry_depart, color = "darkgreen", size = 1) +
    geom_sf(data = points_bordure$geometry_arrivee, color = "darkgreen", size = 1) +
    geom_sf(data = occurrences_points, color = "red", size = 1, shape = 4) +
    labs(title = "Contrôle : points de bordure les plus proches entre paires de patches",
         subtitle = "Pointillés oranges = ligne la plus courte entre bordures (pas le chemin final)") +
    theme_minimal()
  
} else {
  cat("paires_patches est vide : remplis ta liste à l'étape 6 pour voir cette carte.\n")
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
  "parcelles_invekos",
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
  nrow(parcelles_invekos),
  "\n"
)

cat(
  "Parcelles avec gross_marg NA :",
  sum(is.na(parcelles_invekos$gross_marg)),
  "\n"
)

cat(
  "Parcelles avec gross_marg <= 0 :",
  sum(
    !is.na(parcelles_invekos$gross_marg) &
      parcelles_invekos$gross_marg <= 0
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

parcelles_autorisees <- parcelles_invekos %>%
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
  nrow(parcelles_invekos) -
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
# 15 BIS. REMPLACEMENT DES COÛTS NULS
# ------------------------------------------------------------------------------
#
# Certaines cellules ont actuellement un coût égal à 0.
#
# Pour éviter qu'un déplacement soit considéré comme totalement gratuit
# par l'algorithme de chemin de moindre coût, ces cellules reçoivent
# un coût minimal de 0.001.
#
# Cette valeur est suffisamment faible pour conserver leur caractère
# très favorable dans le modèle.

nombre_couts_nuls <- sum(
  values(grille_cout) == 0,
  na.rm = TRUE
)

cat(
  "Nombre de cellules avec un coût nul avant correction :",
  nombre_couts_nuls,
  "\n"
)


# Remplacement des coûts 0 par 0.001

grille_cout[grille_cout == 0] <- 0.001


# Vérification

nombre_couts_nuls_apres <- sum(
  values(grille_cout) == 0,
  na.rm = TRUE
)

cout_min_apres <- min(
  values(grille_cout),
  na.rm = TRUE
)

cat(
  "Nombre de cellules avec un coût nul après correction :",
  nombre_couts_nuls_apres,
  "\n"
)

cat(
  "Nouveau coût minimum :",
  cout_min_apres,
  "\n\n"
)
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

parcelles_invekos$statut_modele <- ifelse(
  !is.na(parcelles_invekos$gross_marg) &
    parcelles_invekos$gross_marg > 0,
  "Autorisee",
  "Interdite"
)


ggplot() +
  
  geom_sf(
    data = parcelles_invekos,
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
    name = "Gross margin\nnormalisé"
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

