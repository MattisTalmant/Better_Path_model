# ==============================================================================
# MISSION ANNEXE - Classification des espèces menacées selon leur réaction
# à la reconnexion des pâturages (bénéfique / neutre / défavorable)
#
# ÉTAPE A : extraction de la liste des espèces distinctes présentes dans la
# couche d'occurrences ponctuelles, en vue de leur classification.
#
# Ce script est INDÉPENDANT des scripts 01/02 - pas besoin de les avoir
# exécutés avant. Adapte juste DOSSIER_DONNEES si besoin.
# ==============================================================================

library(sf)
library(dplyr)

DOSSIER_DONNEES <- "~/Desktop/Data_POM"   # ⚠️ adapte ce chemin si besoin


# ------------------------------------------------------------------------------
# 1. IMPORT DE LA COUCHE PONCTUELLE D'OCCURRENCES
# ------------------------------------------------------------------------------

occurrences_points <- st_read(
  file.path(DOSSIER_DONNEES, "a17_spec_B.gpkg"),
  layer = "a17_pt_spec"
)


# ------------------------------------------------------------------------------
# 2. VÉRIFICATION DE LA COLONNE "nameori"
# ------------------------------------------------------------------------------

if (!"nameori" %in% names(occurrences_points)) {
  stop("La colonne 'nameori' n'existe pas dans cette couche. Colonnes disponibles : ",
       paste(names(occurrences_points), collapse = ", "))
}


# ------------------------------------------------------------------------------
# 3. EXTRACTION DE LA LISTE DES ESPÈCES UNIQUES, AVEC LEUR NOMBRE D'OCCURRENCES
# ------------------------------------------------------------------------------

liste_especes <- occurrences_points |>
  st_drop_geometry() |>
  count(nameori, name = "nb_occurrences") |>
  arrange(desc(nb_occurrences))

cat("Nombre d'espèces distinctes trouvées :", nrow(liste_especes), "\n\n")
print(liste_especes)


# ------------------------------------------------------------------------------
# 4. EXPORT EN CSV POUR CLASSIFICATION
# ------------------------------------------------------------------------------
# On ajoute une colonne vide "categorie", à remplir avec 1, 2 ou 3 :
#   1 = espèce qui bénéficierait de la reconnexion
#   2 = espèce neutre à la reconnexion
#   3 = espèce mise en danger activement par la reconnexion -> à exclure du
#       modèle et à éviter par le chemin

liste_especes$categorie <- NA

chemin_csv <- file.path(DOSSIER_DONNEES, "especes_a_classifier.csv")
write.csv(liste_especes, chemin_csv, row.names = FALSE, fileEncoding = "UTF-8")

cat("Fichier exporté :", chemin_csv, "\n")
cat("Ouvre-le (Excel, Numbers, ou directement dans RStudio via View()) et\n")
cat("remplis la colonne 'categorie' avec 1, 2 ou 3 pour CHAQUE espèce.\n\n")

cat("Envoie-moi ensuite la liste des espèces (copie-colle le résultat de\n")
cat("print(liste_especes) affiché ci-dessus dans la console) pour qu'on\n")
cat("travaille ensemble à leur classification avant de remplir le CSV.\n")

# ==============================================================================
# MISSION ANNEXE - Classification des espèces menacées
#
# ÉTAPE A2 : nettoyage et consolidation de la liste d'espèces brute, avant
# classification. Beaucoup d'entrées de "nameori" sont en réalité la même
# espèce écrite différemment (citations d'auteur, noms allemands, codes
# Natura 2000, complexes d'espèces) - ce script les regroupe autant que
# possible et isole les cas ambigus pour vérification manuelle.
#
# Prérequis : avoir déjà exécuté 00_classification_especes.R (liste_especes
# doit exister dans ton environnement), ou relance-le avant celui-ci.
# ==============================================================================

library(dplyr)
library(stringr)


# ------------------------------------------------------------------------------
# 1. NETTOYAGE DU TEXTE
# ------------------------------------------------------------------------------

liste_especes_nettoyee <- liste_especes |>
  mutate(
    # Retire tout ce qui est entre parenthèses : "(Linnaeus, 1758)" etc.
    nom_nettoye = str_remove_all(nameori, "\\([^)]*\\)"),
    # Retire les citations d'auteur SANS parenthèses : ", Linnaeus, 1758" etc.
    nom_nettoye = str_remove(nom_nettoye, ",\\s*[A-Z][a-zé]+.*$"),
    # Retire les espaces multiples et les espaces en début/fin
    nom_nettoye = str_squish(nom_nettoye)
  )


# ------------------------------------------------------------------------------
# 2. DÉTECTION DES ENTRÉES SUSPECTES (à vérifier manuellement)
# ------------------------------------------------------------------------------
# Une entrée est jugée "suspecte" si elle :
#   - est purement numérique (probable code Natura 2000)
#   - ne contient qu'un seul mot en minuscules/majuscules mêlées (probable
#     abréviation ou nom vernaculaire)
#   - contient une virgule (probable complexe de plusieurs espèces)
#   - est très courte (1-2 caractères, probable erreur de saisie)
#   - est NA (valeur manquante)

liste_especes_nettoyee <- liste_especes_nettoyee |>
  mutate(
    suspect = case_when(
      is.na(nom_nettoye)                          ~ "valeur manquante (NA)",
      str_detect(nom_nettoye, "^[0-9]+$")          ~ "code numérique (probable Natura 2000)",
      str_detect(nom_nettoye, ",")                 ~ "complexe de plusieurs espèces",
      nchar(nom_nettoye) <= 3                      ~ "trop court / probable erreur",
      !str_detect(nom_nettoye, "^[A-Z][a-zäöü]+ ") ~ "ne ressemble pas à 'Genre espèce' (nom vernaculaire ?)",
      TRUE                                          ~ NA_character_
    )
  )

n_suspectes <- sum(!is.na(liste_especes_nettoyee$suspect))
cat("Entrées suspectes détectées :", n_suspectes, "/", nrow(liste_especes_nettoyee), "\n")
cat("(À vérifier à la main - regarde le fichier especes_suspectes.csv)\n\n")


# ------------------------------------------------------------------------------
# 3. CONSOLIDATION DES DOUBLONS (entrées non suspectes uniquement)
# ------------------------------------------------------------------------------
# On regroupe par nom nettoyé et on additionne les occurrences, pour les
# entrées qui ressemblent à un vrai nom scientifique "Genre espèce".

liste_consolidee <- liste_especes_nettoyee |>
  filter(is.na(suspect)) |>
  group_by(nom_nettoye) |>
  summarise(
    nb_occurrences_total = sum(nb_occurrences),
    variantes_regroupees = paste(unique(nameori), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(desc(nb_occurrences_total))

cat("Nombre d'espèces uniques après consolidation :", nrow(liste_consolidee), "\n")
cat("(contre", nrow(liste_especes), "lignes brutes de départ)\n\n")


# ------------------------------------------------------------------------------
# 4. EXPORT DES DEUX FICHIERS
# ------------------------------------------------------------------------------

liste_consolidee$categorie <- NA
chemin_consolide <- file.path(DOSSIER_DONNEES, "especes_consolidees_a_classifier.csv")
write.csv(liste_consolidee, chemin_consolide, row.names = FALSE, fileEncoding = "UTF-8")

especes_suspectes <- liste_especes_nettoyee |> filter(!is.na(suspect))
chemin_suspectes <- file.path(DOSSIER_DONNEES, "especes_suspectes.csv")
write.csv(especes_suspectes, chemin_suspectes, row.names = FALSE, fileEncoding = "UTF-8")

cat("Fichier principal (à classifier) :", chemin_consolide, "\n")
cat("Fichier des cas ambigus (à vérifier à la main) :", chemin_suspectes, "\n\n")
cat("Regarde d'abord especes_suspectes.csv : certaines lignes sont peut-être\n")
cat("en réalité un doublon d'une espèce déjà présente dans la liste principale\n")
cat("(ex. un nom allemand à traduire), d'autres sont clairement à ignorer\n")
cat("(codes, texte de localisation, saisies erronées).\n")