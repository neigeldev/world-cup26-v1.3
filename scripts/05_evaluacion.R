# ══════════════════════════════════════════════════════════════════
# FASE 5: EVALUACIÓN
# Objetivo: validar clusters, analizar perfiles, exportar resultados
# ══════════════════════════════════════════════════════════════════
library(tidyverse)
library(cluster)
library(factoextra)
library(patchwork)

pos_fwd <- readRDS(here::here("models/resultado_fwd.rds"))
pos_mid <- readRDS(here::here("models/resultado_mid.rds"))
pos_def <- readRDS(here::here("models/resultado_def.rds"))
pos_gk  <- readRDS(here::here("models/resultado_gk.rds"))
df_ranking <- readRDS(here::here("models/ranking_global.rds"))

# Mismos k que en 04_mineria.R (obligatorio para silhouette coherente)
k_fwd <- 3L
k_mid <- 3L
k_def <- 3L
k_gk  <- 2L

# ── 1. Silhouette (re-PAM sobre la misma matriz y k) ─────────────
sil_fwd <- silhouette(pam(pos_fwd$mat, k = k_fwd))
sil_mid <- silhouette(pam(pos_mid$mat, k = k_mid))
sil_def <- silhouette(pam(pos_def$mat, k = k_def))
sil_gk  <- silhouette(pam(pos_gk$mat,  k = k_gk))

cat("Silhouette FWD:", round(mean(sil_fwd[, 3]), 3), "\n")
cat("Silhouette MID:", round(mean(sil_mid[, 3]), 3), "\n")
cat("Silhouette DEF:", round(mean(sil_def[, 3]), 3), "\n")
cat("Silhouette GK: ", round(mean(sil_gk[, 3]),  3), "\n")
# > 0.50 = estructura aceptable | > 0.65 = buena

# ── 2. Top 10 por posición ───────────────────────────────────────
top_por_posicion <- df_ranking %>%
  group_by(macro_pos) %>%
  slice_max(score_impacto, n = 10) %>%
  select(rank_global, short_name, nationality_name,
         macro_pos, cluster, overall, score_impacto)

print(top_por_posicion, n = 40)

# ── 3. Perfil medio por cluster (atributos tácticos) ────────────
features_fwd <- c(
  "attacking_finishing", "mentality_positioning",
  "power_shot_power", "power_long_shots", "skill_dribbling"
)

features_mid <- c(
  "attacking_short_passing", "skill_long_passing",
  "mentality_vision", "skill_ball_control", "mentality_interceptions"
)

features_def <- c(
  "defending_marking_awareness", "defending_standing_tackle",
  "mentality_interceptions", "power_strength", "power_jumping"
)

features_gk <- c(
  "goalkeeping_diving", "goalkeeping_handling",
  "goalkeeping_reflexes", "goalkeeping_positioning", "movement_reactions"
)

perfil_cluster <- function(pos_obj, features) {
  pos_obj$df %>%
    group_by(cluster_num) %>%
    summarise(
      n           = n(),
      overall_med = round(median(overall), 1),
      across(all_of(features), ~ round(mean(.x, na.rm = TRUE), 1)),
      .groups     = "drop"
    )
}

cat("\n-- Perfiles FWD --\n")
print(perfil_cluster(pos_fwd, features_fwd))

cat("\n-- Perfiles MID --\n")
print(perfil_cluster(pos_mid, features_mid))

cat("\n-- Perfiles DEF --\n")
print(perfil_cluster(pos_def, features_def))

cat("\n-- Perfiles GK --\n")
print(perfil_cluster(pos_gk, features_gk))

# ── 4. Exportar resultados finales ───────────────────────────────
dir.create(here::here("outputs/tables"), showWarnings = FALSE, recursive = TRUE)

write_csv(df_ranking, here::here("outputs/tables/ranking_global.csv"))
write_csv(top_por_posicion, here::here("outputs/tables/top10_por_posicion.csv"))

cat("[OK] Evaluacion completa - archivos exportados\n")
