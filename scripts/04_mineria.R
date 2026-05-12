# ══════════════════════════════════════════════════════════════════
# FASE 4: MINERÍA — PAM sobre componentes PCA + perfiles y score
# ══════════════════════════════════════════════════════════════════
# Nota: el score debe calcularse por separado por macro_pos porque cada
# pos_fwd$df / pos_mid$df / ... solo contiene el subconjunto de columnas
# usado en PCA (case_when() evalúa RHS que referencian columnas que no
# existen en el tibble → error "objeto no encontrado").
# ══════════════════════════════════════════════════════════════════
library(tidyverse)
library(cluster)
library(factoextra)

pos_fwd <- readRDS(here::here("data/04_processed/pos_fwd.rds"))
pos_mid <- readRDS(here::here("data/04_processed/pos_mid.rds"))
pos_def <- readRDS(here::here("data/04_processed/pos_def.rds"))
pos_gk  <- readRDS(here::here("data/04_processed/pos_gk.rds"))

# ── 1. K definitivos ─────────────────────────────────────────────
k_fwd <- 3L
k_mid <- 3L
k_def <- 3L
k_gk  <- 2L

set.seed(42)
clust_fwd <- pam(pos_fwd$mat, k = k_fwd)
clust_mid <- pam(pos_mid$mat, k = k_mid)
clust_def <- pam(pos_def$mat, k = k_def)
clust_gk  <- pam(pos_gk$mat,  k = k_gk)

# ── 2. Cluster en el dataframe de identidad ──────────────────────
pos_fwd$df <- pos_fwd$df %>% mutate(cluster_num = clust_fwd$clustering)
pos_mid$df <- pos_mid$df %>% mutate(cluster_num = clust_mid$clustering)
pos_def$df <- pos_def$df %>% mutate(cluster_num = clust_def$clustering)
pos_gk$df  <- pos_gk$df  %>% mutate(cluster_num = clust_gk$clustering)

# ── 3. Medioides ─────────────────────────────────────────────────
cat("\n-- Medioides FWD --\n")
pos_fwd$df %>%
  slice(clust_fwd$id.med) %>%
  select(short_name, nationality_name, overall, cluster_num) %>%
  print()

cat("\n-- Medioides MID --\n")
pos_mid$df %>%
  slice(clust_mid$id.med) %>%
  select(short_name, nationality_name, overall, cluster_num) %>%
  print()

cat("\n-- Medioides DEF --\n")
pos_def$df %>%
  slice(clust_def$id.med) %>%
  select(short_name, nationality_name, overall, cluster_num) %>%
  print()

cat("\n-- Medioides GK --\n")
pos_gk$df %>%
  slice(clust_gk$id.med) %>%
  select(short_name, nationality_name, overall, cluster_num) %>%
  print()

# ── 4. Perfil medio por cluster (atributos tácticos) ────────────
perfil_medio <- function(pos_obj, tac_features, titulo) {
  cat("\n==", titulo, "==\n")
  pos_obj$df %>%
    group_by(cluster_num) %>%
    summarise(
      n           = n(),
      overall_med = round(median(overall), 1),
      across(all_of(tac_features[1:6]),
             ~ round(mean(.x, na.rm = TRUE), 1)),
      .groups     = "drop"
    ) %>%
    print()
}

tac_fwd <- c(
  "attacking_finishing", "mentality_positioning",
  "power_shot_power", "power_long_shots",
  "attacking_heading_accuracy", "skill_dribbling",
  "movement_acceleration", "movement_sprint_speed",
  "skill_ball_control", "attacking_crossing",
  "mentality_composure", "power_stamina", "power_jumping"
)

tac_mid <- c(
  "attacking_short_passing", "skill_long_passing",
  "mentality_vision", "skill_ball_control",
  "attacking_crossing", "mentality_interceptions",
  "defending_standing_tackle", "mentality_aggression",
  "power_stamina", "movement_reactions",
  "movement_agility", "power_long_shots",
  "mentality_positioning", "mentality_composure"
)

tac_def <- c(
  "defending_marking_awareness", "defending_standing_tackle",
  "mentality_interceptions", "mentality_aggression",
  "power_jumping", "power_strength", "power_stamina",
  "attacking_short_passing", "skill_long_passing",
  "movement_acceleration", "movement_sprint_speed",
  "attacking_crossing"
)

tac_gk <- c(
  "goalkeeping_diving", "goalkeeping_handling",
  "goalkeeping_kicking", "goalkeeping_positioning",
  "goalkeeping_reflexes", "goalkeeping_speed",
  "movement_reactions", "mentality_composure",
  "attacking_short_passing", "skill_long_passing"
)

perfil_medio(pos_fwd, tac_fwd, "DELANTEROS")
perfil_medio(pos_mid, tac_mid, "MEDIOCAMPISTAS")
perfil_medio(pos_def, tac_def, "DEFENSAS")
perfil_medio(pos_gk,  tac_gk,  "PORTEROS")

# ── 5. Etiquetas tácticas y score (solo columnas presentes) ─────
etiquetar <- function(df, mapa) {
  df %>%
    mutate(perfil = recode(as.character(cluster_num), !!!mapa))
}

pos_fwd$df <- etiquetar(pos_fwd$df, c(
  "1" = "Delantero Estrella",
  "2" = "Pivote / Referencia",
  "3" = "Extremo / Delantero Dinámico"
))

pos_mid$df <- etiquetar(pos_mid$df, c(
  "1" = "Mediocampista Élite",
  "2" = "Mediocampista Ofensivo",
  "3" = "Mediocampista Defensivo"
))

pos_def$df <- etiquetar(pos_def$df, c(
  "1" = "Defensa Élite",
  "2" = "Lateral Ofensivo",
  "3" = "Defensa Central Físico"
))

pos_gk$df <- etiquetar(pos_gk$df, c(
  "1" = "Portero Titular",
  "2" = "Portero de Reserva"
))

calcular_score_fwd <- function(df) {
  df %>%
    mutate(
      score_impacto = case_when(
        perfil == "Delantero Estrella" ~ round(
          attacking_finishing    * 0.20 +
            skill_dribbling        * 0.18 +
            mentality_positioning  * 0.15 +
            movement_acceleration  * 0.12 +
            skill_ball_control     * 0.10 +
            mentality_composure    * 0.10 +
            overall                * 0.15,
          2
        ),
        perfil == "Pivote / Referencia" ~ round(
          attacking_heading_accuracy * 0.22 +
            power_jumping              * 0.18 +
            attacking_finishing        * 0.18 +
            power_shot_power           * 0.15 +
            mentality_positioning      * 0.12 +
            overall                    * 0.15,
          2
        ),
        perfil == "Extremo / Delantero Dinámico" ~ round(
          movement_sprint_speed * 0.20 +
            movement_acceleration * 0.18 +
            skill_dribbling       * 0.17 +
            attacking_finishing   * 0.15 +
            attacking_crossing    * 0.10 +
            overall               * 0.20,
          2
        ),
        TRUE ~ NA_real_
      )
    )
}

calcular_score_mid <- function(df) {
  df %>%
    mutate(
      score_impacto = case_when(
        perfil == "Mediocampista Élite" ~ round(
          attacking_short_passing * 0.18 +
            mentality_vision        * 0.15 +
            skill_ball_control      * 0.12 +
            power_stamina           * 0.10 +
            skill_long_passing      * 0.10 +
            mentality_composure     * 0.10 +
            overall                 * 0.25,
          2
        ),
        perfil == "Mediocampista Ofensivo" ~ round(
          movement_agility        * 0.18 +
            skill_ball_control      * 0.15 +
            attacking_short_passing * 0.15 +
            mentality_vision        * 0.12 +
            power_long_shots        * 0.10 +
            overall                 * 0.30,
          2
        ),
        perfil == "Mediocampista Defensivo" ~ round(
          mentality_interceptions   * 0.22 +
            defending_standing_tackle * 0.20 +
            mentality_aggression      * 0.15 +
            power_stamina             * 0.13 +
            attacking_short_passing   * 0.10 +
            overall                   * 0.20,
          2
        ),
        TRUE ~ NA_real_
      )
    )
}

calcular_score_def <- function(df) {
  df %>%
    mutate(
      score_impacto = case_when(
        perfil == "Defensa Élite" ~ round(
          defending_marking_awareness * 0.20 +
            defending_standing_tackle   * 0.18 +
            mentality_interceptions     * 0.15 +
            attacking_short_passing     * 0.12 +
            power_jumping               * 0.10 +
            overall                     * 0.25,
          2
        ),
        perfil == "Lateral Ofensivo" ~ round(
          movement_sprint_speed   * 0.20 +
            movement_acceleration   * 0.18 +
            attacking_crossing        * 0.17 +
            mentality_interceptions   * 0.10 +
            power_stamina             * 0.10 +
            overall                   * 0.25,
          2
        ),
        perfil == "Defensa Central Físico" ~ round(
          power_strength              * 0.22 +
            defending_marking_awareness * 0.18 +
            defending_standing_tackle   * 0.18 +
            power_jumping               * 0.15 +
            mentality_aggression        * 0.12 +
            overall                     * 0.15,
          2
        ),
        TRUE ~ NA_real_
      )
    )
}

calcular_score_gk <- function(df) {
  df %>%
    mutate(
      score_impacto = case_when(
        perfil == "Portero Titular" ~ round(
          goalkeeping_reflexes    * 0.25 +
            goalkeeping_positioning * 0.22 +
            goalkeeping_diving      * 0.18 +
            goalkeeping_handling    * 0.15 +
            goalkeeping_kicking     * 0.10 +
            overall                 * 0.10,
          2
        ),
        perfil == "Portero de Reserva" ~ round(
          goalkeeping_reflexes    * 0.25 +
            goalkeeping_positioning * 0.22 +
            goalkeeping_diving      * 0.18 +
            goalkeeping_handling    * 0.15 +
            goalkeeping_kicking     * 0.10 +
            overall                 * 0.10,
          2
        ),
        TRUE ~ NA_real_
      )
    )
}

pos_fwd$df <- calcular_score_fwd(pos_fwd$df)
pos_mid$df <- calcular_score_mid(pos_mid$df)
pos_def$df <- calcular_score_def(pos_def$df)
pos_gk$df  <- calcular_score_gk(pos_gk$df)

# ── 6. Ranking global ────────────────────────────────────────────
df_ranking <- bind_rows(pos_fwd$df, pos_mid$df, pos_def$df, pos_gk$df) %>%
  mutate(cluster = cluster_num) %>%
  select(
    short_name, nationality_name, club_name, macro_pos, perfil, cluster,
    overall, age, score_impacto
  ) %>%
  arrange(desc(score_impacto)) %>%
  mutate(rank_global = row_number())

# ── 7. Guardar modelos y ranking ─────────────────────────────────
dir.create(here::here("models"), showWarnings = FALSE, recursive = TRUE)

saveRDS(pos_fwd, here::here("models/resultado_fwd.rds"))
saveRDS(pos_mid, here::here("models/resultado_mid.rds"))
saveRDS(pos_def, here::here("models/resultado_def.rds"))
saveRDS(pos_gk,  here::here("models/resultado_gk.rds"))
saveRDS(df_ranking, here::here("models/ranking_global.rds"))

cat("\n[OK] Mineria y ranking guardados en models/\n")
