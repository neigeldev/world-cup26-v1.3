# ══════════════════════════════════════════════════════════════════
# FASE 3: TRANSFORMACIÓN
# Objetivo: seleccionar features por posición, escalar, construir
#           matrices listas para modelar
# ══════════════════════════════════════════════════════════════════
library(tidyverse)

# ── Features SOLO tácticos (sin overall, age, height_cm) ──────────
# Estos van a la matriz de clustering

tac_fwd <- c(
  "attacking_finishing", "mentality_positioning",
  "power_shot_power", "power_long_shots",
  "attacking_heading_accuracy", "skill_dribbling",
  "movement_acceleration", "movement_sprint_speed",
  "skill_ball_control", "attacking_crossing",
  "mentality_composure", "power_stamina", "power_jumping"
)  # sin overall, age, height_cm

tac_mid <- c(
  "attacking_short_passing", "skill_long_passing",
  "mentality_vision", "skill_ball_control",
  "attacking_crossing", "mentality_interceptions",
  "defending_standing_tackle", "mentality_aggression",
  "power_stamina", "movement_reactions",
  "movement_agility", "power_long_shots",
  "mentality_positioning", "mentality_composure"
)  # sin overall, age

tac_def <- c(
  "defending_marking_awareness", "defending_standing_tackle",
  "mentality_interceptions", "mentality_aggression",
  "power_jumping", "power_strength", "power_stamina",
  "attacking_short_passing", "skill_long_passing",
  "movement_acceleration", "movement_sprint_speed",
  "attacking_crossing"
)  # sin overall, age, height_cm

tac_gk <- c(
  "goalkeeping_diving", "goalkeeping_handling",
  "goalkeeping_kicking", "goalkeeping_positioning",
  "goalkeeping_reflexes", "goalkeeping_speed",
  "movement_reactions", "mentality_composure",
  "attacking_short_passing", "skill_long_passing"
)  # sin overall, age, height_cm, power_strength


# ── Reconstruir matrices de clustering con PCA ────────────────────
library(tidyverse)
library(cluster)

df_campo <- readRDS(here::here("data/03_interim/df_campo_limpio.rds"))
df_gk    <- readRDS(here::here("data/03_interim/df_gk_limpio.rds"))
df_todo  <- bind_rows(df_campo, df_gk)

preparar_pca <- function(data, macro, tac_features,
                         full_features, var_explicada = 0.80) {
  # Dataset completo con identidad
  df_pos <- data %>%
    filter(macro_pos == macro) %>%
    select(short_name, nationality_name, club_name,
           overall, potential, age, height_cm, macro_pos,
           all_of(unique(full_features))) %>%
    drop_na()
  
  # Matriz táctica escalada
  mat_tac <- df_pos %>%
    select(all_of(unique(tac_features))) %>%
    scale()
  
  # PCA
  pca_res  <- prcomp(mat_tac, center = FALSE, scale. = FALSE)
  var_acum <- cumsum(pca_res$sdev^2) / sum(pca_res$sdev^2)
  n_comp   <- which(var_acum >= var_explicada)[1]
  
  cat(macro, "->", n_comp, "componentes explican",
      round(var_acum[n_comp]*100, 1), "% de la varianza\n")
  
  mat_pca  <- as.data.frame(pca_res$x[, 1:n_comp])
  
  list(df     = df_pos,
       mat    = mat_pca,        # ← para clustering
       pca    = pca_res,
       n_comp = n_comp)
}

full_fwd <- unique(c(tac_fwd, "age", "height_cm"))
full_mid <- unique(c(tac_mid, "age"))
full_def <- unique(c(tac_def, "age", "height_cm"))
full_gk  <- unique(c(tac_gk,  "age", "height_cm", "power_strength"))

pos_fwd <- preparar_pca(df_todo, "FWD", tac_fwd, full_fwd)
pos_mid <- preparar_pca(df_todo, "MID", tac_mid, full_mid)
pos_def <- preparar_pca(df_todo, "DEF", tac_def, full_def)
pos_gk  <- preparar_pca(df_todo, "GK",  tac_gk,  full_gk)

# Guardar objetos actualizados
saveRDS(pos_fwd, here::here("data/04_processed/pos_fwd.rds"))
saveRDS(pos_mid, here::here("data/04_processed/pos_mid.rds"))
saveRDS(pos_def, here::here("data/04_processed/pos_def.rds"))
saveRDS(pos_gk,  here::here("data/04_processed/pos_gk.rds"))
