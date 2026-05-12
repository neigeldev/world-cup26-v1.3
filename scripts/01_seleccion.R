# ══════════════════════════════════════════════════════════════════
# FASE 1: SELECCIÓN
# Objetivo: cargar el dataset y seleccionar solo lo relevante
# ══════════════════════════════════════════════════════════════════
library(tidyverse)

# ── Carga ─────────────────────────────────────────────────────────
df_raw <- read_csv(here::here("data/01_raw/worldcup_22.csv"))

# ── Variables a eliminar ──────────────────────────────────────────
vars_eliminar <- c(
  "player_url", "long_name", "dob",
  "player_face_url", "club_logo_url", "club_flag_url",
  "nation_logo_url", "nation_flag_url",
  "club_team_id", "club_jersey_number", "club_loaned_from",
  "club_joined", "club_contract_valid_until",
  "nation_team_id", "nation_position", "nation_jersey_number",
  "body_type", "real_face", "player_tags", "player_traits",
  "release_clause_eur"
)

df_seleccionado <- df_raw %>%
  select(-any_of(vars_eliminar))

# ── Guardar ───────────────────────────────────────────────────────
saveRDS(df_seleccionado,
        here::here("data/03_interim/df_seleccionado.rds"))

cat("[OK] Seleccion completa:", nrow(df_seleccionado),
    "jugadores x", ncol(df_seleccionado), "variables\n")