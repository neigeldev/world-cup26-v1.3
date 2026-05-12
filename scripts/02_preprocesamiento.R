# ══════════════════════════════════════════════════════════════════
# FASE 2: PREPROCESAMIENTO
# Objetivo: parsear ratings, asignar posiciones, tratar NAs
# ══════════════════════════════════════════════════════════════════
library(tidyverse)

df <- readRDS(here::here("data/03_interim/df_seleccionado.rds"))

# ── 1. Parsear ratings posicionales "89+3" → 92 ───────────────────
parse_sofifa_rating <- function(x) {
  base  <- as.numeric(str_extract(x, "^\\d+"))
  tiene_menos <- str_detect(x, "-\\d+$")
  adj   <- as.numeric(str_extract(x, "\\d+$"))
  ifelse(tiene_menos, base - adj, base + adj)
}

vars_pos_ratings <- c("ls","st","rs","lw","lf","cf","rf","rw",
                      "lam","cam","ram","lm","lcm","cm","rcm","rm",
                      "lwb","ldm","cdm","rdm","rwb",
                      "lb","lcb","cb","rcb","rb","gk")

df <- df %>%
  mutate(across(all_of(vars_pos_ratings), parse_sofifa_rating))

# ── 2. Extraer posición principal y macro-posición ─────────────────
asignar_macro <- function(pos) {
  case_when(
    str_detect(pos, "^GK")                            ~ "GK",
    str_detect(pos, "^(CB|LCB|RCB|LB|RB|LWB|RWB)")  ~ "DEF",
    str_detect(pos, "^(CDM|LDM|RDM|CM|LCM|RCM|
                        LM|RM|CAM|LAM|RAM)")           ~ "MID",
    str_detect(pos, "^(ST|LS|RS|LW|RW|LF|RF|CF)")    ~ "FWD",
    TRUE ~ "Otro"
  )
}

df <- df %>%
  mutate(
    pos_principal = str_trim(str_to_upper(
      str_extract(player_positions, "^[^,]+"))),
    macro_pos     = asignar_macro(pos_principal),
    n_posiciones  = str_count(player_positions, ",") + 1
  )

# ── 3. Tratar NAs (mediana por macro_pos) ─────────────────────────
df <- df %>%
  group_by(macro_pos) %>%
  mutate(across(where(is.numeric),
                ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# ── 4. Separar porteros ───────────────────────────────────────────
df_campo <- df %>% filter(macro_pos != "GK")
df_gk    <- df %>% filter(macro_pos == "GK")

# ── Guardar ───────────────────────────────────────────────────────
saveRDS(df_campo, here::here("data/03_interim/df_campo_limpio.rds"))
saveRDS(df_gk,    here::here("data/03_interim/df_gk_limpio.rds"))

cat("[OK] Campo:", nrow(df_campo), "| GK:", nrow(df_gk), "\n")
cat("[OK] NAs restantes:",
    df_campo %>%
      select(where(is.numeric)) %>%
      is.na() %>%
      sum(), "\n")
