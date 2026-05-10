# ══════════════════════════════════════════════════════════════════
# PIPELINE COMPLETO KDD — MUNDIAL FIFA
# ══════════════════════════════════════════════════════════════════
cat("-- Iniciando pipeline KDD --\n", date(), "\n\n")

source(here::here("scripts/01_seleccion.R"))
source(here::here("scripts/02_preprocesamiento.R"))
source(here::here("scripts/03_transformacion.R"))
source(here::here("scripts/04_mineria.R"))
source(here::here("scripts/05_evaluacion.R"))

cat("\n[OK] Pipeline completo finalizado\n", date(), "\n")