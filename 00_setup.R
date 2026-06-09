## ============================================================
##  00_setup.R — Instalação de pacotes e carregamento de dados
##  HMD via HMDHFDplus
##    - Dados exactos publicados pelo Human Mortality Database
##    - Requer cadastro gratuito em mortality.org
##    - Produz e0 e M corretos conforme publicado
##
##  OPÇÃO B: pacote demography (sem login, dados embutidos)
##    - Inclui mortalidade australiana e outras
##    - Para SWE/USA use HMDHFDplus
##
## ============================================================

## ── 1. Pacotes ───────────────────────────────────────────────
pkgs <- c("tidyverse","ggplot2","scales","patchwork",
          "HMDHFDplus","splines","DemoDecomp",
          "remotes","ggrepel","viridis","knitr")
novos <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(novos) > 0) install.packages(novos)

if (!requireNamespace("LifeIneq", quietly = TRUE))
  remotes::install_github("alysonvanraalte/LifeIneq")

library(tidyverse); library(ggplot2); library(patchwork)
library(scales); library(LifeIneq); library(splines)
library(HMDHFDplus)

## ── 2. Tema ggplot e cores ───────────────────────────────────
theme_demo <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "gray40", size = base_size - 1),
      plot.caption  = element_text(colour = "gray50", size = 9, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}
theme_set(theme_demo())
cores <- c(SWE = "#1f497d", USA = "#c00000",
           JPN = "#70ad47", BRA = "#ffc000")

## ── 3. Carregamento de dados HMD ─────────────────────────────
## Credenciais: registre-se em https://www.mortality.org (gratuito)
## No Posit Cloud: Tools > Global Options > Environment > Add variable
##   HMD_USER = seu_email
##   HMD_PASS = sua_senha
## Ou insira interativamente abaixo:

hmd_user <- Sys.getenv("HMD_USER")
hmd_pass <- Sys.getenv("HMD_PASS")

## Função auxiliar: extrai mx de um dataframe HMD 1x1 filtrado por ano
extrair_mx_hmd <- function(df, ano, omega = 110) {
  df |>
    filter(Year == ano, Age <= omega) |>
    arrange(Age) |>
    pull(mx)
}

## Baixar tábuas de vida 1x1 (uma vez; pode demorar ~30s)

lt_swe_raw <- readHMDweb("SWE", "fltper_1x1", hmd_user , hmd_pass )

write.csv(lt_swe_raw, "swe_f.csv", row.names = FALSE)

lt_usa_raw <- readHMDweb("USA", "fltper_1x1", hmd_user , hmd_pass )
write.csv(lt_usa_raw, "usa_f.csv", row.names = FALSE)

## Extrair vetores mx para os anos de interesse
dados_mx <- list(
  SWE_1960 = extrair_mx_hmd(lt_swe_raw, 1960),
  SWE_2019 = extrair_mx_hmd(lt_swe_raw, 2019),
  USA_1960 = extrair_mx_hmd(lt_usa_raw, 1960),
  USA_2019 = extrair_mx_hmd(lt_usa_raw, 2019)
)

ages <- 0:110

write.csv(dados_mx, "dados_mx.csv", row.names = FALSE)

## Verificação rápida de e0 (usando lt_swe_raw diretamente)
e0_check <- lt_swe_raw |>
  filter(Year %in% c(1960, 2019), Age == 0) |>
  select(Year, ex)

print(e0_check)

cat("\n✓ Dados carregados para:",
    paste(names(dados_mx), collapse = ", "), "\n")
cat("  Comprimentos:",
    paste(sapply(dados_mx, length), collapse = ", "), "\n")
