## ============================================================
##  lab1_moda.R
##  SESSÃO 1 (1h30) — Tábua de vida como distribuição
##  e Idade Modal: cinco métodos + impacto do recorte etário
## ============================================================
##
##  Referências principais:
##  Wilmoth & Horiuchi (1999)  Demography 36(4): 475-495
##  Canudas-Romo (2010)        Demographic Research 22: 421-438
##  Horiuchi et al. (2013)     Population Studies 67(3): 291-305
##  Missov et al. (2015)       Demographic Research 32: 701-732
## ============================================================

source("R/00_setup.R")
source("R/funcoes_aux.R")

## ============================================================
## PARTE 0 — Carregar dados
## ============================================================
## Opção A: dados embutidos (sem login)
lt_swe2019  <- build_lt(dados_mx$SWE_2019, sex = "f")
lt_swe1960  <- build_lt(dados_mx$SWE_1960, sex = "f")
lt_usa2019  <- build_lt(dados_mx$USA_2019, sex = "f")
lt_usa1960  <- build_lt(dados_mx$USA_1960, sex = "f")

## Opção B: HMD (requer credenciais)
## usr <- readline("HMD username: ")
## pwd <- readline("HMD password: ")
## lt_swe2019 <- HMDHFDplus::readHMDweb("SWE","fltper_1x1",usr,pwd) |>
##   filter(Year == 2019) |> build_lt_hmd()

cat("e0 Suécia 2019:", round(lt_swe2019$ex[1], 2), "anos\n")
cat("e0 Suécia 1960:", round(lt_swe1960$ex[1], 2), "anos\n")
cat("e0 EUA    2019:", round(lt_usa2019$ex[1], 2), "anos\n")

## ============================================================
## PARTE 1 — A tábua de vida como distribuição de probabilidade
## ============================================================

## 1.1  Visualizar l(x), f(x) = d(x), F(x) simultaneamente
lt <- lt_swe2019

p_lx <- ggplot(lt, aes(age, lx)) +
  geom_line(color = "#1f497d", linewidth = 1.2) +
  labs(title = expression(bold("Curva de sobrevivência") ~ ell(x)),
       x = "Idade", y = expression(ell(x))) +
  scale_x_continuous(breaks = seq(0, 110, 10))

p_dx <- ggplot(lt, aes(age, dx)) +
  geom_col(fill = "#1f497d", alpha = 0.7, width = 0.9) +
  labs(title = expression(bold("Distribuição de mortes") ~ d(x) == f(x)),
       x = "Idade", y = "d(x)") +
  scale_x_continuous(breaks = seq(0, 110, 10))

p_Fx <- lt |>
  mutate(Fx = cumsum(dx) / sum(dx)) |>
  ggplot(aes(age, Fx)) +
  geom_line(color = "#c00000", linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  labs(title = expression(bold("CDF") ~ F(x) == 1 - ell(x)),
       x = "Idade", y = "F(x)") +
  scale_x_continuous(breaks = seq(0, 110, 10))

fig1 <- p_lx / p_dx / p_Fx
ggsave("figs/fig1_tabua_distribuicao.pdf", fig1,
       width = 10, height = 12)
print(fig1)

## ============================================================
## PARTE 2 — Cinco métodos para a idade modal
## ============================================================

## 2.1  Calcular com diferentes recortes
recortes <- c(0, 10, 30, 40)

modos_metodos <- function(lt, age_min) {
  tibble(
    age_min     = age_min,
    Discreto    = mode_discrete(lt, age_min),
    Spline      = mode_spline(lt, age_min),
    Gompertz    = mode_gompertz(lt, if (age_min < 40) 40:90
                                    else age_min:90),
    Kernel      = mode_kernel(lt, bw = 5, age_min),
    FxMax       = mode_fx_max(lt, age_min)
  )
}

tab_modos <- map_dfr(recortes, ~ modos_metodos(lt_swe2019, .x))

cat("\n── Moda por método e recorte etário (Suécia 2019, mulheres) ──\n")
print(tab_modos |> mutate(across(where(is.numeric), ~round(.x, 1))))

## ============================================================
## PARTE 3 — Visualização: d(x) com todos os métodos
## ============================================================

## 3.1  Gráfico comparativo dos 5 métodos (recorte = 40)
modos_40 <- modos_metodos(lt_swe2019, 40)
modos_long <- modos_40 |>
  select(-age_min) |>
  pivot_longer(everything(), names_to = "Metodo", values_to = "Moda") |>
  filter(!is.na(Moda))

fig2 <- ggplot(lt_swe2019, aes(age, dx)) +
  geom_col(fill = "steelblue", alpha = 0.6, width = 0.9) +
  geom_vline(data = modos_long,
             aes(xintercept = Moda, color = Metodo, linetype = Metodo),
             linewidth = 1.0) +
  geom_text(data = modos_long,
            aes(x = Moda + 1.5, y = max(lt_swe2019$dx) * 0.95 -
                  (as.integer(factor(Metodo)) - 1) * 0.003,
                label = sprintf("%s: %.1f", Metodo, Moda),
                color = Metodo),
            hjust = 0, size = 3.2, fontface = "bold") +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(
    values = c("Discreto" = "solid", "Spline" = "dashed",
               "Gompertz" = "dotdash", "Kernel" = "dotted",
               "FxMax"    = "longdash")) +
  labs(title = "Distribuição de mortes e estimativas da moda",
       subtitle = "Suécia 2019, mulheres — recorte: idade ≥ 40",
       x = "Idade", y = "d(x)",
       caption = "Fontes: Canudas-Romo (2010); Horiuchi et al. (2013); Missov et al. (2015)") +
  scale_x_continuous(breaks = seq(0, 110, 10)) +
  theme(legend.position = "none")

ggsave("figs/fig2_moda_metodos.pdf", fig2, width = 11, height = 5)
print(fig2)

## ============================================================
## PARTE 4 — Impacto do recorte etário na estimação da moda
## ============================================================

## 4.1  Comparar as 4 populações com recorte 0 vs 40
pop_lista <- list(
  SWE_1960 = lt_swe1960,
  SWE_2019 = lt_swe2019,
  USA_1960 = lt_usa1960,
  USA_2019 = lt_usa2019
)

tab_recorte <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao   = nm,
    e0          = round(lt$ex[1], 1),
    Moda_desde0 = round(mode_spline(lt, age_min = 0),  1),
    Moda_10plus = round(mode_spline(lt, age_min = 10), 1),
    Moda_30plus = round(mode_spline(lt, age_min = 30), 1),
    Moda_40plus = round(mode_spline(lt, age_min = 40), 1)
  )
})

cat("\n── Impacto do recorte etário na estimação da moda (spline) ──\n")
print(tab_recorte)

## 4.2  Visualizar d(x) bimodal — por que o recorte em 40 existe
fig3 <- ggplot(lt_swe1960, aes(age, dx)) +
  geom_col(fill = "gray70", alpha = 0.8, width = 0.9) +
  geom_vline(xintercept = mode_spline(lt_swe1960, 0),
             color = "red", linewidth = 1, linetype = "dashed",
             show.legend = TRUE) +
  geom_vline(xintercept = mode_spline(lt_swe1960, 40),
             color = "#1f497d", linewidth = 1, linetype = "solid") +
  annotate("text", x = mode_spline(lt_swe1960, 0) + 1,
           y = max(lt_swe1960$dx) * 0.9,
           label = sprintf("Moda desde 0\n= %.1f anos",
                           mode_spline(lt_swe1960, 0)),
           color = "red", hjust = 0, size = 3.5) +
  annotate("text", x = mode_spline(lt_swe1960, 40) + 1,
           y = max(lt_swe1960$dx) * 0.7,
           label = sprintf("Moda desde 40\n= %.1f anos",
                           mode_spline(lt_swe1960, 40)),
           color = "#1f497d", hjust = 0, size = 3.5) +
  annotate("rect", xmin = -1, xmax = 5, ymin = 0,
           ymax = max(lt_swe1960$dx) * 0.45,
           fill = "orange", alpha = 0.15) +
  annotate("text", x = 2, y = max(lt_swe1960$dx) * 0.48,
           label = "Pico infantil", size = 3, color = "darkorange") +
  labs(title = "Distribuição bimodal — por que o recorte em 40 existe",
       subtitle = "Suécia 1960, mulheres",
       x = "Idade", y = "d(x)",
       caption = "Canudas-Romo (2010): recorte em 40 é um critério de identificação\n
       do pico senil, não de sensibilidade da moda à mortalidade infantil.") +
  scale_x_continuous(breaks = seq(0, 110, 10))

ggsave("figs/fig3_bimodalidade_recorte.pdf", fig3, width = 10, height = 5)
print(fig3)

## ============================================================
## PARTE 5 — Relação e0, Mediana e Moda ao longo do tempo
## ============================================================

## (Exercício guiado — requer dados HMD séria temporal)
## Aqui mostramos apenas o cross-section com as 4 populações

tab_localizacao <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao = nm,
    e0        = round(lt$ex[1],         2),
    Mediana   = round(mediana_lt(lt),    2),
    Moda      = round(mode_spline(lt, 40), 2),
    gap_e0_M  = round(mode_spline(lt, 40) - lt$ex[1], 2)
  )
})

cat("\n── Relação e0, Mediana e Moda (gap = M - e0) ──\n")
print(tab_localizacao)

fig4 <- tab_localizacao |>
  pivot_longer(c(e0, Mediana, Moda),
               names_to = "Medida", values_to = "Valor") |>
  ggplot(aes(Populacao, Valor, color = Medida, group = Medida)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = expression(
         "Comparação de " * e[0] * ", mediana e moda"),
       subtitle = "Suécia e EUA, 1960 e 2019 — mulheres",
       x = NULL, y = "Idade (anos)",
       caption = "Moda calculada com spline, recorte em 40 anos.") +
  scale_color_manual(
    values = c("e0" = "#c00000",
               "Mediana" = "#ffc000",
               "Moda"    = "#1f497d"))

ggsave("figs/fig4_e0_mediana_moda.pdf", fig4, width = 9, height = 5)
print(fig4)

cat("\n✓ Lab 1 concluído. Figuras salvas em figs/\n")
