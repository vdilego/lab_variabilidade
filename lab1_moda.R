## ============================================================
##  lab1_moda.R
##  SESSÃO 1 — Tábua de vida como distribuição
##  e Idade Modal: cinco métodos + impacto do recorte etário
## ============================================================
##
##  Referências principais:
##  Wilmoth & Horiuchi (1999)  Demography 36(4): 475-495
##  Canudas-Romo (2010)        Demographic Research 22: 421-438
##  Horiuchi et al. (2013)     Population Studies 67(3): 291-305
##  Missov et al. (2015)       Demographic Research 32: 701-732
## ============================================================

source("/cloud/project/00_setup.R")
source("/cloud/project/funcoes_aux.R")

## ============================================================
## PARTE 0 — Carregar dados
## ============================================================
## Opção A: dados embutidos (sem login)
lt_swe2019  <- build_lt(dados_mx$SWE_2019, sex = "f")
lt_swe1960  <- build_lt(dados_mx$SWE_1960, sex = "f")
lt_usa2019  <- build_lt(dados_mx$USA_2019, sex = "f")
lt_usa1960  <- build_lt(dados_mx$USA_1960, sex = "f")

# verificando dados

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
tab_modos2 <- map_dfr(recortes, ~ modos_metodos(lt_swe1960, .x))

#cat("\n── Moda por método e recorte etário (Suécia 2019, mulheres) ──\n")
print(tab_modos |> mutate(across(where(is.numeric), ~round(.x, 5))))
print(tab_modos2 |> mutate(across(where(is.numeric), ~round(.x, 5))))

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

## ── PARTE 4: tabela comparando 5 métodos (recorte fixo = 40) ──
## Esta é a comparação pedagogicamente útil: qual método
## chega mais perto do pico real?
tab_metodos <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao    = nm,
    e0           = round(lt$ex[1], 1),
    Discreto     = round(mode_discrete(lt, 40),  1),
    Spline       = round(mode_spline(lt,   40),  1),
    Gompertz     = round(mode_gompertz(lt),       1),
    Kernel_bw5   = round(mode_kernel(lt, bw = 5, 40), 1),
    FxMax        = round(mode_fx_max(lt,  40),   1)
  )
})

cat("\n── Comparação de MÉTODOS (recorte = 40) ──\n")
print(tab_metodos)

## ============================================================
## ============================================================
## PARTE 4 — Impacto do recorte etário: métodos e demonstração
## ============================================================

## 4a ── Tabela: comparação de MÉTODOS (recorte fixo = 40) ─────
## Esta é a comparação pedagogicamente relevante para dados HMD
## modernos: o recorte de 40 já é padrão; a variação entre métodos
## é o que interessa.

tab_metodos <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao  = nm,
    e0         = round(lt$ex[1], 1),
    Discreto   = round(mode_discrete(lt, 40),    1),
    Spline     = round(mode_spline(lt,   40),    1),
    Gompertz   = round(mode_gompertz(lt),         1),
    Kernel_bw5 = round(mode_kernel(lt, 5,   40), 1),
    FxMax      = round(mode_fx_max(lt,       40), 1)
  )
})

cat("\n── Comparação de métodos (recorte = 40) ──\n")
print(tab_metodos)


## ── Gráfico comparativo dos métodos ─────────────────────────────
## ============================================================
##  fig_tab_metodos.R
##  Gráfico comparativo dos 5 métodos para a moda
##  Inserir após a criação de tab_metodos em lab1_moda.R
## ============================================================

## ── Transformar em formato longo ─────────────────────────────
tab_long <- tab_metodos |>
  pivot_longer(
    cols      = c(Discreto, Spline, Gompertz, Kernel_bw5, FxMax),
    names_to  = "Metodo",
    values_to = "Moda"
  ) |>
  mutate(
    Metodo = factor(Metodo,
                    levels = c("Discreto","Spline","Gompertz",
                               "Kernel_bw5","FxMax"),
                    labels = c("Discreto","Spline\ncúbico",
                               "Gompertz","Kernel\n(bw=5)",
                               "Máx f(x)")),
    Pais = str_extract(Populacao, "^[A-Z]+"),
    Ano  = str_extract(Populacao, "[0-9]+"),
    label_pop = sprintf("%s\n%s\n(e₀=%.1f)", Pais, Ano, e0)
  )

## ── Calcular a dispersão entre métodos por população ─────────
dispersao <- tab_long |>
  group_by(Populacao, e0, label_pop) |>
  summarise(
    M_min   = min(Moda,  na.rm = TRUE),
    M_max   = max(Moda,  na.rm = TRUE),
    M_med   = median(Moda, na.rm = TRUE),
    M_range = M_max - M_min,
    .groups = "drop"
  )

## ── Paleta de métodos ─────────────────────────────────────────
pal_metodos <- c(
  "Discreto"    = "#c00000",
  "Spline\ncúbico" = "#1f497d",
  "Gompertz"    = "#70ad47",
  "Kernel\n(bw=5)" = "#ffc000",
  "Máx f(x)"    = "#7030a0"
)

## ── Gráfico 1: dotplot com faixas de dispersão ───────────────
## Cada população é uma faixa vertical; cada ponto é um método
fig_metodos <- ggplot() +
  ## Faixa de dispersão (min-max entre métodos)
  geom_linerange(
    data = dispersao,
    aes(x = label_pop, ymin = M_min, ymax = M_max),
    color = "gray75", linewidth = 4, alpha = 0.6
  ) +
  ## Mediana dos métodos
  geom_point(
    data = dispersao,
    aes(x = label_pop, y = M_med),
    shape = 3, size = 5, stroke = 1.5, color = "gray40"
  ) +
  ## Estimativas individuais de cada método
  geom_point(
    data = tab_long |> filter(!is.na(Moda)),
    aes(x = label_pop, y = Moda,
        color = Metodo, shape = Metodo),
    size = 3.5, stroke = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  scale_color_manual(values = pal_metodos) +
  scale_shape_manual(
    values = c("Discreto"=16, "Spline\ncúbico"=17,
               "Gompertz"=15, "Kernel\n(bw=5)"=18,
               "Máx f(x)"=8)) +
  ## Linha de referência: e0 de cada população
  geom_point(
    data = dispersao,
    aes(x = label_pop, y = e0),
    shape = 1, size = 4, stroke = 1.2, color = "black",
    show.legend = FALSE
  ) +
  labs(
    title    = "Estimativas da idade modal à morte: comparação de métodos",
    subtitle = "Faixa cinza = amplitude entre métodos · ✕ = mediana · ○ = e₀",
    x        = NULL,
    y        = "Idade estimada (anos)",
    color    = "Método", shape = "Método",
    caption  = paste0(
      "Recorte: idades ≥ 40 para todos os métodos (Canudas-Romo, 2010).\n",
      "Gompertz: ajuste log-linear de μ(x) em 40–90. Kernel: bandwidth = 5.\n",
      "Dados: HMD, mulheres.")
  ) +
  theme(
    legend.position = "right",
    axis.text.x     = element_text(lineheight = 0.9)
  )

ggsave("figs/fig_metodos_comparacao.pdf", fig_metodos, width = 10, height = 5.5)
print(fig_metodos)

## ── Gráfico 2: desvio de cada método em relação ao Spline ────
## Responde: quanto cada método difere do de referência (Spline)?
ref_spline <- tab_long |>
  filter(Metodo == "Spline\ncúbico") |>
  select(Populacao, Moda_ref = Moda)

tab_desvio <- tab_long |>
  left_join(ref_spline, by = "Populacao") |>
  mutate(Desvio = Moda - Moda_ref) |>
  filter(Metodo != "Spline\ncúbico")

fig_desvio <- ggplot(tab_desvio,
                     aes(x = label_pop, y = Desvio,
                         color = Metodo, group = Metodo)) +
  geom_hline(yintercept = 0, color = "gray60",
             linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 0.8, alpha = 0.5) +
  geom_point(aes(shape = Metodo), size = 3.5) +
  scale_color_manual(
    values = pal_metodos[names(pal_metodos) != "Spline\ncúbico"]) +
  scale_shape_manual(
    values = c("Discreto"=16, "Gompertz"=15,
               "Kernel\n(bw=5)"=18, "Máx f(x)"=8)) +
  scale_y_continuous(breaks = seq(-3, 4, 1),
                     labels = function(x) sprintf("%+d", x)) +
  labs(
    title    = "Desvio de cada método em relação ao Spline cúbico",
    subtitle = "0 = coincide com Spline · positivo = estima M mais alto",
    x        = NULL,
    y        = "Desvio (anos)",
    color    = "Método", shape = "Método",
    caption  = paste0(
      "Spline cúbico: referência (Horiuchi et al., 2013; Kannisto, 2001).\n",
      "Kernel bw=5: maior desvio absoluto (1.07) — bandwidth suaviza demais picos estreitos.\n",
      "Discreto: menor desvio (0.10) — quase idêntico ao Spline.")
  ) +
  theme(
    legend.position = "right",
    axis.text.x     = element_text(lineheight = 0.9)
  )

ggsave("figs/fig_metodos_desvio.pdf", fig_desvio, width = 10, height = 5)
print(fig_desvio)

## ── Tabela-resumo: amplitude e padrão dos desvios ─────────────
cat("\n── Amplitude da estimação por método ──\n")
tab_amplitude <- tab_desvio |>
  group_by(Metodo) |>
  summarise(
    Desvio_medio = round(mean(Desvio, na.rm=TRUE), 2),
    Desvio_abs   = round(mean(abs(Desvio), na.rm=TRUE), 2),
    Min          = round(min(Desvio, na.rm=TRUE), 1),
    Max          = round(max(Desvio, na.rm=TRUE), 1),
    .groups = "drop"
  ) |>
  arrange(desc(Desvio_abs))

print(tab_amplitude)
cat("  Discreto:        desvio médio abs = 0.10  ← mais próximo do Spline\n")
cat("  Gompertz:        desvio médio abs = 0.62, tende a superestimar (+0.38)\n")
cat("  Máx f(x)/FxMax:  desvio médio abs = 0.47, consistentemente ~+0.5 anos\n")
cat("  Kernel bw=5:     desvio médio abs = 1.07  ← maior afastamento do Spline\n")
cat("\n  O Kernel tem maior desvio porque bw=5 suaviza demais o pico senil\n")
cat("  quando ele é estreito (ex: SWE 2019, pico concentrado em 90 anos).\n")
cat("  Experimente: mode_kernel(lt_swe2019, bw=2, 40)\n")



cat("\n── Por que todos os recortes dão o mesmo resultado? ──\n")
for (nm in names(pop_lista)) {
  lt  <- pop_lista[[nm]]
  d0  <- lt$dx[1]
  m40 <- lt$age[which.max(lt$dx[lt$age >= 40])]
  dm  <- max(lt$dx[lt$age >= 40])
  cat(sprintf("  %s: dx[0]=%.4f  dx[M=%d]=%.4f  → pico senil %s pico infantil\n",
              nm, d0, m40, dm,
              ifelse(dm > d0, "DOMINA", "domina")))
}
cat("\n  Nas populações HMD modernas o pico senil sempre supera\n")
cat("  o pico infantil, então todos os recortes retornam o mesmo M.\n")
cat("  Para ver a diferença precisamos de anos com alta mortalidade infantil.\n")

## 4b ── Dados históricos: quando f(0) > f(M_senil) ─────────────
## Carrega Sweden para anos históricos onde a condição ocorre
## Requer que lt_swe_raw já esteja disponível (carregado em 00_setup.R)

## Anos pedagógicos:
##   1800: f(0) >> f(M)  — bimodalidade extrema (e0 ≈ 33)
##   1900: f(0) >  f(M)  — bimodalidade moderada (e0 ≈ 48)
##   1950: f(0) <  f(M)  — pico senil já domina  (e0 ≈ 72)

anos_hist <- c(1800, 1900, 1950)

lt_hist <- map(anos_hist, function(ano) {
  mx_ano <- lt_swe_raw |>
    filter(Year == ano, Age <= 110) |>
    arrange(Age) |>
    pull(mx)
  build_lt(mx_ano, sex = "f")
}) |> setNames(paste0("SWE_", anos_hist))

## Verificar bimodalidade
cat("\n── Bimodalidade em dados históricos (Sweden females) ──\n")
tab_hist <- map2_dfr(lt_hist, names(lt_hist), function(lt, nm) {
  d0   <- lt$dx[1]
  m40  <- lt$age[which.max(lt$dx[lt$age >= 40])]
  dm   <- max(lt$dx[lt$age >= 40])
  tibble(
    Ano           = nm,
    e0            = round(lt$ex[1], 1),
    dx_infancia   = round(d0, 5),
    Moda_senil    = m40,
    dx_moda_senil = round(dm, 5),
    Bimodal       = ifelse(d0 > dm, "SIM ← f(0) > f(M)", "não")
  )
})
print(tab_hist)

## 4c ── Gráfico comparativo: três distribuições históricas ──────
cores_hist <- c(SWE_1800 = "#c00000",
                SWE_1900 = "#ffc000",
                SWE_1950 = "#1f497d")

## Calcular modos com e sem recorte para cada ano
tab_recorte_hist <- map2_dfr(lt_hist, names(lt_hist), function(lt, nm) {
  tibble(
    Ano         = nm,
    e0          = round(lt$ex[1], 1),
    Moda_desde0 = round(mode_spline(lt, age_min = 0),  1),
    Moda_10plus = round(mode_spline(lt, age_min = 10), 1),
    Moda_30plus = round(mode_spline(lt, age_min = 30), 1),
    Moda_40plus = round(mode_spline(lt, age_min = 40), 1)
  )
})

cat("\n── Impacto do recorte — anos históricos Sweden ──\n")
print(tab_recorte_hist)

## Gráfico: d(x) empilhado para os três anos + marcadores de moda
dx_long <- map2_dfr(lt_hist, names(lt_hist), function(lt, nm) {
  lt |> select(age, dx) |> mutate(Ano = nm)
})

modos_hist_long <- tab_recorte_hist |>
  select(Ano, Moda_desde0, Moda_40plus) |>
  pivot_longer(-Ano, names_to = "Recorte", values_to = "Moda") |>
  mutate(Recorte = recode(Recorte,
                          Moda_desde0 = "Sem recorte (≥ 0)",
                          Moda_40plus = "Com recorte (≥ 40)"))

fig3c <- ggplot(dx_long, aes(age, dx, color = Ano)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_vline(data = filter(modos_hist_long, Recorte == "Sem recorte (≥ 0)"),
             aes(xintercept = Moda, color = Ano),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = filter(modos_hist_long, Recorte == "Com recorte (≥ 40)"),
             aes(xintercept = Moda, color = Ano),
             linetype = "solid", linewidth = 1.2) +
  scale_color_manual(values = cores_hist) +
  ## Anotação explicando as linhas
  annotate("text", x = 95, y = max(dx_long$dx[dx_long$Ano=="SWE_1800"]) * 0.95,
           label = "Sólida = recorte 40\nTracejada = sem recorte",
           hjust = 1, size = 3, color = "gray40") +
  facet_wrap(~ Ano, scales = "free_y", ncol = 1) +
  labs(
    title    = "Distribuição de mortes d(x) — Sweden females",
    subtitle = "1800: f(0) >> f(M) | 1900: f(0) > f(M) | 1950: f(M) > f(0)",
    x = "Idade", y = "d(x)",
    color = NULL,
    caption = paste0(
      "Linhas sólidas: moda com recorte ≥ 40 (Canudas-Romo, 2010).\n",
      "Linhas tracejadas: argmax sem recorte (retorna pico infantil em 1800 e 1900).\n",
      "Dados: Human Mortality Database, Sweden females.")
  ) +
  scale_x_continuous(breaks = seq(0, 110, 20)) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("figs/fig3_bimodalidade_historica.pdf", fig3c, width = 10, height = 10)
print(fig3c)

## 4d ── Gráfico de painel único (sem facet) para slide ─────────
fig3d <- ggplot(dx_long, aes(age, dx, color = Ano, group = Ano)) +
  geom_line(linewidth = 1.2) +
  ## Modos com recorte 40
  geom_point(data = modos_hist_long |> filter(Recorte == "Com recorte (≥ 40)"),
             aes(x = Moda, y = 0.005, color = Ano), size = 4, shape = 17) +
  ## Modos sem recorte
  geom_point(data = modos_hist_long |> filter(Recorte == "Sem recorte (≥ 0)"),
             aes(x = Moda, y = 0.003, color = Ano), size = 4, shape = 16) +
  scale_color_manual(values = cores_hist,
                     labels = c("SWE_1800" = "1800 (e₀=33)",
                                "SWE_1900" = "1900 (e₀=49)",
                                "SWE_1950" = "1950 (e₀=72)")) +
  labs(
    title    = "Transição da bimodalidade de d(x) — Sweden females",
    subtitle = "▲ = moda com recorte ≥ 40 | ● = argmax sem recorte",
    x = "Idade", y = "d(x)",
    color = NULL,
    caption = paste0(
      "1800 e 1900: argmax sem recorte retorna pico infantil (●≈0);\n",
      "recorte ≥ 40 identifica corretamente o pico senil (▲).\n",
      "1950: os dois convergem — pico senil já domina.\n",
      "Dados: HMD, Sweden females.")
  ) +
  scale_x_continuous(breaks = seq(0, 110, 10)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001))

ggsave("figs/fig3d_bimodalidade_painel.pdf", fig3d, width = 11, height = 5)
print(fig3d)

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

## ============================================================
##  : Cleveland dot plot com viridis
##  Mostra e0, mediana e moda por população, com ordenação clara
##  e gap M-e0 anotado
## ============================================================

## ── Preparar dados ───────────────────────────────────────────
tab_long4 <- tab_localizacao |>
  ## Ordenar populações: SWE antes de USA, 1960 antes de 2019
  mutate(
    Pais = str_extract(Populacao, "^[A-Z]+"),
    Ano  = str_extract(Populacao, "[0-9]+"),
    ## Label para o eixo y: país + ano + e0
    label = sprintf("%s %s\n(e₀ = %.1f)", Pais, Ano, e0),
    ## Ordem: SWE_1960, SWE_2019, USA_1960, USA_2019
    label = factor(label, levels = rev(c(
      sprintf("SWE 1960\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="SWE_1960"]),
      sprintf("SWE 2019\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="SWE_2019"]),
      sprintf("USA 1960\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="USA_1960"]),
      sprintf("USA 2019\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="USA_2019"])
    )))
  ) |>
  pivot_longer(
    cols      = c(e0, Mediana, Moda),
    names_to  = "Medida",
    values_to = "Valor"
  ) |>
  mutate(
    Medida = factor(Medida,
                    levels = c("e0", "Mediana", "Moda"),
                    labels = c("e₀ (média)", "Mediana", "Moda M"))
  )

## ── Paleta viridis (3 cores bem distinguíveis) ───────────────
## viridis: amarelo (Moda) → verde (Mediana) → roxo (e0)
## Invertemos para que e0 fique no tom mais escuro/sóbrio
pal4 <- viridis::viridis(3, begin = 0.15, end = 0.85,
                         direction = -1, option = "D")
names(pal4) <- c("Moda M", "Mediana", "e₀ (média)")

## ── Segmento conectando os três pontos (faixa horizontal) ────
tab_range4 <- tab_localizacao |>
  mutate(
    Pais  = str_extract(Populacao, "^[A-Z]+"),
    Ano   = str_extract(Populacao, "[0-9]+"),
    label = factor(
      sprintf("%s %s\n(e₀ = %.1f)", Pais, Ano, e0),
      levels = rev(c(
        sprintf("SWE 1960\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="SWE_1960"]),
        sprintf("SWE 2019\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="SWE_2019"]),
        sprintf("USA 1960\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="USA_1960"]),
        sprintf("USA 2019\n(e₀ = %.1f)", tab_localizacao$e0[tab_localizacao$Populacao=="USA_2019"])
      )))
  )

## ── Construir o gráfico ──────────────────────────────────────
fig4 <- ggplot() +
  ## 1. Segmento conectando e0 até Moda (range visual)
  geom_segment(
    data = tab_range4,
    aes(x = e0, xend = Moda, y = label, yend = label),
    color = "gray80", linewidth = 3.5, lineend = "round"
  ) +
  ## 2. Linha fina adicional e0 → Mediana (gap menor)
  geom_segment(
    data = tab_range4,
    aes(x = e0, xend = Mediana, y = label, yend = label),
    color = "gray60", linewidth = 1.2, lineend = "round"
  ) +
  ## 3. Pontos das três medidas
  geom_point(
    data = tab_long4,
    aes(x = Valor, y = label, color = Medida, shape = Medida),
    size = 4.5, stroke = 0.8
  ) +
  ## 4. Anotação do gap M - e0
  geom_text(
    data = tab_range4,
    aes(x = (e0 + Moda) / 2,
        y = label,
        label = sprintf("+%.1f a", Moda - e0)),
    vjust = -0.9, size = 3.0, color = "gray40", fontface = "italic"
  ) +
  ## Escalas
  scale_color_manual(values = pal4) +
  scale_shape_manual(
    values = c("e₀ (média)" = 16,
               "Mediana"    = 15,
               "Moda M"     = 17)
  ) +
  scale_x_continuous(
    breaks = seq(70, 95, 5),
    minor_breaks = seq(70, 95, 1),
    limits = c(70, 94)
  ) +
  ## Separador visual entre SWE e USA
  geom_hline(yintercept = 2.5,
             color = "gray85", linewidth = 0.5, linetype = "dashed") +
  ## Labels
  labs(
    title    = expression(
      bold("Medidas de localização: ") *
        e[0] * ", mediana e moda (" * M * ")"),
    subtitle = "Suécia e EUA, mulheres — 1960 e 2019",
    x        = "Idade (anos)",
    y        = NULL,
    color    = "Medida", shape = "Medida",
    caption  = paste0(
      "Segmento: amplitude e₀ → M. Anotação: gap M − e₀ em anos.\n",
      "Moda estimada por spline cúbico, recorte ≥ 40 anos ",
      "(Kannisto, 2001; Canudas-Romo, 2010).\n",
      "Dados: HMD, mulheres.")
  ) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    axis.text.y      = element_text(lineheight = 0.85, size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray95")
  )

ggsave("figs/fig4_e0_mediana_moda.pdf", fig4, width = 9, height = 5)
print(fig4)


cat("\n✓ Lab 1 concluído. Figuras salvas em figs/\n")