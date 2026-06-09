## ============================================================
##  lab2_variabilidade.R
##  SESSÃO 2 — Medidas de variabilidade / disparidade
##  e-dagger, entropia, Gini, Theil, IQR, threshold age
##  Usando o pacote LifeIneq (van Raalte, Riffe et al.)
## ============================================================
##
##  Referências principais:
##  Shkolnikov et al. (2003)   Demographic Research 8(11)
##  Vaupel, Zhang & vRaalte (2011)  BMJ Open 1(1)
##  Aburto & van Raalte (2018) Demography 55(6): 2071-2096
##  Aburto et al. (2019)       Demographic Research 41: 83-102
##  Aburto et al. (2020)       PNAS 117(10): 5250-5259
##  van Raalte et al. (2018)   Science 362: 1002-1004
##  Permanyer & Scholl (2019)  PLoS ONE
##  Permanyer et al. (2018)    Demography
##  Permanyer, Sasson & Villavicencio (2023)  JRSSA 186(2)
##
##  Repositórios:
##  LifeIneq:  github.com/alysonvanraalte/LifeIneq
##  Threshold: github.com/jmaburto/The-treshold-age-of-the-lifetable-Entropy
## ============================================================

source("R/00_setup.R")
source("R/funcoes_aux.R")

## ============================================================
## PARTE 0 — Preparar tábuas de vida
## ============================================================
lt_swe2019 <- build_lt(dados_mx$SWE_2019, sex = "f")
lt_swe1960 <- build_lt(dados_mx$SWE_1960, sex = "f")
lt_usa2019 <- build_lt(dados_mx$USA_2019, sex = "f")
lt_usa1960 <- build_lt(dados_mx$USA_1960, sex = "f")

pop_lista <- list(
  SWE_1960 = lt_swe1960,
  SWE_2019 = lt_swe2019,
  USA_1960 = lt_usa1960,
  USA_2019 = lt_usa2019
)

## ============================================================
## PARTE 1 — e-dagger: interpretação como anos perdidos
## ============================================================

## 1.1  Calcular e† para cada população
edags <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao = nm,
    e0        = lt$ex[1],
    e_dagger  = edagger_lt(lt),
    H         = keyfitz_H(lt),
    G         = gini_lt(lt)
  )
})

cat("── e†, H e G para as quatro populações ──\n")
print(edags |> mutate(across(where(is.numeric), ~round(.x, 3))))

## 1.2  Verificar identidade G = H = e†/e0
cat("\n── Verificação: G = H = e†/e0 ──\n")
edags |>
  mutate(
    edagger_sobre_e0 = e_dagger / e0,
    G_menos_H        = abs(G - H),
    G_menos_edag_e0  = abs(G - edagger_sobre_e0)
  ) |>
  select(Populacao, H, G, edagger_sobre_e0,
         G_menos_H, G_menos_edag_e0) |>
  mutate(across(where(is.numeric), ~round(.x, 6))) |>
  print()

## 1.3  Visualizar d(x) ponderado por e(x) — o que e† mede
fig5 <- lt_swe2019 |>
  mutate(
    contrib_edag = ex * dx / lt_swe2019$lx[1],
    Regiao = if_else(age < threshold_age(lt_swe2019),
                     "Abaixo de a*", "Acima de a*")
  ) |>
  ggplot(aes(age, contrib_edag, fill = Regiao)) +
  geom_col(width = 0.9, alpha = 0.8) +
  geom_vline(xintercept = threshold_age(lt_swe2019),
             linewidth = 1.2, color = "black", linetype = "dashed") +
  annotate("text",
           x = threshold_age(lt_swe2019) + 1.5,
           y = max(lt_swe2019$ex * lt_swe2019$dx /
                     lt_swe2019$lx[1]) * 0.95,
           label = sprintf("a* = %.1f anos",
                           threshold_age(lt_swe2019)),
           hjust = 0, size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Abaixo de a*" = "#c00000",
               "Acima de a*"  = "#1f497d")) +
  labs(
    title    = expression(
      bold("Contribuição de cada idade para ") * e^"\u2020"),
    subtitle = expression("Área total = " * e^"\u2020" *
      " (anos de vida esperados perdidos em média)"),
    x = "Idade", y = expression(e(x) %.% d(x) / ell[0]),
    fill = NULL,
    caption = paste0(
      "Suécia 2019, mulheres. ",
      "e† = ", round(edagger_lt(lt_swe2019), 2), " anos. ",
      "Fonte: Vaupel, Zhang & van Raalte (2011).")
  ) +
  scale_x_continuous(breaks = seq(0, 110, 10))

ggsave("figs/fig5_contrib_edagger.pdf", fig5, width = 10, height = 5)
print(fig5)

## ============================================================
## PARTE 2 — Usando o pacote LifeIneq
## ============================================================
## Referência: van Raalte & Riffe (github.com/alysonvanraalte/LifeIneq)
## Vignette: rdrr.io/github/alysonvanraalte/LifeIneq/f/vignettes/

## 2.1  Calcular todas as medidas com LifeIneq
calcular_lifeineq <- function(lt, nome) {
  ## LifeIneq espera: age, dx, lx, ex, ax
  lt_in <- lt |> select(age, dx, lx, ex, ax)

  tibble(
    Populacao = nome,
    e0        = lt$ex[1],
    ## Medidas absolutas (em anos)
    SD        = LifeIneq::ineq_sd(age = lt$age, dx = lt$dx,
                                   ex = lt$ex, ax = lt$ax,
                                   distribution = FALSE),
    IQR       = iqr_lt(lt),   # não está no LifeIneq — usamos a nossa
    e_dagger  = LifeIneq::ineq_edag(age = lt$age, dx = lt$dx,
                                     lx = lt$lx, ex = lt$ex,
                                     ax = lt$ax),
    ## Medidas relativas (adimensionais)
    Gini      = LifeIneq::ineq_gini(age = lt$age, dx = lt$dx,
                                     lx = lt$lx, ex = lt$ex,
                                     ax = lt$ax),
    H_Keyfitz = LifeIneq::ineq_H(age = lt$age, dx = lt$dx,
                                   lx = lt$lx, ex = lt$ex,
                                   ax = lt$ax),
    Theil     = LifeIneq::ineq_theil(age = lt$age, dx = lt$dx,
                                      ex = lt$ex, ax = lt$ax,
                                      distribution = FALSE),
    ## Threshold age — da nossa função (baseada em Aburto et al. 2019)
    a_star    = threshold_age(lt)
  )
}

tab_ineq <- map2_dfr(pop_lista, names(pop_lista), calcular_lifeineq)

cat("\n── Tabela completa de indicadores (LifeIneq + auxiliares) ──\n")
print(tab_ineq |> mutate(across(where(is.numeric), ~round(.x, 3))))

## ============================================================
## PARTE 3 — Gráfico de teia: comparação de medidas
## ============================================================

fig6 <- tab_ineq |>
  select(Populacao, SD, IQR, e_dagger, Gini, H_Keyfitz, Theil) |>
  pivot_longer(-Populacao, names_to = "Medida", values_to = "Valor") |>
  ## Normalizar para comparação visual
  group_by(Medida) |>
  mutate(Valor_norm = (Valor - min(Valor)) /
           (max(Valor) - min(Valor) + 1e-9)) |>
  ungroup() |>
  ggplot(aes(Medida, Valor_norm,
             color = Populacao, group = Populacao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    SWE_1960 = "#1f497d", SWE_2019 = "#70b0d0",
    USA_1960 = "#c00000", USA_2019 = "#f08080")) +
  labs(
    title    = "Medidas de variabilidade normalizadas",
    subtitle = "0 = mínimo entre populações; 1 = máximo",
    x = NULL, y = "Valor normalizado [0,1]",
    color = NULL,
    caption = "Medidas calculadas com LifeIneq + funções auxiliares."
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("figs/fig6_medidas_comparadas.pdf", fig6, width = 9, height = 5)
print(fig6)

## ============================================================
## PARTE 4 — Entropia de Keyfitz como elasticidade
## ============================================================

## 4.1  Verificação numérica da interpretação como elasticidade
## Se mu_delta(x) = (1-delta)*mu(x), delta e0 / e0 ≈ H * delta

delta <- 0.10  # redução de 10% em todas as taxas

verificar_elasticidade <- function(lt, nome, delta) {
  ## Nova tábua com mu reduzido proporcionalmente
  lt_new <- build_lt(lt$mx * (1 - delta), sex = "f")

  e0_orig  <- lt$ex[1]
  e0_new   <- lt_new$ex[1]
  ganho_rel <- (e0_new - e0_orig) / e0_orig
  H         <- keyfitz_H(lt)
  previsao  <- H * delta

  tibble(
    Populacao     = nome,
    e0_original   = round(e0_orig,   2),
    e0_nova       = round(e0_new,    2),
    ganho_e0_anos = round(e0_new - e0_orig, 2),
    ganho_rel_pct = round(100 * ganho_rel, 2),
    H_Keyfitz     = round(H, 4),
    previsao_pct  = round(100 * previsao, 2),
    erro_pct      = round(abs(ganho_rel_pct - previsao_pct), 3)
  )
}

tab_elast <- map2_dfr(pop_lista, names(pop_lista),
                       ~ verificar_elasticidade(.x, .y, delta))

cat(sprintf("\n── Verificação: H como elasticidade (delta = %.0f%%) ──\n",
            100 * delta))
print(tab_elast)
cat("  erro_pct: diferença absoluta entre ganho real e previsão H*delta\n")

## 4.2  Curva de e0(delta) — o quanto e0 cresce com delta
delta_seq <- seq(0, 0.5, by = 0.01)

curva_e0_delta <- function(lt, nome) {
  map_dfr(delta_seq, function(d) {
    lt_d <- build_lt(lt$mx * (1 - d), sex = "f")
    tibble(
      Populacao = nome,
      delta     = d,
      e0        = lt_d$ex[1]
    )
  })
}

curvas <- map2_dfr(pop_lista, names(pop_lista), curva_e0_delta)

## Tangentes no delta=0: inclinação = e† = H * e0
tangentes <- tab_ineq |>
  mutate(
    intercept = tab_ineq$e0 - e_dagger * 0,
    slope     = e_dagger
  )

fig7 <- curvas |>
  ggplot(aes(delta, e0, color = Populacao, group = Populacao)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    SWE_1960 = "#1f497d", SWE_2019 = "#70b0d0",
    USA_1960 = "#c00000", USA_2019 = "#f08080")) +
  labs(
    title = expression(
      "Expectativa de vida " * e[0](delta) *
      " sob redução proporcional " * delta * " de " * mu(x)),
    subtitle = expression(
      "Tangente em " * delta * "=0 tem inclinação " * e^"\u2020"),
    x = expression(delta * " (redução proporcional em " *
                   mu(x) * ")"),
    y = expression(e[0](delta) ~ "(anos)"),
    color = NULL,
    caption = paste0(
      "Keyfitz (1977); Vaupel, Zhang & van Raalte (2011).\n",
      "Elasticidade ≈ H = e†/e0 (válida para δ pequeno).")
  )

ggsave("figs/fig7_e0_delta.pdf", fig7, width = 9, height = 5)
print(fig7)

## ============================================================
## PARTE 5 — Threshold age a*
## ============================================================
## Baseado em: Aburto, Alvarez, Villavicencio & Vaupel (2019)
## Demographic Research 41: 83-102
## Repositório: github.com/jmaburto/The-treshold-age-of-the-lifetable-Entropy

## 5.1  Calcular a* para todas as populações
tab_astar <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  a  <- threshold_age(lt)
  e0 <- lt$ex[1]
  H  <- keyfitz_H(lt)
  ## proporção de mortes acima de a* deve ser ≈ H
  prop_acima <- sum(lt$dx[lt$age >= a]) / sum(lt$dx)
  tibble(
    Populacao    = nm,
    e0           = round(e0, 2),
    a_star       = round(a, 1),
    H_Keyfitz    = round(H, 4),
    prop_acima_H = round(prop_acima, 4),   # deve ≈ H
    verif        = round(abs(H - prop_acima), 5)
  )
})

cat("\n── Threshold age a* — verificação: prop(T > a*) ≈ H ──\n")
print(tab_astar)

## 5.2  Visualizar e(x) cruzando e0 = a* para Suécia 2019
lt <- lt_swe2019
a_star_val <- threshold_age(lt)
e0_val     <- lt$ex[1]

fig8 <- ggplot(lt, aes(age, ex)) +
  geom_line(color = "#1f497d", linewidth = 1.3) +
  ## linha horizontal em e0
  geom_hline(yintercept = e0_val, linetype = "dashed",
             color = "#c00000", linewidth = 0.9) +
  ## linha vertical em a*
  geom_vline(xintercept = a_star_val, linetype = "dotdash",
             color = "darkorange", linewidth = 0.9) +
  ## ponto de cruzamento
  geom_point(aes(x = a_star_val, y = e0_val),
             color = "darkorange", size = 4) +
  ## regiões coloridas
  annotate("rect",
           xmin = -1, xmax = a_star_val,
           ymin = -Inf, ymax = Inf,
           fill = "#c00000", alpha = 0.06) +
  annotate("rect",
           xmin = a_star_val, xmax = 115,
           ymin = -Inf, ymax = Inf,
           fill = "#1f497d", alpha = 0.06) +
  ## anotações
  annotate("text", x = a_star_val / 2, y = max(lt$ex) * 0.92,
           label = "Redução de μ(x)\naum enta e†",
           color = "#c00000", size = 3.5, fontface = "italic") +
  annotate("text", x = a_star_val + 5, y = max(lt$ex) * 0.92,
           label = "Redução de μ(x)\ndiminui e†",
           color = "#1f497d", size = 3.5, fontface = "italic",
           hjust = 0) +
  annotate("text",
           x = a_star_val + 1.5, y = e0_val + 1.5,
           label = sprintf("a* = %.1f anos\ne(a*) = e₀ = %.2f",
                           a_star_val, e0_val),
           color = "darkorange", hjust = 0, size = 3.5) +
  labs(
    title    = expression(
      "Esperança de vida " * e(x) * " e o threshold age " * a^"*"),
    subtitle = expression(
      a^"*" * ": onde " * e(a^"*") == e[0]),
    x = "Idade", y = expression(e(x) ~ "(anos)"),
    caption = paste0(
      "Suécia 2019, mulheres. ",
      "Aburto, Alvarez, Villavicencio & Vaupel (2019), Demographic Research 41.\n",
      "Repositório: github.com/jmaburto/The-treshold-age-of-the-lifetable-Entropy")
  ) +
  scale_x_continuous(breaks = seq(0, 110, 10))

ggsave("figs/fig8_threshold_age.pdf", fig8, width = 10, height = 5)
print(fig8)

## 5.3  Comparar a* entre populações
fig9 <- tab_astar |>
  ggplot(aes(x = e0, y = a_star, label = Populacao,
             color = str_extract(Populacao, "^[A-Z]+"))) +
  geom_point(size = 4) +
  ggrepel::geom_label_repel(size = 3.2, nudge_y = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray50") +
  scale_color_manual(values = cores, guide = "none") +
  labs(
    title    = expression(
      "Threshold age " * a^"*" * " vs expectativa de vida " * e[0]),
    subtitle = "Linha tracejada: a* = e0",
    x        = expression(e[0] ~ "(anos)"),
    y        = expression(a^"*" ~ "(anos)"),
    caption  = "Aburto et al. (2019): a* < e0 em populações com mortalidade moderna."
  )

ggsave("figs/fig9_astar_e0.pdf", fig9, width = 7, height = 6)
print(fig9)

## ============================================================
## PARTE 6 — Trajetória histórica: e0 × e†
## (estilo Aburto et al., PNAS 2020)
## ============================================================

serie_historica <- bind_rows(
  resumo_lt(lt_swe1960, "SWE 1960"),
  resumo_lt(lt_swe2019, "SWE 2019"),
  resumo_lt(lt_usa1960, "USA 1960"),
  resumo_lt(lt_usa2019, "USA 2019")
) |>
  mutate(
    pais = str_extract(Populacao, "^[A-Z]+"),
    ano  = str_extract(Populacao, "[0-9]+")
  )

fig10 <- ggplot(serie_historica,
                aes(e0, e_dagger, color = pais, group = pais)) +
  geom_path(linewidth = 1.2,
            arrow = arrow(length = unit(0.35, "cm"),
                          type = "closed")) +
  geom_point(size = 3.5) +
  ggrepel::geom_label_repel(
    aes(label = Populacao),
    size = 3, nudge_y = 0.3) +
  scale_color_manual(values = cores, guide = "none") +
  labs(
    title    = expression(
      "Trajetória histórica: " * e[0] * " e " * e^"\u2020"),
    subtitle = "Seta indica direção temporal (1960 → 2019)",
    x        = expression(e[0] ~ "(anos)"),
    y        = expression(e^"\u2020" ~ "(anos perdidos em média)"),
    caption  = paste0(
      "Estilo: Aburto et al. (2020), PNAS 117(10).\n",
      "e† = G × e0 = H × e0 (Vaupel, Zhang & van Raalte, 2011).")
  )

ggsave("figs/fig10_trajetoria.pdf", fig10, width = 8, height = 6)
print(fig10)

## ============================================================
## PARTE 7 — Índice de Theil: decomposição entre/dentro
## (Permanyer & Scholl, 2019; Permanyer, Sasson & Villavicencio, 2023)
## ============================================================

## 7.1  Theil com recorte 0 vs 15+ (Permanyer & Scholl, 2019)
tab_theil <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(
    Populacao  = nm,
    e0         = round(lt$ex[1], 2),
    Theil_0    = round(theil_lt(lt, 0),  4),
    Theil_15   = round(theil_lt(lt, 15), 4)
  )
})

cat("\n── Theil total e adulto (15+) — Permanyer & Scholl (2019) ──\n")
print(tab_theil)

## 7.2  Decomposição entre grupos etários
## Seguindo a estrutura de Permanyer et al. (2018):
## T_total = T_entre + soma(w_k * T_k)

theil_decomp <- function(lt, breaks = c(0, 15, 50, Inf),
                          labels = c("0-14", "15-49", "50+")) {
  e0 <- lt$ex[1]
  lt_g <- lt |>
    mutate(grupo = cut(age, breaks = breaks,
                       labels = labels, right = FALSE))

  res <- lt_g |>
    group_by(grupo) |>
    summarise(
      s_k    = sum(dx) / sum(lt$dx),           # fração de mortes
      e_k    = weighted.mean(age + ax, dx),    # e0 do grupo
      T_k    = theil_lt(cur_data(), 0),        # Theil interno
      .groups = "drop"
    )

  T_entre  <- sum(res$s_k * (res$e_k / e0) * log(res$e_k / e0),
                  na.rm = TRUE)
  T_dentro <- sum(res$s_k * res$T_k, na.rm = TRUE)
  T_total  <- T_entre + T_dentro

  list(
    grupos   = res,
    T_entre  = T_entre,
    T_dentro = T_dentro,
    T_total  = T_total,
    pct_entre = 100 * T_entre  / T_total,
    pct_dentro= 100 * T_dentro / T_total
  )
}

cat("\n── Decomposição do Theil — Suécia 2019 ──\n")
dec_swe <- theil_decomp(lt_swe2019)
cat(sprintf("  T total  = %.4f\n", dec_swe$T_total))
cat(sprintf("  T entre  = %.4f (%.1f%%)\n",
            dec_swe$T_entre, dec_swe$pct_entre))
cat(sprintf("  T dentro = %.4f (%.1f%%)\n",
            dec_swe$T_dentro, dec_swe$pct_dentro))
print(dec_swe$grupos |>
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n── Decomposição do Theil — EUA 2019 ──\n")
dec_usa <- theil_decomp(lt_usa2019)
cat(sprintf("  T total  = %.4f\n", dec_usa$T_total))
cat(sprintf("  T entre  = %.4f (%.1f%%)\n",
            dec_usa$T_entre, dec_usa$pct_entre))
cat(sprintf("  T dentro = %.4f (%.1f%%)\n",
            dec_usa$T_dentro, dec_usa$pct_dentro))

## 7.3  Gráfico: contribuição dos grupos para o Theil
grupos_plot <- bind_rows(
  dec_swe$grupos |> mutate(Populacao = "SWE 2019"),
  dec_usa$grupos |> mutate(Populacao = "USA 2019")
) |>
  mutate(contrib_abs = s_k * T_k)

fig11 <- grupos_plot |>
  ggplot(aes(grupo, contrib_abs, fill = Populacao)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
  scale_fill_manual(
    values = c("SWE 2019" = "#1f497d", "USA 2019" = "#c00000")) +
  labs(
    title    = "Contribuição de cada grupo etário para o Theil",
    subtitle = expression(
      "Componente dentro: " * Sigma * " w[k] × T[k]"),
    x = "Grupo etário", y = "Contribuição para T",
    fill = NULL,
    caption = paste0(
      "Decomposição: Permanyer & Scholl (2019); ",
      "Permanyer, Sasson & Villavicencio (2023), JRSSA 186(2).")
  )

ggsave("figs/fig11_theil_decomp.pdf", fig11, width = 8, height = 5)
print(fig11)

## ============================================================
## PARTE 8 — Curva de Lorenz da tábua de vida
## ============================================================

lorenz_lt <- function(lt, nome) {
  tibble(
    Populacao  = nome,
    F_mortes   = c(0, cumsum(lt$dx) / sum(lt$dx)),
    L_anos     = c(0, cumsum(lt$Lx) / sum(lt$Lx))
  )
}

lor_todas <- map2_dfr(pop_lista, names(pop_lista), lorenz_lt)

## Calcular Gini para label
ginis <- map2_dfr(pop_lista, names(pop_lista), function(lt, nm) {
  tibble(Populacao = nm, G = round(gini_lt(lt), 3))
})

lor_todas <- lor_todas |>
  left_join(ginis, by = "Populacao") |>
  mutate(label = sprintf("%s (G = %.3f)", Populacao, G))

fig12 <- ggplot(lor_todas, aes(F_mortes, L_anos,
                                color = Populacao,
                                group = Populacao)) +
  geom_line(linewidth = 1.1) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray40") +
  scale_color_manual(
    values = c(SWE_1960 = "#1f497d", SWE_2019 = "#70b0d0",
               USA_1960 = "#c00000", USA_2019 = "#f08080"),
    labels = setNames(lor_todas$label[!duplicated(lor_todas$label)],
                      lor_todas$Populacao[!duplicated(lor_todas$Populacao)])
  ) +
  coord_equal() +
  labs(
    title    = "Curva de Lorenz da tábua de vida",
    subtitle = "Eixo x: proporção acumulada de mortes | Eixo y: proporção acumulada de Lx",
    x = "Proporção acumulada de mortes F(x)",
    y = "Proporção acumulada de Lx",
    color = NULL,
    caption = "Shkolnikov, Andreev & Begun (2003), Demographic Research 8(11)."
  )

ggsave("figs/fig12_lorenz.pdf", fig12, width = 7, height = 7)
print(fig12)

## ============================================================
## PARTE 9 — Exercício final integrador
## ============================================================
cat("\n")
cat("═══════════════════════════════════════════════════════\n")
cat("EXERCÍCIO FINAL — Responda com base nos resultados acima\n")
cat("═══════════════════════════════════════════════════════\n")
cat("\n")
cat("1. Para a Suécia 2019, uma redução de 5% em todas as taxas\n")
cat("   de mortalidade aumentaria e0 em quantos anos?\n")
cat("   (Use a interpretação de H como elasticidade)\n")
H_swe2019  <- keyfitz_H(lt_swe2019)
e0_swe2019 <- lt_swe2019$ex[1]
cat(sprintf("   Resposta: %.2f × %.4f × %.2f ≈ %.2f anos\n",
            e0_swe2019, H_swe2019, 0.05,
            e0_swe2019 * H_swe2019 * 0.05))
cat("\n")
cat("2. Uma redução de mortalidade concentrada nas idades ACIMA\n")
cat("   de a* =", round(threshold_age(lt_swe2019), 1),
    "anos na Suécia 2019 aumentaria ou\n")
cat("   diminuiria e†? (Use o resultado de Aburto et al., 2019)\n")
cat("   Resposta: DIMINUIRIA e† — o que reduz a desigualdade.\n")
cat("\n")
cat("3. Compare G e Theil para SWE_2019 vs USA_2019.\n")
cat("   Qual medida mostra a maior diferença relativa entre países?\n")
tab_ineq |>
  filter(Populacao %in% c("SWE_2019", "USA_2019")) |>
  select(Populacao, Gini, Theil, e_dagger) |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print()

cat("\n✓ Lab 2 concluído. Todas as figuras salvas em figs/\n")
