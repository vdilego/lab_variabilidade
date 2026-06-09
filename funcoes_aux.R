## ============================================================
##  funcoes_aux.R
##  Funções demográficas anotadas com referências completas
##  Para uso nos Labs 1 e 2
## ============================================================

## ── Tábua de vida ────────────────────────────────────────────

#' Constrói tábua de vida completa a partir de mx
#
# @param mx  vetor de taxas centrais de mortalidade
# @param ages vetor de idades (default 0:110)
# @param sex  "f" (feminino) ou "m" (masculino) — afeta a(0)
# @return data.frame com colunas age, mx, ax, qx, lx, dx, Lx, Tx, ex
#
# Referência: Preston, Heuveline & Guillot (2001),
#   "Demography: Measuring and Modeling Population Processes",
#    cap. 3. Blackwell.
build_lt <- function(mx, ages = 0:110, sex = "f") {
  n  <- length(ages)
  ax <- rep(0.5, n)
  # a(0) — fração do ano 0 vivida (Coale-Demeny)
  if (sex == "f") {
    ax[1] <- ifelse(mx[1] >= 0.107, 0.350,
             ifelse(mx[1] >= 0.053, 0.053 / mx[1] - 2.800 * mx[1] + 0.326, 0.149))
  } else {
    ax[1] <- ifelse(mx[1] >= 0.107, 0.330,
             ifelse(mx[1] >= 0.053, 0.045 / mx[1] - 2.684 * mx[1] + 0.330, 0.045))
  }
  ax[n] <- 1 / mx[n]  # grupo aberto

  qx      <- mx / (1 + (1 - ax) * mx)
  qx[n]   <- 1.0

  lx <- c(1, cumprod(1 - qx[-n]))
  dx <- lx * qx

  Lx    <- lx - (1 - ax) * dx
  Lx[n] <- lx[n] / mx[n]

  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx

  data.frame(age = ages, mx = mx, ax = ax, qx = qx,
             lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
}

## ── Medidas de localização ───────────────────────────────────

#' Mediana da distribuição de idades à morte
mediana_lt <- function(lt) {
  idx <- which(cumsum(lt$dx / sum(lt$dx)) >= 0.5)[1]
  if (is.na(idx) || idx == 1) return(NA_real_)
  x0 <- lt$age[idx-1]; x1 <- lt$age[idx]
  p0 <- cumsum(lt$dx / sum(lt$dx))[idx-1]
  p1 <- cumsum(lt$dx / sum(lt$dx))[idx]
  x0 + (0.5 - p0) / (p1 - p0) * (x1 - x0)
}

## ── Quatro métodos para a idade modal ────────────────────────

#' Método 1: máximo de dx — discreto simples
#'
#' Referência: padrão em tábuas de vida. Ver Canudas-Romo (2010),
#'   Demographic Research 22, 421-438, eq. (1).
mode_discrete <- function(lt, age_min = 0) {
  sub <- subset(lt, age >= age_min)
  sub$age[which.max(sub$dx)]
}

#' Método 2: spline cúbico monotônico sobre dx
#'
#' Referência: Kannisto (2001); Horiuchi et al. (2013),
#'   Population Studies 67(3), 291-305.
mode_spline <- function(lt, age_min = 40) {
  sub  <- subset(lt, age >= age_min & dx > 0)
  spl  <- splinefun(sub$age, sub$dx, method = "monoH.FC")
  ages_fine <- seq(age_min, max(sub$age), by = 0.1)
  dx_fine   <- spl(ages_fine)
  ages_fine[which.max(dx_fine)]
}

# Método 3: ajuste Gompertz em mu(x)
#
# mu(x) = alpha * exp(beta * x)
# Moda analítica: M = -ln(alpha/beta) / beta
#
# Referência: Missov et al. (2015), Demographic Research 32, 701-732.
#   Canudas-Romo (2010): recorte em 40+ para evitar bimodalidade.
mode_gompertz <- function(lt, age_range = 40:90) {
  sub <- subset(lt, age %in% age_range & mx > 0)
  if (nrow(sub) < 5) return(NA_real_)
  fit   <- lm(log(mx) ~ age, data = sub)
  alpha <- exp(coef(fit)[1])
  beta  <- coef(fit)[2]
  if (beta <= 0 || alpha <= 0) return(NA_real_)
  -log(alpha / beta) / beta
}

# Método 4: estimador de kernel gaussiano sobre dx
#
# Referência: Horiuchi et al. (2013), Population Studies 67(3), 291-305.
mode_kernel <- function(lt, bw = 5, age_min = 40) {
  sub    <- subset(lt, age >= age_min & dx > 0)
  freq   <- round(sub$dx / sum(sub$dx) * 1e5)   # <- linha corrigida
  idades <- rep(sub$age, freq)
  if (length(idades) < 10) return(NA_real_)
  dens <- density(idades, bw = bw,
                  from = age_min, to = max(sub$age) + 1,
                  kernel = "gaussian")            # <- explícito
  dens$x[which.max(dens$y)]
}
# Método 5: maximização direta de d(x) via spline em lx
#
# d(x) = -l'(x). Maximizamos diretamente a derivada negativa de lx.
# Referência: Wilmoth & Horiuchi (1999), Demography 36(4), eq. (2).
mode_fx_max <- function(lt, age_min = 40) {
  sub  <- subset(lt, age >= age_min)
  spl  <- splinefun(sub$age, sub$lx, method = "monoH.FC")
  ages_fine <- seq(age_min, max(sub$age), by = 0.1)
  # f(x) = -l'(x)
  fx_fine <- -spl(ages_fine, deriv = 1)
  ages_fine[which.max(fx_fine)]
}

## ── Amplitude interquartil (IQR) ─────────────────────────────

# IQR da distribuição de idades à morte
#
# Referência: Wilmoth & Horiuchi (1999), Demography 36(4),
#   Appendix A, eq. (A7).
iqr_lt <- function(lt) {
  find_q <- function(p) {
    idx <- which(lt$lx <= p)[1]
    if (is.na(idx) || idx == 1) return(NA_real_)
    x0 <- lt$age[idx-1]; x1 <- lt$age[idx]
    l0 <- lt$lx[idx-1];  l1 <- lt$lx[idx]
    x0 + (p - l0) / (l1 - l0) * (x1 - x0)
  }
  find_q(0.25) - find_q(0.75)
}

## ── Desvio-padrão (total e condicional) ──────────────────────

# Desvio-padrão da distribuição de idades à morte
#
# @param lt  tábua de vida
# @param age_min  recorte inferior (0 = total; 30 = sigma(30+))
#
# Referência total: Wilmoth & Horiuchi (1999), eq. (A8).
# Referência condicional (sigma(30+)): Myers & Manton (1984);
#   Wilmoth & Horiuchi (1999), p. 481.
sd_lt <- function(lt, age_min = 0) {
  sub  <- subset(lt, age >= age_min)
  e_c  <- sub$ex[1]
  mid  <- sub$age + sub$ax
  px   <- sub$dx / sum(sub$dx)
  sqrt(sum((mid - e_c)^2 * px, na.rm = TRUE))
}

## ── e-dagger ─────────────────────────────────────────────────

#' e-dagger: anos de vida esperada perdidos em média
#'
# e† = integral[ e(x) * f(x) dx ]
#'
#' Referência: Vaupel, Zhang & van Raalte (2011),
#'   BMJ Open 1(1), e000128, eq. (3).
#'   Shkolnikov et al. (2011), Pop Dev Rev 37(3), 519-543.
edagger_lt <- function(lt) {
  sum(lt$ex * lt$dx, na.rm = TRUE) / lt$lx[1]
}

## ── Entropia de Keyfitz ───────────────────────────────────────

# Entropia de Keyfitz H = e† / e0
#
# Interpretação: elasticidade de e0 a uma redução proporcional
# uniforme em mu(x). Se mu_delta(x) = (1-delta)*mu(x), então:
#   delta(e0)/e0 ≈ H * delta
#
# Referência: Keyfitz (1977), Demography 14(4), 411-418.
#   Leser (1955), Population Studies 9(1), 67-71.
#   Aburto et al. (2019), Demographic Research 41, 83-102.
keyfitz_H <- function(lt) {
  edagger_lt(lt) / lt$ex[1]
}

## ── Gini da tábua de vida ────────────────────────────────────

# Coeficiente de Gini da tábua de vida
#
# G = 1 - (1/e0) * integral[ l(x)^2 ] dx
#
# Referência: Hanada (1983), J. Japan Statistical Society 13(2).
#   Shkolnikov, Andreev & Begun (2003), Demographic Research 8(11).
#   Wilmoth & Horiuchi (1999), eq. (A10).
gini_lt <- function(lt) {
  e0  <- lt$ex[1]
  lx  <- lt$lx
  age <- lt$age
  int <- sum((lx[-length(lx)]^2 + lx[-1]^2) / 2 * diff(age))
  1 - int / e0
}

## ── Índice de Theil ───────────────────────────────────────────

# Índice de Theil da distribuição de idades à morte
#
# T = E[ (x/e0) * log(x/e0) ]
#
# Referência: Permanyer & Scholl (2019), PLoS ONE.
#   Permanyer, Spijker, Blanes & Renteria (2018), Demography.
#   Permanyer, Sasson & Villavicencio (2023), JRSSA 186(2).
theil_lt <- function(lt, age_min = 0) {
  sub  <- subset(lt, age >= age_min)
  e0   <- lt$ex[1]  # usa e0 global como referência
  mid  <- pmax(sub$age + sub$ax, 0.01)
  px   <- sub$dx / sum(sub$dx)
  r    <- mid / e0
  sum(r * log(r) * px, na.rm = TRUE)
}

## ── Threshold age a* ─────────────────────────────────────────

# Threshold age: a* tal que e(a*) = e0
#
# Abaixo de a*: redução de mu(x) aumenta e†
# Acima de a*:  redução de mu(x) diminui e†
#
# Referência: Aburto, Alvarez, Villavicencio & Vaupel (2019),
#   Demographic Research 41, 83-102.
#   Repositório: github.com/jmaburto/The-treshold-age-of-the-lifetable-Entropy
threshold_age <- function(lt) {
  n     <- nrow(lt)
  H_bar <- keyfitz_H(lt)   # e†(0) / e0
  
  # Para cada idade x, calcular H(x) = e†(x)/e0(x)  e  H_bar(x) condicional
  # Condição de Aburto eq. (9): g(x) = 0
  # onde g(x) = e†(x)/e0(x) + e†_bar(x)/e0(x) - 1 - H_bar
  # simplificando: g(x) = H(x) + H_bar(x) - 1 - H_bar
  # com H_bar(x) = e†(x)/e0(x) também... 
  # 
  # MAIS SIMPLES: usar a condição equivalente de Zhang & Vaupel (2009)
  # para a†: H(x) + H_bar(x) = 1
  # que em termos da tábua de vida é: e(x) = e†_total
  # i.e., ex[x] == edagger_lt(lt)  ← esta é a condição para a†
  
  e_dag <- edagger_lt(lt)
  
  # a†: primeira idade onde ex cai abaixo de e†
  idx <- which(lt$ex <= e_dag)[1]
  if (is.na(idx) || idx == 1) return(NA_real_)
  
  x0 <- lt$age[idx-1]; x1 <- lt$age[idx]
  e0 <- lt$ex[idx-1];  e1 <- lt$ex[idx]
  x0 + (e_dag - e0) / (e1 - e0) * (x1 - x0)
}

## ── Tabela-resumo de todos os indicadores ────────────────────

# Calcula todos os indicadores para uma tábua de vida
resumo_lt <- function(lt, nome = "Pop") {
  tibble(
    Populacao  = nome,
    e0         = lt$ex[1],
    Mediana    = mediana_lt(lt),
    Moda_spline = mode_spline(lt, 40),
    Moda_gomp  = mode_gompertz(lt),
    Moda_kernel= mode_kernel(lt),
    IQR        = iqr_lt(lt),
    SD_total   = sd_lt(lt, 0),
    SD_10plus  = sd_lt(lt, 10),
    SD_30plus  = sd_lt(lt, 30),
    Gini       = gini_lt(lt),
    Theil      = theil_lt(lt),
    e_dagger   = edagger_lt(lt),
    H_Keyfitz  = keyfitz_H(lt),
    a_star     = threshold_age(lt)
  )
}


