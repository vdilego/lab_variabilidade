# Laboratório: Variabilidade das Idades à Morte em Demografia

**Disciplina**: Doutorado em Demografia  
**Repositório**: [github.com/vdilego/lab_variabilidade](https://github.com/vdilego)  
**Plataforma**: [Posit Cloud](https://posit.cloud)

---

## Estrutura do Laboratório

O laboratório está dividido em **duas sessões de 1h30** cada:

| Sessão | Tema | Script |
|--------|------|--------|
| Lab 1 | Tábua de vida como distribuição · Idade modal (5 métodos) · Impacto do recorte etário | `R/lab1_moda.R` |
| Lab 2 | Medidas de variabilidade (`LifeIneq`) · $e^\dagger$ · Entropia · Theil · Threshold age | `R/lab2_variabilidade.R` |

Scripts auxiliares:
- `R/00_setup.R` — instalação de pacotes e configurações globais
- `R/funcoes_aux.R` — funções demográficas anotadas com referências

---

## Pacotes necessários

```r
# CRAN
install.packages(c("tidyverse", "ggplot2", "scales",
                   "patchwork", "HMDHFDplus", "splines",
                   "DemoDecomp"))

# GitHub — van Raalte, Riffe et al.
remotes::install_github("alysonvanraalte/LifeIneq")
```

---

## Dados

Os scripts usam dados do **Human Mortality Database (HMD)**.  
Registro gratuito em [mortality.org](https://www.mortality.org).  
Credenciais são pedidas interativamente — nunca as inclua no código.

Para rodar sem login, o script `R/00_setup.R` oferece dados embutidos
(taxas de mortalidade femininas da Suécia e EUA, anos selecionados).

---

## Referências dos repositórios utilizados

- **Threshold age** (Aburto et al., 2019):  
  <https://github.com/jmaburto/The-treshold-age-of-the-lifetable-Entropy>

- **LifeIneq** (van Raalte, Riffe et al.):  
  <https://github.com/alysonvanraalte/LifeIneq>

- **DemoDecomp** (Riffe, 2018):  
  <https://github.com/timriffe/DemoDecomp>

---

## Referências bibliográficas

- Wilmoth & Horiuchi (1999). *Demography*, 36(4), 475–495.
- Shkolnikov et al. (2003). *Demographic Research*, 8(11), 305–358.
- Vaupel, Zhang & van Raalte (2011). *BMJ Open*, 1(1), e000128.
- Aburto et al. (2019). *Demographic Research*, 41, 83–102.
- Aburto et al. (2020). *PNAS*, 117(10), 5250–5259.
- van Raalte, Sasson & Martikainen (2018). *Science*, 362, 1002–1004.
- Permanyer & Scholl (2019). *PLoS ONE*.
- Permanyer, Spijker, Blanes & Renteria (2018). *Demography*.
- Permanyer, Sasson & Villavicencio (2023). *JRSSA*, 186(2), 217–240.
- Canudas-Romo (2010). *Demographic Research*, 22, 421–438.
