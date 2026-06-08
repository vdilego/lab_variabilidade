# Configuração no Posit Cloud

## Passo a passo para montar o projeto no Posit Cloud

### 1. Criar novo projeto

1. Acesse [posit.cloud](https://posit.cloud) e faça login
2. Clique em **New Project → New Project from Git Repository**
3. Cole a URL: `https://github.com/vdilego/lab_variabilidade`

Alternativamente, crie um projeto vazio e faça upload dos arquivos:
- `R/00_setup.R`
- `R/funcoes_aux.R`
- `R/lab1_moda.R`
- `R/lab2_variabilidade.R`

---

### 2. Instalar pacotes (primeira execução)

Execute no console do Posit Cloud:

```r
source("R/00_setup.R")
```

Este script instala automaticamente todos os pacotes necessários,
incluindo o `LifeIneq` do GitHub.

**Tempo estimado**: 5–10 minutos na primeira execução.

---

### 3. Criar pasta de figuras

```r
dir.create("figs", showWarnings = FALSE)
```

---

### 4. Executar o Lab 1

```r
source("R/funcoes_aux.R")
source("R/lab1_moda.R")
```

**Tempo estimado**: ~20 minutos

---

### 5. Executar o Lab 2

```r
source("R/lab2_variabilidade.R")
```

**Tempo estimado**: ~25 minutos

---

### 6. Dados do HMD (opcional)

Para usar dados reais do HMD, registre-se em [mortality.org](https://mortality.org)
e substitua o bloco de dados embutidos no início de cada script:

```r
library(HMDHFDplus)
usr <- readline("HMD username: ")
pwd <- readline("HMD password: ")

lt_swe2019 <- readHMDweb("SWE", "fltper_1x1", usr, pwd) |>
  filter(Year == 2019) |>
  mutate(mx = mx) |>
  pull(mx) |>
  build_lt(sex = "f")
```

---

### Notas para o Posit Cloud

- **Memória**: o plano gratuito tem 1 GB de RAM, suficiente para este lab
- **Persistência**: os projetos ficam salvos na sua conta
- **Compartilhamento**: use *Share > Access: Everyone* para que os alunos acessem
- Os pacotes precisam ser reinstalados se o ambiente for reiniciado
  (use `renv` para ambientes reprodutíveis — ver abaixo)

---

### Reprodutibilidade com `renv` (recomendado para turma)

```r
install.packages("renv")
renv::init()
# Após instalar todos os pacotes:
renv::snapshot()
# Os alunos restauram com:
renv::restore()
```
