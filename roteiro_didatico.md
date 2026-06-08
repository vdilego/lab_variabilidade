# Roteiro Didático — Laboratório de Variabilidade das Idades à Morte

**Disciplina**: Doutorado em Demografia  
**Duração**: 2 sessões × 1h30  
**Plataforma**: Posit Cloud  

---

## Sessão 1 — A Tábua de Vida como Distribuição e a Idade Modal

### Objetivos de aprendizagem

Ao final desta sessão, o aluno deverá ser capaz de:

1. Construir uma tábua de vida completa a partir de $m_x$ em R
2. Interpretar $f(x) = d(x)/\ell_0$ como densidade de probabilidade
3. Calcular a moda por cinco métodos diferentes
4. Explicar por que o recorte etário em 40 anos (Canudas-Romo, 2010) é
   uma solução de **identificação algorítmica**, não de sensibilidade
5. Descrever a relação entre $e_0$, mediana e moda ao longo da transição epidemiológica

### Estrutura (90 min)

| Tempo | Atividade |
|-------|-----------|
| 0–10  | Motivação: por que $e_0$ não conta toda a história (Wilmoth & Horiuchi, 1999) |
| 10–25 | Build_lt em R: construção e interpretação das colunas |
| 25–50 | Cinco métodos para a moda — comparação visual com `fig2` |
| 50–70 | Impacto do recorte etário — bimodalidade com `fig3` |
| 70–90 | Exercício: $e_0$ vs mediana vs moda nas 4 populações — `fig4` |

### Perguntas de discussão

- Por que `mode_discrete()` sem recorte pode retornar a moda errada
  para a Suécia 1960?
- O que o gráfico `fig3` (bimodalidade) revela sobre o processo
  de transição epidemiológica?
- Por que $M > e_0$ em populações com alta mortalidade infantil?

---

## Sessão 2 — Medidas de Variabilidade e Threshold Age

### Objetivos de aprendizagem

1. Calcular e interpretar $e^\dagger$, $\mathcal{H}$, $G$, Theil, IQR, $\sigma$ com `LifeIneq`
2. Verificar numericamente a identidade $G = \mathcal{H} = e^\dagger/e_0$
3. Interpretar $\mathcal{H}$ como elasticidade de $e_0$ a reduções proporcionais em $\mu(x)$
4. Identificar o threshold age $a^*$ e interpretar sua implicação para políticas
5. Decompor o Theil entre e dentro de grupos etários

### Estrutura (90 min)

| Tempo | Atividade |
|-------|-----------|
| 0–15  | Intuição de $e^\dagger$ via `fig5` — o que cada morte contribui |
| 15–35 | Pacote `LifeIneq`: todas as medidas numa linha — tabela comparativa |
| 35–50 | Entropia como elasticidade: verificação numérica com `tab_elast` e `fig7` |
| 50–70 | Threshold age $a^*$: cálculo, visualização (`fig8`), implicações (`fig9`) |
| 70–90 | Theil: decomposição entre/dentro de grupos (`fig11`) + exercício final |

### Perguntas de discussão

- `tab_elast` mostra que o erro entre o ganho real em $e_0$ e a previsão
  $\mathcal{H} \times \delta$ é pequeno. Para que valores de $\delta$ a
  aproximação começa a deteriorar?
- Na Suécia 2019, $a^* \approx$ 70 anos. O que isso implica para a
  eficácia de programas de redução de mortalidade por doenças
  cardiovasculares vs. mortalidade infantil?
- Por que o componente "dentro de grupos" domina o Theil total?
  (Permanyer et al., 2018)

---

## Exercício Final Integrador

**Contexto**: Uma política de saúde propõe reduzir $\mu(x)$ em 10%
apenas para idades abaixo de $a^*$ em um país com mortalidade similar
à Suécia 2019.

1. Qual seria o efeito esperado sobre $e_0$?
2. Qual seria o efeito sobre $e^\dagger$ (aumenta ou diminui)? Por quê?
3. Se a mesma redução de 10% fosse aplicada apenas acima de $a^*$,
   como os efeitos se inverteriam?
4. Compare com os EUA 2019: qual país tem maior "potencial" de ganho
   em $e_0$ por redução proporcional de $\mu(x)$? Use $\mathcal{H}$.

---

## Referências para os alunos

### Leitura obrigatória (antes do lab)
- Wilmoth & Horiuchi (1999). *Demography* 36(4): 475–495.
- Vaupel, Zhang & van Raalte (2011). *BMJ Open* 1(1): e000128.
- Aburto et al. (2019). *Demographic Research* 41: 83–102.

### Leitura complementar
- Shkolnikov, Andreev & Begun (2003). *Demographic Research* 8(11).
- van Raalte, Sasson & Martikainen (2018). *Science* 362: 1002–1004.
- Permanyer & Scholl (2019). *PLoS ONE*.
- Permanyer, Sasson & Villavicencio (2023). *JRSSA* 186(2): 217–240.
- Canudas-Romo (2010). *Demographic Research* 22: 421–438.
