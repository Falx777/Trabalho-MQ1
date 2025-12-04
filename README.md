# 📊 Análise Exploratória das Tarifas Comerciais dos EUA sobre as Exportações Brasileiras

**Uma Análise de Série Interrompida (ITS) e Modelos ARIMA (2015–2025)**

Este repositório contém o código em R utilizado para analisar o impacto das tarifas comerciais impostas pelos Estados Unidos sobre produtos brasileiros. A análise combina:

* Construção de séries históricas (2015–2025)
* Análise exploratória
* Estatísticas descritivas
* Seleção dos Top 10 países e produtos exportados
* Modelagem ARIMA e avaliação de resíduos
* Séries contrafactuais via Regressão OLS (Interrupted Time Series - ITS)
* Matrizes de correlação e visualizações

---

## 🗂️ Estrutura do Projeto

```
├── planilhas/
│   ├── paises/
│   │   ├── estatisticas/
│   │   ├── its paises/
│   │   ├── residuos/
│   ├── produtos/
│   │   ├── estatisticas/
│   │   ├── its produtos/
│   │   ├── residuos/
├── scripts/
│   ├── analise_completa.R
├── README.md
```

---

## 📥 1. Preparação dos Dados

### **Baixe os arquivos de exportações do Brasil (2015–2025):**

**Base completa por Município e SH4**
➡️ [https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta](https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta)

Salve todas as planilhas dentro do diretório configurado no script.

### **Pastas necessárias (na mesma raiz do script):**

```
planilhas/paises/estatisticas
planilhas/paises/its paises
planilhas/paises/residuos
planilhas/produtos/estatisticas
planilhas/produtos/its produtos
planilhas/produtos/residuos
```

---

## ⚙️ 2. Configuração do Diretório no Script

No início do arquivo R ajuste:

```r
meu.diretorio <- "SEU/CAMINHO/AQUI"
setwd("SEU/CAMINHO/AQUI")
```

---

## 📦 3. Pacotes Necessários

O script utiliza os seguintes pacotes:

```r
dplyr
psych
lubridate
forecast
tseries
modelsummary
ipeadatar
FinTS
rugarch
pheatmap
tidyr
tibble
```

Instale-os com:

```r
install.packages(c(
  "dplyr","psych","lubridate","forecast","tseries",
  "modelsummary","ipeadatar","FinTS","rugarch",
  "pheatmap","tidyr","tibble"
))
```

---

## 🔍 4. Etapas da Análise

### **4.1. Importação e Consolidação dos Dados**

* Agrega exportações por **país**, **produto (SH4)**, **mês** e **ano**.
* Gera estatísticas descritivas completas.

### **4.2. Identificação dos Top 10**

Função `top.10(tipo, df)`:

* Seleciona os 10 maiores países/produtos exportados.
* Cria tabela de composição relativa (%).
* Gera gráfico de pizza com `pizza(tipo, top.10.itens)`.

### **4.3. Construção das Séries Temporais (2015–2025)**

Função `montar.serie()`:

* Lê todos os CSVs anuais.
* Monta séries mensais para cada país/produto.
* Salva arquivos consolidados.

### **4.4. Gráficos Temporais**

A função `series.e.graficos()`:

* Plota exportações mensais para cada item.
* Marca o ponto de intervenção (**julho/2025**) no gráfico.

### **4.5. Modelagem ARIMA**

Função `gerar.modelos.arima()`:

* Estima modelos ARIMA antes da intervenção.
* Previsões de 4 meses além da quebra.
* Salva modelos e gráficos automáticos.

### **4.6. Diagnóstico de Resíduos**

Função `graficos.residuos()`:

* Teste de Ljung-Box
* Teste ARCH
* ACF e PACF
* Salvamento automático dos gráficos

### **4.7. Modelos OLS (ITS)**

Função `ols.graficos()`:

* Cria modelos do tipo:

  ```
  y_t = β0 + β1*t + β2*D + β3*T + β4*Câmbio + efeitos mensais
  ```

* Gera gráficos com:

  * Observado
  * Ajustado
  * Contrafactual
  * Linha de intervenção

### **4.8. Estatísticas Descritivas dos Itens**

Função `gerar.estatisticas.descritivas()`:

* Calcula média, mediana, desvio padrão, amplitude, assimetria etc.

### **4.9. Matriz de Correlação**

* Correlação entre todos os produtos do Top 10.
* Heatmap via `pheatmap`.

---

## 📊 Exemplos de Saídas Geradas

O script produz automaticamente:

* Gráficos de pizza
* Séries temporais individuais
* Gráficos ARIMA (previsão vs. observado)
* Gráficos ITS com contrafactual
* Análise de resíduos
* Matrizes de correlação
* Tabelas formatadas dos modelos

Os arquivos são salvos nas pastas de **paises/** e **produtos/**.

---

## 📌 5. Pontos de Atenção

* O script exige **grande quantidade de memória** (muitos CSVs grandes).
* O processo de modelagem (ARIMA) pode ser lento — por isso existe opção de carregar modelos salvos via `.rds`.
* O ponto de intervenção está definido como:

  ```
  2025-07-01
  ```

---

## 🧑‍💻 Autor

Este repositório faz parte de um estudo acadêmico sobre o impacto comercial de políticas tarifárias utilizando metodologias modernas de séries temporais.

---
