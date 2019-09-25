
library(tidyverse)
library(ggridges)
library(readxl)

base_jul <- read_excel("dados/pe_julho2019-simulado.xlsx")
base_ago <- read_excel("dados/pe_agosto2019-simulado.xlsx")

base_completa <- bind_rows(base_jul, base_ago) %>% 
  janitor::clean_names()

# Tabelas -----------------------------------------------------------------

# Tabela 1
# Medidas descritivas da perda esperada


# Tabela 2
# Resumo descritivo das variáveis Stage, EAD, dias de atraso e PE

# Tabela 3
# Matriz de transição da variável Stage


# Gráfico 1
# Gráfico de barras da variável Stage por mês para carros

# Gráfico 2
# Boxplot da variável dias de atraso por mês e modelo de contrato

# Gráfico 3.1
# Distribuição da variável dias de atraso por modelo de contrato, stage e mês


# Gráfico 3.2
# Distribuição da variável dias de atraso por modelo de contrato, stage e mês
