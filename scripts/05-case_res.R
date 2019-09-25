
library(tidyverse)
library(readxl)

base_jul <- read_excel("dados/pe_julho2019-simulado.xlsx")
base_ago <- read_excel("dados/pe_agosto2019-simulado.xlsx")

base_completa <- bind_rows(base_jul, base_ago) %>% 
  janitor::clean_names()
  
# Tabelas -----------------------------------------------------------------

# Tabela 1

base_completa %>% 
  mutate(mes = lubridate::month(dt_ref, label = TRUE)) %>%
  group_by(modelo_stg_ctb, mes, stage_ifrs9_ctb) %>% 
  summarise(
    `PE média` = mean(pe_final_lel_ctb),
    `Desvio-padrão PE` = sd(pe_final_lel_ctb),
    `PE mínima` = min(pe_final_lel_ctb),
    `PE máxima` = max(pe_final_lel_ctb)
  ) %>% 
  ungroup() %>% 
  rename(
    Modelo = modelo_stg_ctb,
    Mês = mes,
    Stage = stage_ifrs9_ctb
  ) %>%
  mutate_if(is.numeric, ~round(.x, 2)) %>% 
  writexl::write_xlsx("scripts/tabelas/tabela_1.xlsx")


# Tabela 2

base_completa %>%
  select(
    contrato,
    modelo_stg_ctb,
    stage_ifrs9_ctb,
    ead_ctb,
    dias_atraso_ctb,
    pe_final_lel_ctb
  ) %>% 
  group_by(contrato) %>% 
  summarise(
    modelo = first(modelo_stg_ctb),
    stage = paste(stage_ifrs9_ctb, collapse = " -> "),
    EAD = mean(ead_ctb),
    atraso = paste(dias_atraso_ctb, collapse = " -> "),
    PE = mean(pe_final_lel_ctb)
  ) %>% 
  arrange(desc(PE)) %>% 
  writexl::write_xlsx("scripts/tabelas/tabela_2.xlsx")

# Tabela 3

base_completa %>% 
  filter(modelo_stg_ctb == "CAR") %>% 
  mutate(mes = lubridate::month(dt_ref, label = TRUE)) %>%
  select(mes, contrato, stage_ifrs9_ctb) %>% 
  spread(mes, stage_ifrs9_ctb) %>% 
  count(Jul, Aug) %>% 
  spread(Aug, n) %>% 
  mutate_all(~ifelse(is.na(.x), 0, .x)) %>% 
  rename(`Stage ant/Stage atual` = Jul) %>% 
  writexl::write_xlsx("scripts/tabelas/tabela_3.xlsx")

  
  
  
  