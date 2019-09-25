
library(tidyverse)
library(ggridges)
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
  mutate_if(is.numeric, ~round(.x, 2))


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
  arrange(desc(PE))

# Tabela 3

base_completa %>% 
  filter(modelo_stg_ctb == "CAR") %>% 
  mutate(mes = lubridate::month(dt_ref, label = TRUE)) %>%
  select(mes, contrato, stage_ifrs9_ctb) %>% 
  spread(mes, stage_ifrs9_ctb) %>% 
  count(Jul, Aug) %>% 
  spread(Aug, n) %>% 
  mutate_all(~ifelse(is.na(.x), 0, .x)) %>% 
  rename(`Stage ant/Stage atual` = Jul)


# Gráfico 1

base_completa %>% 
  filter(modelo_stg_ctb == "CAR") %>% 
  mutate(
    mes = lubridate::month(dt_ref, label = TRUE, locale = "pt_BR.UTF-8"),
    stage_ifrs9_ctb = as.character(stage_ifrs9_ctb)
  ) %>%
  count(stage_ifrs9_ctb, mes) %>% 
  ggplot(aes(x = stage_ifrs9_ctb, fill = stage_ifrs9_ctb, y = n)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), nudge_y = 10) +
  facet_wrap(vars(mes)) +
  labs(x = "Stage", y = "Número de contratos") +
  theme_minimal()

# Gráfico 2

base_completa %>% 
  mutate(
    mes = lubridate::month(dt_ref, label = TRUE, locale = "pt_BR.UTF-8")
  ) %>%
  ggplot(aes(x = modelo_stg_ctb, y = dias_atraso_ctb)) +
  geom_boxplot() +
  facet_wrap(vars(mes)) +
  labs(x = "Modelo", y = "Dias de atraso") +
  theme_minimal()

# Gráfico 3.1

base_completa %>% 
  mutate(
    mes = lubridate::month(dt_ref, label = TRUE, locale = "pt_BR.UTF-8"),
    stage_ifrs9_ctb = as.character(stage_ifrs9_ctb)
  ) %>%
  ggplot(aes(x = pe_final_lel_ctb, y = stage_ifrs9_ctb, fill = stage_ifrs9_ctb)) +
  geom_density_ridges(show.legend = FALSE) +
  facet_grid(rows = vars(mes), col = vars(modelo_stg_ctb), scales = "free") +
  labs(x = "Perda esperada", y = "Stage") +
  theme_minimal()
ggsave("scripts/graficos/grafico_3_1.png")

# Gráfico 3.2

base_completa %>% 
  mutate(
    mes = lubridate::month(dt_ref, label = TRUE, locale = "pt_BR.UTF-8"),
    stage_ifrs9_ctb = as.character(stage_ifrs9_ctb)
  ) %>%
  ggplot(aes(x = dias_atraso_ctb, y = stage_ifrs9_ctb, fill = stage_ifrs9_ctb)) +
  geom_density_ridges(show.legend = FALSE) +
  facet_grid(rows = vars(mes), col = vars(modelo_stg_ctb), scales = "free") +
  labs(x = "Perda esperada", y = "Stage") +
  theme_minimal()
  