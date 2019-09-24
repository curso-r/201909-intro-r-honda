# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(magrittr)

magrittr::divide_by(2)

x <- 1:10

x/2
x %>% divide_by(2)

# Base de dados -----------------------------------------------------------

imdb <- read_rds("dados/imdb.rds")

# select ------------------------------------------------------------------

# exemplo 1

select(imdb, titulo, ano, orcamento)

# exemplo 2 

select(imdb, starts_with("ator"))

# exemplo 3

select(imdb, -starts_with("ator"), -titulo)

# Exercício 1
# Crie uma tabela com apenas as colunas titulo, diretor, e orcamento. Salve em um
# objeto chamado imdb_simples.

# Exercício 2
# Remova as colunas ator_1, ator_2 e ator_3 de três formas diferentes. Salve em um
# objeto chamado imdb_sem_ator.

# arrange -----------------------------------------------------------------

# exemplo 1

arrange(imdb, orcamento)

# exemplo 2

arrange(imdb, desc(orcamento))

# exemplo 3

arrange(imdb, desc(ano), titulo)

# exercício 1
# Ordene os filmes em ordem crescente de ano e decrescente de lucro e salve 
# em um objeto chamado filmes_ordenados

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))
arrange(df, x)

# exemplo 5

imdb %>% filter(ano == 2010) %>% arrange(desc(orcamento))

# Exercício 2 
# Selecione apenas as colunas título e orçamento 
# e então ordene de forma decrescente pelo orçamento.



# Pipe (%>%) --------------------------------------------------------------

# g(f(x)) = x %>% f() %>% g()

# Receita de bolo sem pipe. Tente entender o que é preciso fazer.

esfrie(
  asse(
    coloque(
      bata(
        acrescente(
          recipiente(
            rep(
              "farinha", 
              2
            ), 
            "água", "fermento", "leite", "óleo"
          ), 
          "farinha", até = "macio"
        ), 
        duração = "3min"
      ), 
      lugar = "forma", tipo = "grande", untada = TRUE
    ), 
    duração = "50min"
  ), 
  "geladeira", "20min"
)

# Veja como o código acima pode ser reescrito utilizando-se o pipe. 
# Agora realmente se parece com uma receita de bolo.

recipiente(rep("farinha", 2), "água", "fermento", "leite", "óleo") %>%
  acrescente("farinha", até = "macio") %>%
  bata(duração = "3min") %>%
  coloque(lugar = "forma", tipo = "grande", untada = TRUE) %>%
  asse(duração = "50min") %>%
  esfrie("geladeira", "20min")

# ATALHO: CTRL + SHIFT + M

# Exercício
# Refaça o exercício 2 do arrange utilizando o %>% 


# filter ------------------------------------------------------------------

# exemplo 1
imdb %>% filter(nota_imdb > 9) %>% View

# exemplo 2
filmes_bons <- imdb %>% filter(nota_imdb > 9)
filmes_bons

# exemplo 3
filmes_bons <- filmes_bons %>% filter(orcamento < 1000000)
filmes_bons

imdb %>% filter(nota_imdb > 9 & orcamento < 1000000)

# exercício 1
# Criar uma variável chamada `filmes_baratos` 
# com filmes com orçamento menor do 
# que 1 milhão de dólares e ordenado pelo lucro
# de forma decrescente.

filmes_baratos <- imdb %>% 
  filter(orcamento < 1e6) %>% 
  arrange(desc(receita - orcamento)) %>% 
  view()

View(filmes_baratos)




# exemplo 5
imdb %>% filter(ano > 2010 & nota_imdb > 8.5)
imdb %>% filter(orcamento < 100000 & receita > 1000000)

imdb %>% filter(receita > orcamento)
imdb %>% filter(receita > orcamento + 500000000)
imdb %>% filter((receita > orcamento + 500000000) | nota_imdb > 9)

imdb %>% filter(ano > 2010)

imdb %>% filter(!ano > 2010)
imdb %>% filter(ano <= 2010)

imdb %>% filter(!receita > orcamento)

# exercício 2
# Criar um objeto chamado bons_baratos 
# com filmes que tiveram nota no imdb 
# maior do que 8.5 e um orcamento menor 
# do que 1 milhão de dólares.

bons_baratos <- imdb %>% 
  filter(nota_imdb > 8.5, orcamento < 1e6)

# exercício 3
# Criar um objeto chamado curtos_legais 
# com filmes de até 1h30 e nota no imdb
# maior do que 8.5.

curtos_legais <- imdb %>% 
  filter(duracao <= 90, nota_imdb > 8.5)

# exercício 4
# Criar um objeto antigo_colorido com filmes 
# de antes de 1940 que são 
# coloridos. Crie também um objeto antigo_bw 
# com filmes antigos que não são coloridos.

table(imdb$cor, useNA = "i")
unique(imdb$cor)
imdb %>% distinct(cor)

antigo_colorido <- imdb %>% 
  filter(ano < 1940 & cor == "Color")

# exercício 5
# Criar um objeto ww com filmes dos diretores Wes Anderson ou 
# do Woody Allen.

ww <- imdb %>% 
  filter(diretor == "Wes Anderson" | diretor == "Woody Allen") %>% 
  view()

# Exercício 6
# Crie uma tabela apenas com filmes do Woody Allen 
# e apenas as colunas titulo e ano,
# ordenada por ano.

imdb %>% 
  filter(diretor == "Woody Allen") %>% 
  select(titulo, ano) %>% 
  arrange(ano) %>% 
  View()


# exemplo 6
# %in%

pitts <- imdb %>% 
  filter(ator_1 %in% c('Angelina Jolie Pitt', "Brad Pitt"))

# exercicio 6
# Refaça o exercício 5 usando o %in%.

ww2 <- imdb %>% 
  filter(ano %in% 2000:2010) %>% 
  view()

# exemplo 7
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

imdb %>% filter(is.na(orcamento)) %>% View

imdb %>% 
  filter_at(vars(orcamento, receita, ano), ~is.na(.x)) %>% 
  View

# exercício 7
# Identifique os filmes que não possuem informação tanto de receita quanto de orcamento
# e salve em um objeto com nome sem_info.

sem_info <- imdb %>% 
  filter_at(vars(orcamento, receita), ~ is.na(.x)) %>% 
  view()

imdb %>%
  filter(is.na(orcamento), is.na(receita))

imdb %>% 
  filter(orcamento > 1e6)

# exemplo 8
# str_detect

imdb %>% filter(generos == "Action") %>% View

str_detect(letters, "a")

imdb %>% filter(str_detect(generos, "Action")) %>% View

# exercício 8
# Salve em um objeto os filmes de Ação e 
# Comédia com nota no imdb maior do que 8.

imdb %>% 
  filter(
    str_detect(generos, "Action"),
    str_detect(generos, "Comedy"),
    nota_imdb > 8
  ) %>% View

imdb %>% 
  filter(
    str_detect(generos, "Action.*Comedy.*"),
    nota_imdb > 8
  ) %>% 
  View

# mutate ------------------------------------------------------------------

# exemplo 1

imdb %>% mutate(duracao = duracao/60) %>% View()

# exemplo 2

imdb %>% mutate(duracao_horas = duracao/60) %>% 
  select(duracao_horas, everything())

# exercício 1
# Crie uma variável chamada lucro. Salve em um objeto chamado imdb_lucro.

imdb_lucro <- imdb %>% mutate(lucro = receita - orcamento)

# exercicio 2
# Modifique a variável lucro para ficar na escala de milhões de dólares.

imdb_lucro <- imdb_lucro %>% 
  mutate(lucro = lucro/1e6) %>% View

imdb %>% 
  mutate(
    lucro = receita - orcamento,
    lucro = lucro/1e6,
    prejuizo = orcramento - receita
  )

# exercício 3
# Filtre apenas os filmes com prejuízo maior do que 3 milhões de dólares. 
# Deixe essa tabela ordenada com o maior prejuízo primeiro. Salve o resultado em 
# um objeto chamado filmes_prejuizo.


filmes_prejuizo <- imdb_lucro %>% 
  filter(lucro < -3) %>% 
  arrange(lucro)

imdb %>% 
  mutate(
    lucro = receita - orcamento,
    lucro = lucro/1e6
  ) %>% 
  filter(lucro < -3) %>% 
  arrange(lucro)
  

View(filmes_prejuizo)

# exemplo 3
# gêneros

# install.packages("gender")
library(gender)

install.packages("genderdata", repos = "http://packages.ropensci.org", type = "source")

gender(c("William"), years = 2012)
gender(c("Robin"), years = 2012)

gender(c("Madison", "Hillary"), years = 1930, method = "ssa")
gender(c("Madison", "Hillary"), years = 2010, method = "ssa")

gender("Matheus", years = 1920)

obter_genero <- function(nome, ano) {
  
  if (is.na(nome) | is.na(ano)) {
    return(NA_character_)
  }
  
  ano_min <- ano - 60
  ano_max <- ano - 30
  
  if (ano_min < 1880) {
    ano_min <- 1880
  }
  
  genero <- gender(nome, years = c(ano_min, ano_max), method = "ssa")$gender
  
  if(length(genero) == 0) {
    genero <- NA_character_
  }
  
  genero
}

obter_genero("Madison", 1930)
obter_genero("Matheus", 1930)

# demora +- 10 min.
imdb_generos <- imdb %>%
  select(diretor, ano) %>%
  distinct() %>%
  mutate(
    diretor_primeiro_nome = str_extract(diretor, ".* ") %>% str_trim(),
    genero = map2_chr(diretor_primeiro_nome, ano, obter_genero)
  )

map2_chr(vetor1, vetor2, funcao)

# saveRDS(imdb_generos, "data/imdb_generos.rds")
imdb_generos <- read_rds("dados/imdb_generos.rds")

# https://github.com/meirelesff/genderBR

# summarise ---------------------------------------------------------------

# exemplo 1

imdb %>% summarise(media_orcamento = mean(orcamento, na.rm=TRUE))

# exemplo 2

imdb %>% summarise(
  media_orcamento = mean(orcamento, na.rm=TRUE),
  mediana_orcamento = median(orcamento, na.rm = TRUE)
)

# exemplo 3

imdb %>% summarise(
  media_orcamento = mean(orcamento, na.rm=TRUE),
  mediana_orcamento = median(orcamento, na.rm = TRUE),
  qtd = n(),
  diretores_faltantes = sum(is.na(diretor)),
  qtd_diretores = n_distinct(diretor)
)

# exemplo 4

imdb_generos %>%
  summarise(n_diretora = sum(genero == "female", na.rm = TRUE))

# exercício 1
# Use o `summarise` para calcular a proporção de filmes com diretoras.
# DICA: proporcao -> mean(vetor_logico)
# DICA: contagem  -> sum(vetor_logico)
imdb_generos %>%
  summarise(
    prop_diretoras = sum(genero == "female", na.rm = TRUE)/sum(!is.na(genero))
  )

# exercício 2
# Calcule a duração média e mediana dos filmes da base.
imdb %>%
  summarise(
    duracao_media = mean(duracao, na.rm = TRUE),
    duracao_mediana = median(duracao, na.rm = TRUE)
  )

# exercício 3
# Calcule o lucro médio dos filmes com duracao < 60 minutos. 
# E o lucro médio dos filmes com mais de 2 horas.
imdb_lucro %>%
  filter(duracao < 60) %>%
  summarise(
    lucro_medio = mean(lucro, na.rm = TRUE)
  )

imdb_lucro %>%
  filter(duracao > 120) %>%
  summarise(
    lucro_medio = mean(lucro, na.rm = TRUE)
  )

imdb_lucro %>%
  summarise(
    lucro_medio_curta_metragem = mean(lucro[duracao < 60], na.rm = TRUE),
    lucro_medio_longa_metragem = mean(lucro[duracao > 120], na.rm = TRUE)
  )

# group_by + summarise ----------------------------------------------------

# exemplo 1

imdb %>% group_by(ano)

# exemplo 2

imdb %>% 
  group_by(ano) %>%
  summarise(
    qtd_filmes = n(),
    sumario1 = ...,
    sumario2 = ...,
    sumario3 = ...,
    sumario4 = ...,
  )

# "atalho" para fazer tabela de contagem
imdb %>%
  count(ano)

# exemplo 3

imdb %>% 
  group_by(diretor, ano) %>% 
  summarise(qtd_filmes = n())

# exercício 1
# Crie uma tabela com apenas o nome dos diretores com mais de 10 filmes.
imdb %>%
  group_by(diretor) %>%
  summarise(
    qtd_filmes = n()
  ) %>%
  filter(
    qtd_filmes > 10
  )

imdb %>%
  count(diretor) %>%
  filter(
    n > 10,
    !is.na(diretor)
  ) %>%
  print(n = 40)


# exercício 2
# Crie uma tabela com a receita média e mediana dos filmes por ano.
sumario_da_maior_receita <- imdb %>%
  filter(!is.na(receita)) %>%
  group_by(ano) %>%
  summarise(
    receita_media = mean(receita), 
    receita_mediana = median(receita, na.rm = TRUE),
    receita_maxima = max(receita, na.rm = TRUE)
  ) %>%
  arrange(receita_media) %>%
  filter(
    receita_media == max(receita_media)
  )

# exercício 3
# Crie uma tabela com a nota média do imdb dos filmes por tipo de classificacao.
a <- imdb %>%
  group_by(classificacao) %>%
  summarise(
    nota_media = mean(nota_imdb, na.rm = TRUE)
  )


# exemplo 4

imdb %>%
  filter(str_detect(generos, "Action"), !is.na(diretor)) %>%
  group_by(diretor) %>%
  summarise(qtd_filmes = n()) %>%
  arrange(desc(qtd_filmes))

# exemplo 5

imdb %>% 
  filter(ator_1 %in% c("Brad Pitt", "Angelina Jolie Pitt")) %>%
  group_by(ator_1) %>%
  summarise(orcamento = mean(orcamento), receita = mean(receita), qtd = n())

# left join ---------------------------------------------------------------
# exemplo 1

imdb_generos2 <- imdb %>%
  left_join(
    imdb_generos, 
    by = c("diretor", "ano")
  )

# exemplo 2

depara_cores <- tibble(
  cor = c("Color", "Black and White"),
  cor2 = c("colorido", "pretoEbranco")
)

imdb_cor <- imdb %>% 
  left_join(
    depara_cores %>% select(cor, cor2), 
    by = c("cor")
  )

dplyr::join

# exemplo 3

imdb_generos3 <- imdb %>%
  left_join(
    imdb_generos, 
    by = c("diretor", "ano")
  )

# exercicio 1
# Calcule a média dos orçamentos e receitas para filmes feitos por
# genero do diretor.
imdb_generos3 <- imdb %>%
  left_join(
    imdb_generos, 
    by = c("diretor", "ano")
  ) %>%
  group_by(genero) %>%
  summarise(
    orcamento_medio = mean(orcamento, na.rm = TRUE),
    receita_media = mean(receita, na.rm = TRUE)
  )

# gather ------------------------------------------------------------------
# exemplo 1

imdb_gather <- imdb %>% gather("importancia_ator", "nome_ator", starts_with("ator"))

# spread ------------------------------------------------------------------

# exemplo 1

imdb <- spread(imdb_gather, importancia_ator, nome_ator)

