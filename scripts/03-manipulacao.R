# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(dplyr)

# Base de dados -----------------------------------------------------------

imdb <- read_rds("dados/imdb.rds")

# select ------------------------------------------------------------------

# exemplo 1

imdb_select <- select(imdb, diretor, everything())

select(imdb, titulo, ano, orcamento)

# exemplo 2 

select(imdb, starts_with("ator"))

# exemplo 3

select(imdb, -starts_with("ator"), -titulo)


# Renomear
select(imdb, Diretor = diretor)
rename(imdb, Diretor = diretor)

# Exercício 1
# Crie uma tabela com apenas as colunas titulo, diretor, e orcamento. Salve em um
# objeto chamado imdb_simples.
imdb_simples <- select(
  imdb, 
  titulo, 
  diretor, 
  orcamento
)
imdb_simples
view(imdb_simples)

# Exercício 2
# Remova as colunas ator_1, ator_2 e ator_3 de três formas diferentes. Salve em um
# objeto chamado imdb_sem_ator.
imdb_sem_ator <- select(
  imdb,
  -ator_1,
  -ator_2,
  -ator_3
)

imdb_sem_ator <- select(imdb, -starts_with("ator"))
  
imdb_sem_ator <- select(imdb, titulo:likes_facebook)

imdb_sem_ator <- select(imdb, -matches("[0-9]$"))

imdb_sem_ator <- select(imdb, -num_range("ator_", 1:3))

# arrange -----------------------------------------------------------------

# exemplo 1

imdb_ord <- arrange(imdb, orcamento, receita)
View(imdb_ord)

# exemplo 2

imdb_ord <- arrange(imdb, desc(orcamento))
View(imdb_ord)

# exemplo 3

imdb_ord <- arrange(imdb, desc(ano), titulo)
View(imdb_ord)




imdb_ord <- arrange(imdb, desc(receita - orcamento))
View(imdb_ord)

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))
arrange(df, desc(x))

# exercício 1
# Ordene os filmes em ordem crescente de ano e decrescente de lucro e salve 
# em um objeto chamado filmes_ordenados
filmes_ordenados <- arrange(imdb, ano, desc(receita - orcamento))
View(filmes_ordenados)


# Exercício 2 
# Selecione apenas as colunas título e orçamento 
# e então ordene de forma decrescente pelo orçamento.

filmes_ordenados <- select(imdb, titulo, orcamento)
filmes_ordenados <- arrange(filmes_ordenados, desc(orcamento))

filmes_ordenados <- arrange(select(imdb, titulo, orcamento), desc(orcamento))


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

imdb %>% 
  select(titulo, orcamento) %>% 
  arrange(desc(orcamento))

# Comparações lógicas ------------------------------------------------------

1 > 0
2 < 1
3 == 3
3 != 1
5 %in% c(2, 4, 5)

"a" < "c"

"a1" %in% letters


# Valores especiais -------------------------------------------------------

# Existem valores reservados para representar dados faltantes, 
# infinitos, e indefinições matemáticas.

NA   # (Not Available) significa dado faltante/indisponível. 

NaN  # (Not a Number) representa indefinições matemáticas, como 0/0 e log(-1). 
# Um NaN é um NA, mas a recíproca não é verdadeira.

Inf  # (Infinito) é um número muito grande ou o limite matemático, por exemplo, 
# 1/0 e 10^310. Aceita sinal negativo -Inf.

NULL # representa a ausência de informação.

# Use as funções is.na(), is.nan(), is.infinite() e is.null() 
# para testar se um objeto é um desses valores.

a <- NA
a == NA

is.na(NaN)
is.null()
is.nan(NA)

# Seja x a idade de Maria. Não sabemos a idade de Maria:
x <- NA

# Seja y a idade de João. Não sabemos a idade de João:
y <- NA

# Maria e João têm a mesma idade?
x == y

# Não sabemos.

safe_f <- purrr::possibly(f, otherwise = NA)

safe_f()

# filter ------------------------------------------------------------------

# exemplo 1
imdb %>% filter(nota_imdb > 9) %>% View

# exemplo 2
filmes_bons <- imdb %>% filter(nota_imdb > 9)
filmes_bons

# exemplo 3
filmes_bons <- filmes_bons %>% filter(orcamento < 1000000)
filmes_bons


# exercício 1
# Criar uma variável chamada `filmes_baratos` com filmes com orçamento menor do 
# que 1 milhão de dólares.

filmes_baratos <- imdb %>% filter(orcamento < 1e6)

& -> "e"
| -> "ou"

1 > 2 & 3 == 3

TRUE & TRUE
TRUE & FALSE

TRUE | TRUE
TRUE | FALSE

!TRUE
!FALSE
# exemplo 5
imdb %>% filter(ano > 2010, nota_imdb > 8.5)
imdb %>% filter(orcamento < 100000 & receita > 1000000) %>% View()

imdb %>% filter(receita > orcamento)
imdb %>% filter(receita > orcamento + 500000000)
imdb %>% filter(receita > orcamento + 500000000 | nota_imdb > 9)

imdb %>% filter(!is.na(receita))



imdb %>% filter(ano > 2010)
imdb %>% filter(!ano > 2010)
imdb %>% filter(!(receita > orcamento))

# exercício 2
# Criar um objeto chamado bons_baratos com filmes que tiveram nota no imdb 
# maior do que 8.5 e um orcamento menor do que 1 milhão de dólares.

bons_baratos <- imdb %>% filter(nota_imdb > 8.5 & orcamento < 1e6) %>% view()

# exercício 3
# Criar um objeto chamado curtos_legais com filmes de até 1h30 e nota no imdb
# maior do que 8.5.

curtos_legais <- imdb %>% 
  filter(duracao <= 90 & nota_imdb > 8.5) %>% view()
curtos_legais
# exercício 4
# Criar um objeto antigo_colorido com filmes de antes de 1940 que são 
# coloridos. Crie também um objeto antigo_bw com filmes antigos que não são coloridos.

antigo_colorido <- imdb %>% 
  filter(ano < 1940, cor == "Color")

antigo_bw <- imdb %>% 
  filter(ano < 1940, cor == "Black and White")

antigo_bw <- imdb %>% 
  filter(ano < 1940, !cor == "Color")

antigo_bw <- imdb %>% 
  filter(ano < 1940, is.na(cor))

distinct(imdb, cor)
count(imdb, cor, name = "qtd")

table(imdb$cor, useNA = "always")



base_exemplo <- tibble(água = 1, `variavel com espaco` = 2, AAAA = 3)

janitor::clean_names(base_exemplo) %>% View()

base <- read_csv("arquivo") %>% janitor::clean_names()

# exercício 5
# Criar um objeto ww com filmes do Wes Anderson ou do Woody Allen.
# Manter nessa tabela apenas as colunas título, diretor e receita. 
# Ordene a tabela pela receita.

ww <- imdb %>% 
  filter(diretor == "Wes Anderson" | diretor == "Woody Allen") %>%
  select(titulo, receita) %>% 
  arrange(receita) %>% view()
  

imdb %>% 
  filter(diretor == "Wes Anderson" | diretor == "Woody Allen") %>% 
  select()

# exemplo 6
# %in%

pitts <- imdb %>% 
  filter(ator_1 %in% c('Angelina Jolie Pitt', "Brad Pitt")) %>% view

# exercicio 6
# Refaça o exercício 5 usando o %in%.

# exemplo 7
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

imdb %>% filter(is.na(orcamento))

# exercício 7
# Identifique os filmes que não possuem informação tanto de receita quanto de orcamento
# e salve em um objeto com nome sem_info.

library(stringr)

diretores_w <- imdb$diretor[stringr::str_detect(imdb$diretor, "^W")]

diretores_com_w <- imdb %>% 
  filter(stringr::str_detect(diretor, "^W")) %>% 
  select(diretor)

letters[NA]

ww <- imdb %>% 
  filter(diretor %in% diretores_com_w$diretor) %>%
  select(titulo, diretor, receita) %>% 
  arrange(receita) %>% view()

# exemplo 8
# str_detect

imdb %>% filter(generos == "Action")
imdb %>% filter(str_detect(generos, "Action"))

# exercício 8
# Salve em um objeto os filmes de Ação e Comédia 
# com nota no imdb maior do que 8.

stringr::str_replace_all(imdb$generos, "[|]", "-")
stringr::str_split(imdb$generos, "[|]")
stringr::str_count("agagagagag")

acao_comedia <- imdb %>% 
  filter(str_detect(generos, "Action|Comedy") &
           nota_imdb > 8) %>% view()

# mutate ------------------------------------------------------------------

# exemplo 1

imdb %>% mutate(duracao = duracao/60) %>% view

# exemplo 2

imdb %>% mutate(
  duracao_horas = duracao/60,
  duracao_horas = round(duracao_horas),
  lucro = receita - orcamento
) %>%
  select(titulo:duracao, duracao_horas, everything()) %>% view

# exercício 1
# Crie uma variável chamada lucro. Salve em um objeto chamado imdb_lucro.

imdb_lucro <- imdb %>% 
  mutate(lucro = receita - orcamento)

# exercicio 2
# Modifique a variável lucro para ficar na escala de milhões de dólares.

imdb_lucro <- imdb_lucro %>% 
  mutate(lucro = lucro/1e6) %>% view()

# exercício 3
# Filtre apenas os filmes com prejuízo maior do que 3 milhões de dólares. 
# Deixe essa tabela ordenada com o maior prejuízo primeiro. Salve o resultado em 
# um objeto chamado filmes_prejuizo.

filmes_prejuizo <- imdb_lucro %>% 
  filter(lucro < -3) %>% 
  arrange(lucro) %>% view


# exemplo 3
# sexo

# install.packages("genderBR")
library(gender)

gender(c("William"), years = c(1950,2012))
gender(c("Robin"), years = 2012)

gender(c("Madison", "Hillary"), years = 1930, method = "ssa")
gender(c("Madison", "Hillary"), years = 2010, method = "ssa")

gender("Matheus", years = 2010)

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
  qtd_diretores = n_distinct(diretor)
)

# exemplo 4

imdb_generos %>%
  summarise(n_diretora = sum(genero == "female", na.rm = TRUE))

# exercício 1
# Use o `summarise` para calcular a proporção de filmes com diretoras.

# exercício 2
# Calcule a duração média e mediana dos filmes da base.

# exercício 3
# Calcule o lucro médio dos filmes com duracao < 60 minutos. E o lucro médio dos filmes com
# mais de 2 horas.

# group_by + summarise ----------------------------------------------------

# exemplo 1

imdb %>% group_by(ano)

# exemplo 2

imdb %>% 
  group_by(ano) %>% 
  summarise(qtd_filmes = n())

# exemplo 3

imdb %>% 
  group_by(diretor) %>% 
  summarise(qtd_filmes = n())

# exercício 1
# Crie uma tabela com apenas o nome dos diretores com mais de 10 filmes.

# exercício 2
# Crie uma tabela com a receita média e mediana dos filmes por ano.

# exercício 3
# Crie uma tabela com a nota média do imdb dos filmes por tipo de classificacao.

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
  left_join(imdb_generos, by = c("titulo", "diretor", "ano"))

# exemplo 2

depara_cores <- tibble(
  cor = c("Color", "Black and White"),
  cor2 = c("colorido", "pretoEbranco")
)

imdb_cor <- left_join(imdb, depara_cores, by = c("cor"))

# exemplo 3

imdb_generos3 <- imdb %>%
  left_join(imdb_generos, by = c("diretor", "ano"))

# exercicio 1
# Calcule a média dos orçamentos e receitas para filmes feitos por
# genero do diretor.

# gather ------------------------------------------------------------------

# exemplo 1

imdb_gather <- gather(imdb, "importancia_ator", "nome_ator", starts_with("ator"))

# spread ------------------------------------------------------------------

# exemplo 1

imdb <- spread(imdb_gather, importancia_ator, nome_ator)

