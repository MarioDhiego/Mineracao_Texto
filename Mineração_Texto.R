

library(literaturaBR) # meio obvio
library(tidytext) # excelente pacote de text mining
library(tidyverse) # <3
library(stringr) # indispensavel para manipulacao de texto
library(quanteda) # otimas funcoes para analise quantitativa de texto
library(qdap) # similar ao quanteda, embo ra eu nao me lembre exatamente se eu o uso neste post
library(forcats) # manipulacao de fatores
library(ggthemes) # temas para o ggplot2
library(lexiconPT)


data("memorias_de_um_sargento_de_milicias")
data("memorias_postumas_bras_cubas")
data("alienista")
data("escrava_isaura")
data("ateneu")



df <- bind_rows(memorias_de_um_sargento_de_milicias,
                memorias_postumas_bras_cubas,
                alienista,
                escrava_isaura,
                ateneu)


# Olhando a estrutura do dataframe
glimpse(df)


df_corpus <- df %>% 
  # agrupar por livro
  group_by(book_name) %>% 
  # formatar o dataframe para que so tenha uma linha por livro
  summarise(text = paste0(text, sep = "", collapse = ". "))

dim(df_corpus)


meu_corpus <- quanteda::corpus(df_corpus$text, docnames = df_corpus$book_name)
summary(meu_corpus)


corpus_dfm <- dfm(meu_corpus, remove_punct = TRUE,
                  remove = quanteda::stopwords("portuguese"),
                  groups = df_corpus$book_name)

# Analisando as 15 palavras mais comuns no geral por livro
dfm_sort(corpus_dfm)[, 1:15]

# ocorrencias da palavra amor
dfm_select(corpus_dfm, "amor")



# usar a função head() para o output nao ficar mt grande
kwic(meu_corpus, "amor") %>% head()

topfeatures(corpus_dfm, groups = df_corpus$book_name)


# normalizar os livros pelo seu tamanho
corpus_dfm_norm <- dfm_weight(corpus_dfm, "relfreq")
corpus_simil <- textstat_simil(corpus_dfm_norm, method = "correlation",
                               margin = "documents", upper = TRUE,
                               diag = FALSE)
# ver os resultados individualmente para cada livro
round(corpus_simil, 3)



corpus_dist <- textstat_dist(corpus_dfm_norm, method = "euclidean",
                             margin = "documents", upper = TRUE,
                             diag = FALSE)
# ver os resultados individualmente para cada livro
plot(hclust(corpus_dist))



# Criar um dataframe em que cada linha corresponda a uma unica palavra
df.token <- df %>%
  unnest_tokens(term, text)

glimpse(df.token)


# importar lexico de sentimentos
data("oplexicon_v3.0")
df.token <- df.token %>%
  inner_join(oplexicon_v3.0, by = "term")



# extrair capitulos de cada livro
df_chapter_number <- df.token %>%
  distinct(book_name, chapter_name) %>%
  group_by(book_name) %>%
  # normalizar capitulo de acordo com sua posicao no livro
  mutate(chapter_number_norm = row_number()/max(row_number()))

glimpse(df_chapter_number)




df.sentiment <- df.token %>%
  # calcular sentimento por capitulo
  group_by(book_name, chapter_name) %>%
  summarise(polarity = sum(polarity, na.rm = TRUE)) %>%
  ungroup() %>%
  # retornar posicao relativa (ou normalizada) do capitulo de cada livro
  left_join(df_chapter_number) %>%
  arrange(book_name, chapter_number_norm)

# grafico
df.sentiment %>%
  ggplot(aes(x = chapter_number_norm, y = polarity)) +
  geom_line() +
  facet_wrap(~ book_name, ncol = 5, labeller = label_wrap_gen(20)) +
  labs(x = "Posição relativa no livro", y = "Sentimento") +
  theme_bw()



# aplicando a funcao no objeto sem stopwords e pontuação
lexdiv <- textstat_lexdiv(corpus_dfm, measure = "TTR")
lexdiv

#grafico
lexdiv %>% 
  as.data.frame() %>% 
  magrittr::set_colnames("TTR") %>% 
  tibble::rownames_to_column("livro") %>% 
  mutate(livro = forcats::fct_reorder(livro, TTR)) %>% 
  ggplot(aes(x = livro, y = TTR)) + 
  geom_col(fill = "cadetblue4") +
  coord_flip() + 
  labs(x = NULL, y = "TTR") +
  theme_minimal()


kwic(meu_corpus, "amor") %>% textplot_xray(scale = "relative")

kwic(meu_corpus, "fogo") %>% textplot_xray(scale = "relative")



















































