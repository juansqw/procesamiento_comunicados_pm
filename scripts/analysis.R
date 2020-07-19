
# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(tm)
library(SentimentAnalysis)

# Código para crear el set de datos usando los txt,
# no hay que correrlo otra vez

comunicados_path <- list.files("data/minutas_BCRD")

comunicados <- comunicados_path %>% 
  map(
    ~read_lines(here::here("data", "comunicados", .x)) %>% 
      as.tibble() %>% 
      filter(value != "") %>% 
      mutate(date = str_extract(.x, "....."), 
             paragraph = 1:length(date)) %>% 
      separate(col = date, into = c("mes", "year"))
    ) %>% 
  bind_rows()

xlsx::write.xlsx(as.data.frame(comunicados), here::here("data", "comunicados.xlsx"),
                 row.names = FALSE)

write.csv(
  as.data.frame(comunicados), 
  here::here("data", "comunicados.csv"), 
  row.names = FALSE)

# Textmining --------------------------------------------------------------

comunicados_entorno <- readxl::read_excel(
  here::here("data", "comunicados_entorno.xlsx"), 
  sheet = "comunicados")

comunicados_entorno <- comunicados_entorno %>% 
  mutate(date = lubridate::ymd(paste(year + 2000, mes, "01", sep = "-"))) %>% 
  select(date, year, mes, value, paragraph, entorno) %>% 
  rowid_to_column(var = "document")

# tidy format
comunicados_tidy <- comunicados_entorno %>% 
  unnest_tokens(word, value) %>% 
  group_by(word) %>% 
  filter(n() > 10, str_detect(word, "[0-9]", negate = TRUE)) %>% 
  anti_join(get_stopwords()) %>% 
  ungroup()


# Visualizaciones ---------------------------------------------------------

# Bigram analysis
comunicados_bigram <- comunicados_entorno %>% 
  unnest_tokens(bigram, value, token = "ngrams", n = 2) %>% 
  select(entorno, bigram) %>% 
  separate(bigram, into = c("first", "second"), sep = " ") %>% 
  anti_join(get_stopwords(), by = c("first" = "word")) %>% 
  anti_join(get_stopwords(), by = c("second" = "word")) %>% 
  filter(str_detect(first, "[0-9]", negate = TRUE)) %>% 
  filter(str_detect(second, "[0-9]", negate = TRUE)) %>% 
  mutate(bigram = paste(first, second)) %>% 
  count(entorno, bigram, sort = TRUE) 

# gráficos del top 20
(plt_bigram_entorno <- comunicados_bigram %>% 
  filter(entorno %in% c("national", "international")) %>% 
  group_by(entorno) %>% 
  top_n(20) %>% 
  ggplot(aes(x = n, y = reorder_within(bigram, n, entorno), fill = entorno)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  facet_wrap(~entorno, scales = "free") +
  scale_y_reordered() +
  theme_light() +
  labs(y = NULL, x = "Frecuencia"))


# Número de palabras en las minutas del banco central
(plt_palablas_comunicados_smooth <- comunicados_tidy %>% 
  count(date) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point() + 
  geom_smooth() +
  theme_minimal() +
  labs(x = NULL, y = "Cantidad de palabras"))

(plt_palablas_comunicados <- comunicados_tidy %>% 
  count(date) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point() + 
  theme_minimal() +
  labs(x = NULL, y = "Cantidad de palabras"))


(plt_monogram <- comunicados_tidy %>% 
  mutate(year = year + 2000) %>% 
  count(year, word, sort = TRUE) %>% 
  group_by(year) %>% 
  filter(word != 'year') %>% 
  top_n(10) %>% 
  ggplot(aes(x = n, y = reorder_within(word, n, year))) +
  geom_col(fill = "midnightblue", alpha = 0.8) +
  scale_y_reordered() +
  facet_wrap(~year, scales = "free") +
  labs(y = "Términos frecuentes", x = 'Frecuencia') +
  theme_light())

(plt_bigram_year <- comunicados_entorno %>% 
  unnest_tokens(bigram, value, token = "ngrams", n = 2) %>% 
  select(year, bigram) %>% 
  separate(bigram, into = c("first", "second"), sep = " ") %>% 
  anti_join(get_stopwords(), by = c("first" = "word")) %>% 
  anti_join(get_stopwords(), by = c("second" = "word")) %>% 
  filter(str_detect(first, "[0-9]", negate = TRUE)) %>% 
  filter(str_detect(second, "[0-9]", negate = TRUE)) %>% 
  mutate(bigram = paste(first, second),
         year = year + 2000) %>% 
  count(year, bigram, sort = TRUE) %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  ggplot(aes(x = n, y = reorder_within(bigram, n, year))) +
  geom_col(alpha = 0.8, fill = "midnightblue") +
  facet_wrap(~year, scales = "free") +
  scale_y_reordered() +
  theme_light() +
  labs(x = "Frecuencia", y = "Bigramas más frecuentes"))

(plt_tf_idf <- comunicados_tidy %>% 
  count(year, word, sort = TRUE) %>%
  filter(!word %in% c("de", "en", "n", "used", "el",
                      "la", "o", "las", "ed", "por", 
                      "para", "del", "take", "y", "su", 
                      "que", "su", "se", "")) %>% 
  bind_tf_idf(word, year, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(year = year + 2000) %>%
  select(year, word, f = n, n = tf_idf) %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  ggplot(aes(x = f, y = reorder_within(word, n, year))) +
  geom_col(alpha = 0.8) +
  scale_y_reordered() +
  facet_wrap(~year, scales = "free") +
    theme_light() +
    labs(x = NULL, y = NULL))

# sentiment analysis ------------------------------------------------------


corpus_comunicados <- comunicados_entorno %>% 
  select(doc_id = document, text = value) %>% 
  DataframeSource() %>% 
  VCorpus(readerControl = list(language = "en"))


corpus_comunicados <-  corpus_comunicados %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stripWhitespace)) %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(content_transformer(removePunctuation)) 


comunicados_tdm <- TermDocumentMatrix(corpus_comunicados)

comunicados_sentiments <- analyzeSentiment(comunicados_tdm) %>% 
  as_tibble()

comunicados_sentiments <- comunicados_entorno %>% 
  select(document, date, entorno) %>% 
  bind_cols(comunicados_sentiments)

(plt_sentiment_entorno <-  comunicados_sentiments %>%
  filter(entorno %in% c("national", "international")) %>% 
  group_by(date, entorno) %>% 
  summarise(sentiment_he = mean(SentimentHE)) %>% 
  ggplot(aes(x = date, y = sentiment_he, color = entorno)) +
  geom_line() +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Sentiments", color = NULL))

write_csv(comunicados_sentiments, "data/comunicados_sentiments.csv")


# Visualización del indicador de incertidumbre ----------------------------
indicador <- readxl::read_excel('data/indicador.xlsx') %>% 
  mutate(Date = lubridate::ymd(Date)) %>% 
  rename(date = Date)

indicador_long <- indicador %>% 
  pivot_longer(
    cols = c(Geopolítica, Mercados, Negocios, Petróleo),
    names_to = "sectores",
    values_to = "sentiments"
  )

indicador_incertidumbre <- indicador %>% 
  select(Date, indicador_Incertidumbre)


# Elasticidades -----------------------------------------------------------

rescale <- function(x, new_min = 0.000001, new_max = 1) {
  old_range <-  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  new_range <-  (new_max - (new_min))
  new_value <-  (((x - min(x, na.rm = TRUE)) * new_range) / old_range) + (new_min)
  return(new_value)
}

# Datos encuesta EEM
eem <- read_csv("data/eem.csv")

# Elasticidades Delta expectativas / delta Sentiments

alasticidad_exp_sent <- comunicados_sentiments %>% 
  group_by(date) %>% 
  summarise(WordCount = sum(WordCount), 
            across(contains("Sentiment"), mean)) %>% 
  
  left_join(
    select(eem, date = periodo, inflacion_interanual,
           tc_interanual, pib_diciembre2, tc_interanual_v)
    ) %>%
  setNames(., str_remove(names(.), "Sentiment")) %>% 
  mutate(
    across(c(-date, -WordCount), rescale),
    across(c(-date, -WordCount), ~c(NA, diff(log(.)))),
    across(GI:LM, ~(inflacion_interanual/.), .names = '{col}_inflacion_elast'),
    across(GI:LM, ~(tc_interanual/.), .names = '{col}_tc_elast'),
    across(GI:LM, ~(pib_diciembre2/.), .names = '{col}_pib_elast')
  ) %>% select(date, contains("elast"))

xlsx::write.xlsx(as.data.frame(elasticidad_indicador_incertidumbre_expectativas),
                 "data/elasticidad_sentiments_expectativas.xlsx", 
                 row.names = FALSE, showNA = FALSE)

# Elasticidades indicador de incertidumbre y expectativas 

elasticidad_indicador_incertidumbre_expectativas <-  indicador %>% 
    left_join(
    select(eem, date = periodo, inflacion_interanual,
           tc_interanual, pib_diciembre2, tc_interanual_v)
  ) %>%
  filter(date < "2020-06-01") %>% 
  janitor::clean_names() %>% 
  mutate(
    across(-date, rescale),
    across(-date, ~c(NA, diff(log(.)))),
    across(geopolitica:indicador_incertidumbre, ~(inflacion_interanual/.), .names = '{col}_inflacion_elast'),
    across(geopolitica:indicador_incertidumbre, ~(tc_interanual/.), .names = '{col}_tc_elast'),
    across(geopolitica:indicador_incertidumbre, ~(pib_diciembre2/.), .names = '{col}_pib_elast')
  ) %>% select(date, contains("elast"))


xlsx::write.xlsx(as.data.frame(elasticidad_indicador_incertidumbre_expectativas),
                 "data/elasticidad_indicador_incertidumbre_expectativas.xlsx", 
                 row.names = FALSE, showNA = FALSE)


# visualizaciones elasticidades -------------------------------------------

# adecuando la data según sectores
sector_expectativa <- elasticidad_indicador_incertidumbre_expectativas %>% 
  pivot_longer(cols = -date, 
               names_to = c("sector", "variable_expectativa"),
               #names_sufix = '_elast',
               values_to = "elasticidad",
               names_pattern = '(geopolitica|mercados|petroleo|negocios|indicador_incertidumbre)_(.*$)') %>% 
  mutate(variable_expectativa = str_remove(variable_expectativa, "_elast"),
         variable_expectativa = recode(variable_expectativa,
                                       "inflacion" = "Inflación",
                                       "pib" = "PIB",
                                       "tc" = "Tipo de cambio")) %>% 
  filter(!is.na(elasticidad)) 
  
  #
  (plt_points_elasticidad_sectores <- sector_expectativa %>% 
    filter(sector != "indicador_incertidumbre", elasticidad < quantile(elasticidad, 0.9),
           elasticidad > quantile(elasticidad, 0.1)) %>% 
  ggplot(aes(y = elasticidad, x = variable_expectativa)) +
  geom_jitter(width = 0.05, alpha = 0.4, color = "midnightblue") +
  #coord_cartesian(ylim = c(-2, 3)) +
  facet_wrap(~str_to_title(sector)) +
  theme_light() +
  labs(x = "\nVariable de expectativa", y = "Elasticidad"))
  
  (plt_hist_elasticidad_sectores <- sector_expectativa %>% 
    filter(sector != "indicador_incertidumbre", elasticidad < quantile(elasticidad, 0.9),
           elasticidad > quantile(elasticidad, 0.1)) %>% 
    ggplot(aes(x = elasticidad, fill = variable_expectativa)) +
    geom_histogram(position = "identity", alpha = 0.4) +
    facet_wrap(~sector) +
    theme_light() +
    labs(x = "Elasticidad", y = "Frecuencia", fill = NULL) +
    theme(legend.position = "bottom"))
  
 (plt_desnity_sector_elasticidad <- sector_expectativa %>% 
    filter(sector != "indicador_incertidumbre", elasticidad < quantile(elasticidad, 0.9),
           elasticidad > quantile(elasticidad, 0.1)) %>%
    ggplot(aes(x = elasticidad, y = variable_expectativa)) +
    #scale_y_reverse() +
    ggridges::geom_density_ridges(fill = "midnightblue", alpha = 0.6) + 
    facet_wrap(~str_to_title(sector)) +
    theme_light() +
    labs(y = NULL, x = "Elasticidad"))
  

(plt_hist_indicador_elasticidad <- sector_expectativa %>% 
  filter(sector == "indicador_incertidumbre", elasticidad < quantile(elasticidad, 0.9),
         elasticidad > quantile(elasticidad, 0.1)) %>% 
  ggplot(aes(x = elasticidad)) +
  geom_histogram(binwidth = 0.2, alpha = 0.6, fill = "midnightblue") +
  theme_light() +
  labs(y = "Frecuencia", x = "Elasticidad"))



