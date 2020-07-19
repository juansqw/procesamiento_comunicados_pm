ggsave(
  here::here("visualizaciones", "plt_bigram_entorno.png"),
  plt_bigram_entorno,
  height = 8,
  width = 9,
  dpi = 350)

ggsave(
  here::here("visualizaciones", "plt_bigram_year.png"),
  plt_bigram_year,
  height = 8,
  width = 9,
  dpi = 350)


ggsave(
  here::here("visualizaciones", "plt_monogram.png"),
  plt_monogram,
  height = 8,
  width = 9,
  dpi = 350)

ggsave(
  here::here("visualizaciones", "plt_monogram.png"),
  plt_palablas_comunicados,
  height = 6,
  width = 8,
  dpi = 350)

ggsave(
  here::here("visualizaciones", "plt_palablas_comunicados_smooth.png"),
  plt_palablas_comunicados_smooth,
  height = 6,
  width = 8,
  dpi = 350)


ggsave(
  here::here("visualizaciones", "plt_tf_idf.png"),
  plt_tf_idf,
  height = 8,
  width = 9,
  dpi = 350)

ggsave(
  here::here("visualizaciones", "plt_sentiment_entorno.png"),
  plt_sentiment_entorno,
  height = 8,
  width = 9,
  dpi = 350)


ggsave(
  here::here("visualizaciones", "plt_poins_elasticidad_sectore.png"),
  plt_points_elasticidad_sectores,
  height = 5,
  width = 8,
  dpi = 350)


ggsave(
  here::here("visualizaciones", "plt_density_elasticidad_sectores.png"),
  plt_desnity_sector_elasticidad,
  height = 5,
  width = 8,
  dpi = 350)

ggsave("visualizaciones/plt_indicador_hist.png", plt_hist_indicador_elasticidad,
       height = 5, width = 8, dpi = 350)

