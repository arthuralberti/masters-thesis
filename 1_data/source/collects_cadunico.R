# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

# data bases
df_estimativa <- read_excel(
  file.path(path_input, "famílias - estimativa.xlsx"),
  col_names = c("SG_UF", "NM_MUNICIPIO", "NR_MUNICIPIO", "NM_REGIAO", "2004", 
                "2006", "2009", "2012"),
  col_types = "text"
)

df_realizado <- read_csv(
  file.path(path_input, "famílias - realizado.csv"),
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

df_quotas <- read_csv(
  file.path(path_output, "base_tratados_domiciliar.csv"),
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

# ajustando as bases
## quotas estimadas
df_estimativa <- df_estimativa %>%
  mutate(
    NM_MUNICIPIO = tolower(NM_MUNICIPIO) %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^A-Za-z ]", "") %>%
      str_squish(),
    
    NM_REGIAO = tolower(NM_REGIAO) %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^A-Za-z ]", "") %>%
      str_squish())

df_estimativa <- df_estimativa %>%
  mutate(
    AUMENTOU_2006 = as.integer(`2004` < `2006`),                                        # mean: 0.3987
    AUMENTOU_2009 = as.integer(`2006` < `2009`),                                        # mean: 0.6763
    AUMENTOU_2012 = as.integer(`2009` < `2012`),                                        # mean: 0.5350
    
    `20062` = if_else(AUMENTOU_2006 == 0, `2004`, `2006`),
    `20092` = if_else(AUMENTOU_2009 == 0, `2006`, `2009`),
    `20122` = if_else(AUMENTOU_2012 == 0, `2009`, `2012`),
    
    AUMENTOU_20062 = as.integer(`2004` < `20062`),                                        # mean: 0.3987
    AUMENTOU_20092 = as.integer(`20062` < `20092`),                                       # mean: 0.5903
    AUMENTOU_20122 = as.integer(`20092` < `20122`),                                       # mean: 0.4364
  ) %>%
  select(-c(`20062`, `20092`, `20122`))

df_estimativa <- df_estimativa %>%
  mutate(
    `2005` = `2004`,
    `2007` = `2006`,
    `2008` = `2006`,
    `2010` = `2009`,
    `2011` = `2009`,
    `2013` = `2012`,
    `2014` = `2012`,
    `2015` = `2012`,
    `2016` = `2012`,
    `2017` = `2012`,
    `2018` = `2012`,
    `2019` = `2012`,
    `2020` = `2012`,
    `2021` = `2012`,
    `2022` = `2012`,
    `2023` = `2012`,
    `2024` = `2012`,
  )

df_estimativa <- df_estimativa %>%
    pivot_longer(
      cols = starts_with("20"),
      names_to = "ANO",
      values_to = "FAMILIAS_EST"
    )

df_estimativa <- df_estimativa %>%
  arrange(NR_MUNICIPIO, ANO) %>%
  group_by(NR_MUNICIPIO) %>%
  mutate(FAMILIAS_EST2 = cummax(FAMILIAS_EST)) %>%
  ungroup()

df_estimativa <- df_estimativa %>%
    mutate(
      NR_MUNICIPIO = substr(NR_MUNICIPIO, 1, 6),
    ) %>%
    crossing(MES = as.character(1:12))

## quotas realizadas
df_realizado <- df_realizado %>%
  separate(Referência, into = c("MES", "ANO"), sep = "/", convert = TRUE) 

df_realizado <- df_realizado %>%
  mutate(
    FAMILIAS_REAL = if_else(ANO < 2023,
                            `Famílias PBF (até Out/2021)`, 
                            `Famílias PBF (a partir de Mar/2023)`),
    
    VALOR_REAL = if_else(ANO < 2023,
                         `Valor repassado às famílias PBF (até Out/2021)`,
                         `Valor repassado às famílias PBF (a partir de Mar/2023)`),
    
    across(
      everything(),
      ~ str_remove(.x, ",00$")),
    
    across(
      c(FAMILIAS_REAL, VALOR_REAL),
      ~ parse_number(.x,                   
                     locale = locale(decimal_mark = ",",
                                     grouping_mark = ".")))
  )

df_realizado <- df_realizado %>%
  select(
    ANO,
    MES,
    NR_MUNICIPIO = Código,
    FAMILIAS_REAL,
    VALOR_REAL
  )

# controle e tratamento



# merge da base final
df <- df_estimativa %>%
  left_join(df_realizado,
            by = c(
              "ANO",
              "MES",
              "NR_MUNICIPIO"
            )) %>%
  filter(ANO <= 2016)

df <- df %>%
  left_join(df_quotas %>%
              select(NR_MUNICIPIO = cod_ibge, 
                     TREATMENT = treated) %>%
              mutate(NR_MUNICIPIO = as.character(NR_MUNICIPIO)),
            by = "NR_MUNICIPIO") 

df <- df %>%
  mutate(
    DATE = as.Date(paste(ANO, MES, "01", sep = "-"))
    )

df <- df %>%
  relocate(ANO, MES, DATE, NM_REGIAO, SG_UF, NM_MUNICIPIO, NR_MUNICIPIO, FAMILIAS_EST, 
           FAMILIAS_REAL, VALOR_REAL, TREATMENT) %>%
  arrange(NR_MUNICIPIO, DATE)

write.csv(
  df,
  file = file.path(
    path_output,
    paste0("cadunico - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
