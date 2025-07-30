# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

# collects the databases
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

df_ibge <- read_dta(file.path(path_input, "códigos - ibge.dta")) %>%
  select(cod2000, cod2010)

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
    
#    AUMENTOU_20062 = as.integer(`2004` < `20062`),                                      # mean: 0.3987
#    AUMENTOU_20092 = as.integer(`20062` < `20092`),                                     # mean: 0.5903
#    AUMENTOU_20122 = as.integer(`20092` < `20122`),                                     # mean: 0.4364
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
  )

df_estimativa <- df_estimativa %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "NR_ANO",
    values_to = "QT_FAM_EST"
  )

df_estimativa <- df_estimativa %>%
  arrange(NR_MUNICIPIO, NR_ANO) %>%
  group_by(NR_MUNICIPIO) %>%
  mutate(QT_FAM_EST2 = cummax(QT_FAM_EST)) %>%
  ungroup()

df_estimativa <- df_estimativa %>%
  mutate(
    NR_MUNICIPIO = substr(NR_MUNICIPIO, 1, 6),
  ) %>%
  crossing(NR_MES = as.character(1:12))

## quotas realizadas
df_realizado <- df_realizado %>%
  separate(Referência, into = c("NR_MES", "NR_ANO"), sep = "/", convert = TRUE) 

df_realizado <- df_realizado %>%
  mutate(
    QT_FAM_REAL = if_else(NR_ANO < 2023,
                            `Famílias PBF (até Out/2021)`, 
                            `Famílias PBF (a partir de Mar/2023)`),
    
    QT_VAL_REAL = if_else(NR_ANO < 2023,
                         `Valor repassado às famílias PBF (até Out/2021)`,
                         `Valor repassado às famílias PBF (a partir de Mar/2023)`),
    
    across(
      everything(),
      ~ str_remove(.x, ",00$")),
    
    across(
      c(QT_FAM_REAL, QT_VAL_REAL),
      ~ parse_number(.x,                   
                     locale = locale(decimal_mark = ",",
                                     grouping_mark = "."))),
    
    QT_VAL_FAM_REAL = QT_VAL_REAL / QT_FAM_REAL
  )

df_realizado <- df_realizado %>%
  select(
    NR_ANO,
    NR_MES,
    NR_MUNICIPIO = Código,
    QT_FAM_REAL,
    QT_VAL_REAL,
    QT_VAL_FAM_REAL
  )

# codigos do ibge
sum(is.na(df_ibge$cod2000))
sum(is.na(df_ibge$cod2010))

df_ibge <- df_ibge %>%
  mutate(
    D_MUN_NOVO = as.integer(is.na(cod2000)),
    cod2010 = substr(as.character(cod2010), 1, nchar(cod2010) - 1)
  )

# controle e tratamento
# construir de novo


# merge da base final
df <- df_estimativa %>%
  left_join(df_realizado,
            by = c(
              "NR_ANO",
              "NR_MES",
              "NR_MUNICIPIO"
            )) %>%
  filter(NR_ANO <= 2020)

sum(is.na(df_quotas$treated))

df <- df %>%
  inner_join(df_quotas %>%
              select(NR_MUNICIPIO = cod_ibge, 
                     TREATMENT = treated) %>%
              mutate(NR_MUNICIPIO = as.character(NR_MUNICIPIO)),
            by = "NR_MUNICIPIO") 

df <- df %>%
  left_join(df_ibge %>%
              select(
                cod2010, D_MUN_NOVO
              ),
            by = c("NR_MUNICIPIO" = "cod2010"))

df <- df %>%
  mutate(
    DATE = as.Date(paste(NR_ANO, NR_MES, "01", sep = "-"))
  )

fix_keys <- tribble(
  ~NR_MUNICIPIO, ~NR_ANO, ~NR_MES,
  520080, 2011, 4,
  110146, 2011, 4,
  520410, 2011, 4,
  353510, 2011, 4,
  241180, 2011, 4,
  410405, 2011, 4,
  352450, 2011, 4,
  410810, 2011, 11
)

df <- df %>%
  arrange(NR_MUNICIPIO, DATE) %>% 
  group_by(NR_MUNICIPIO) %>% 
  mutate(
    QT_FAM_REAL = if_else(
      (NR_MUNICIPIO %in% fix_keys$NR_MUNICIPIO) &
        (NR_ANO            %in% fix_keys$NR_ANO) &
        (NR_MES            %in% fix_keys$NR_MES),
      lag(QT_FAM_REAL, n = 1, default = NA_real_),
      QT_FAM_REAL
    )
  ) %>% 
  ungroup()

df <- df %>%
  group_by(NR_MUNICIPIO) %>%
  mutate(
    QT_FAM_EST            = as.numeric(QT_FAM_EST),
    QT_FAM_REAL           = as.numeric(QT_FAM_REAL),
    QT_VAL_REAL           = as.numeric(QT_VAL_REAL),
    
    AUMENTOU_2006 = first(AUMENTOU_2006),
    AUMENTOU_2009 = first(AUMENTOU_2009),
    AUMENTOU_2012 = first(AUMENTOU_2012),
    
    TREATMENT2 = as.integer(AUMENTOU_2009 == 1 | AUMENTOU_2012 == 1),
      
    D_SEM_EST = as.integer(
      any(is.na(QT_FAM_EST)) |
        any(QT_FAM_EST == 0 & format(DATE, "%Y") == "2009")
    ), 
    D_SEM_REAL = as.integer(
      any(is.na(QT_FAM_REAL)) |
        any(QT_FAM_REAL == 0 & format(DATE, "%Y") == "2009")
    ),
    D_SEM_TREAT = as.integer(
      any(is.na(TREATMENT))),
    
    ROLL12_QT_FAM_REAL = rollmean(QT_FAM_REAL, k = 12, fill = NA, align = "right"),
    ROLL12_QT_VAL_REAL = rollmean(QT_VAL_REAL, k = 12, fill = NA, align = "right"),
    
    FAM_REAL_0701_0903 = mean(
      QT_FAM_REAL[DATE >= as.Date("2007-01-01") & DATE <= as.Date("2009-03-01")],
      na.rm = TRUE
    ),
    FAM_REAL_0903_1209 = mean(
      QT_FAM_REAL[DATE >= as.Date("2009-03-01") & DATE <= as.Date("2012-09-30")],
      na.rm = TRUE
    ),
    
    FAM_EST_0701_0903 = mean(
      QT_FAM_EST[DATE >= as.Date("2007-01-01") & DATE <= as.Date("2009-03-01")],
      na.rm = TRUE
    ),
    FAM_EST_0903_1209 = mean(
      QT_FAM_EST[DATE >= as.Date("2009-03-01") & DATE <= as.Date("2012-09-30")],
      na.rm = TRUE
    ),
    
    VAL_REAL_0701_0903 = mean(
      QT_VAL_REAL[DATE >= as.Date("2007-01-01") & DATE <= as.Date("2009-03-01")],
      na.rm = TRUE
    ),
    VAL_REAL_0903_0903 = mean(
      QT_VAL_REAL[DATE >= as.Date("2009-03-01") & DATE <= as.Date("2012-09-30")],
      na.rm = TRUE
    ),
    
    VAR_FAM_REAL = (QT_FAM_REAL - FAM_REAL_0701_0903) / FAM_REAL_0701_0903,
    VAR_FAM_EST = (QT_FAM_EST - FAM_EST_0701_0903) / FAM_EST_0701_0903,
    VAR_VAL_REAL = (log(QT_VAL_REAL) - log(VAL_REAL_0701_0903)) / log(VAL_REAL_0701_0903),
    
    SH_FAM = (QT_FAM_REAL / QT_FAM_EST),
    ROLL3_SH_FAM = rollmean(SH_FAM, k = 3, fill = NA, align = "right"),
    ROLL9_SH_FAM = rollmean(SH_FAM, k = 9, fill = NA, align = "right"),
    ROLL12_SH_FAM = rollmean(SH_FAM, k = 12, fill = NA, align = "right"),
    SH_FAM_0801_0809 = mean(
      QT_FAM_REAL[DATE >= as.Date("2008-01-01") & DATE <= as.Date("2008-09-30")],
      na.rm = TRUE
    ) / QT_FAM_EST,
    SH_FAM_1201_1209 = mean(
      QT_FAM_REAL[DATE >= as.Date("2012-01-01") & DATE <= as.Date("2012-09-30")],
      na.rm = TRUE
    ) / QT_FAM_EST,
    SH_FAM_1601_1609 = mean(
      QT_FAM_REAL[DATE >= as.Date("2016-01-01") & DATE <= as.Date("2016-09-30")],
      na.rm = TRUE
    ) / QT_FAM_EST,
    
    EST_2004         = QT_FAM_EST[NR_ANO == 2004][1],
    REAL_0501        = ROLL12_QT_FAM_REAL[NR_ANO == 2005 & NR_MES == 1][1],
    REAL_0809        = ROLL12_QT_FAM_REAL[NR_ANO == 2008 & NR_MES == 9][1],
    VR_FAM_2004_2008 = (REAL_0809 - REAL_0501) / EST_2004,
    
    EST_2008         = QT_FAM_EST[NR_ANO == 2008][1],
    REAL_0901        = ROLL12_QT_FAM_REAL[NR_ANO == 2009 & NR_MES == 1][1],
    REAL_1209        = ROLL12_QT_FAM_REAL[NR_ANO == 2012 & NR_MES == 9][1],
    VR_FAM_2008_2012 = (REAL_1209 - REAL_0901) / EST_2008,
    
    EST_2012         = QT_FAM_EST[NR_ANO == 2012][1],
    REAL_1301        = ROLL12_QT_FAM_REAL[NR_ANO == 2013 & NR_MES == 1][1],
    REAL_1609        = ROLL12_QT_FAM_REAL[NR_ANO == 2016 & NR_MES == 9][1],
    VR_FAM_2012_2016 = (REAL_1609 - REAL_1301) / EST_2012,
    
    NR_MESES_TREAT_2008 = cumsum(as.integer(
      DATE >= ymd("2005-01-01") &
        DATE <= ymd("2008-09-30") &
        VR_FAM_2004_2008 >= 1
    )),
    NR_MESES_TREAT_2012 = cumsum(as.integer(
      DATE >= ymd("2009-01-01") &
        DATE <= ymd("2012-09-30") &
        VR_FAM_2008_2012 >= 1
    )),
    NR_MESES_TREAT_2016 = cumsum(as.integer(
      DATE >= ymd("2013-01-01") &
        DATE <= ymd("2016-09-30") &
        VR_FAM_2012_2016 >= 1
    ))
  ) %>%
  ungroup()

df <- df %>%
  group_by(NR_MUNICIPIO) %>%
  mutate(

  ) %>%
  ungroup()

sum(is.na(df$TREATMENT))

df <- df %>%
  filter(
    D_SEM_TREAT == 0
  )

mean(df$TREATMENT) # 0.5

tmp <- df %>%
  filter(year(DATE) == 2012, month(DATE) == 9) %>%
  select(SG_UF, NR_MUNICIPIO, 
         SH_FAM_1209 = SH_FAM, 
         ROLL3_SH_FAM_1209 = ROLL3_SH_FAM,
         ROLL9_SH_FAM_1209 = ROLL9_SH_FAM,
         ROLL12_SH_FAM_1209 = ROLL12_SH_FAM,
         )

tmp1 <- df %>%
  filter(year(DATE) == 2009, month(DATE) == 1) %>%
  select(SG_UF, NR_MUNICIPIO, 
         SH_FAM_0901 = SH_FAM, 
         ROLL3_SH_FAM_0901 = ROLL3_SH_FAM,
         ROLL9_SH_FAM_0901 = ROLL9_SH_FAM,
         ROLL12_SH_FAM_0901 = ROLL12_SH_FAM,
  )

df <- df %>% 
  group_by(SG_UF, NR_MUNICIPIO, NR_ANO) %>% 
  arrange(NR_MES, .by_group = TRUE) %>% 
  summarise(
    # ---------- “último (dezembro)” ----------------------------------------
    QT_FAM_EST          = last(QT_FAM_EST),
    QT_FAM_EST2         = last(QT_FAM_EST2),
    QT_FAM_REAL         = last(QT_FAM_REAL),
    QT_VAL_REAL         = last(QT_VAL_REAL),
    ROLL12_QT_FAM_REAL  = last(ROLL12_QT_FAM_REAL),
    ROLL12_QT_VAL_REAL  = last(ROLL12_QT_VAL_REAL),
    ROLL12_SH_FAM       = last(ROLL12_SH_FAM),
    SH_FAM              = last(SH_FAM),
    
    # ---------- “média do ano” --------------------------------------------
    QT_VAL_FAM_REAL     = mean(QT_VAL_FAM_REAL, na.rm = TRUE),
    
    # ---------- “primeiro (janeiro)” --------------------------------------
    TREATMENT           = first(TREATMENT),
    TREATMENT2          = first(TREATMENT2),
    D_MUN_NOVO          = first(D_MUN_NOVO),
    D_SEM_EST           = first(D_SEM_EST),
    D_SEM_REAL          = first(D_SEM_REAL),
    D_SEM_TREAT         = first(D_SEM_TREAT),
    
    # ---------- “constantes” ----------------------------------------------
    FAM_REAL_0701_0903  = first(FAM_REAL_0701_0903),
    FAM_REAL_0903_1209  = first(FAM_REAL_0903_1209),
    FAM_EST_0701_0903   = first(FAM_EST_0701_0903),
    FAM_EST_0903_1209   = first(FAM_EST_0903_1209),
    VAL_REAL_0701_0903  = first(VAL_REAL_0701_0903),
    VAL_REAL_0903_0903  = first(VAL_REAL_0903_0903),
    SH_FAM_0801_0809    = first(SH_FAM_0801_0809),
    SH_FAM_1201_1209    = first(SH_FAM_1201_1209),
    SH_FAM_1601_1609    = first(SH_FAM_1601_1609),
    VR_FAM_2004_2008    = first(VR_FAM_2004_2008),
    VR_FAM_2008_2012    = first(VR_FAM_2008_2012),
    VR_FAM_2012_2016    = first(VR_FAM_2012_2016),
    
    # ---------- “maior do ano” --------------------------------------------
    NR_MESES_TREAT_2008      = max(NR_MESES_TREAT_2008, na.rm = TRUE),
    NR_MESES_TREAT_2012      = max(NR_MESES_TREAT_2012, na.rm = TRUE),
    NR_MESES_TREAT_2016      = max(NR_MESES_TREAT_2016, na.rm = TRUE),
    .groups = "drop"
  )

n_distinct(df$NR_ANO, df$NR_MUNICIPIO)

df <- df %>%
  left_join(tmp, 
            by = c("SG_UF", "NR_MUNICIPIO")) 

df <- df %>%
  left_join(tmp1, 
            by = c("SG_UF", "NR_MUNICIPIO")) 

write.csv(
  df,
  file = file.path(
    path_output,
    paste0("cadunico - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
