# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

df_votos <- list.files(path_output,
                       pattern = "tse - votos - ",
                       full.names = TRUE) %>%
  .[!grepl("tratada", basename(.), ignore.case = TRUE)] %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  dplyr::arrange(desc(mtime)) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(path) %>%
  readr::read_csv(show_col_types = FALSE)

df_votos <- df_votos %>%
  mutate(
    across(
      where(is.numeric),
      ~ replace(.x, .x %in% c(-1, -3), NA_real_)
    ),
    across(
      where(is.character),
      ~ replace(.x, .x %in% c("#NE", "#NULO#", "#NULO"), NA_character_)
    )
  )

colnames(df_votos)

df_votos %>%
  group_by(CD_CARGO, DS_CARGO) %>%
  summarise(n_observacoes = n())

df_votos <- df_votos %>%
  filter(
    CD_CARGO == 11 |
      DS_CARGO == "Prefeito"
  )

df_votos <- df_votos %>%
  mutate(
    QT_VOTOS_NOMINAIS = pmax(
      QT_VOTOS_NOMINAIS,
      QT_VOTOS_NOMINAIS_VALIDOS,
      na.rm = TRUE
    )
  )

df_votos <- df_votos %>%
  group_by(
    ANO_ELEICAO, NR_TURNO, 
    SG_UF, SG_UE, NM_UE, CD_MUNICIPIO, NM_MUNICIPIO, 
    CD_CARGO, DS_CARGO, NR_CANDIDATO, NM_CANDIDATO,
    CD_SITUACAO_CANDIDATURA, DS_SITUACAO_CANDIDATURA, CD_DETALHE_SITUACAO_CAND, DS_DETALHE_SITUACAO_CAND,
    NR_PARTIDO, SG_PARTIDO, NM_PARTIDO, DS_COMPOSICAO_COLIGACAO, NM_COLIGACAO,
    CD_SIT_TOT_TURNO, DS_SIT_TOT_TURNO
  ) %>%
  summarise(
    QT_VT_CANDIDATO = sum(QT_VOTOS_NOMINAIS, na.rm = TRUE),
    .groups = "drop"
  )

sum(is.na(df_votos$QT_VT_CANDIDATO))

df_votos %>%
  group_by(DS_SITUACAO_CANDIDATURA, CD_SITUACAO_CANDIDATURA) %>%
  summarise(n_observacoes = n())

df_votos <- df_votos %>%
  mutate(
    DS_SITUACAO_CANDIDATURA = case_when(
      CD_SITUACAO_CANDIDATURA %in% c(2, 4, 12)              ~ "apto",
      CD_SITUACAO_CANDIDATURA %in% c(3, 6, 7, 10, 14, 5, 8) ~ "inapto",
      TRUE                                                  ~ DS_SITUACAO_CANDIDATURA
    ),
    CD_SITUACAO_CANDIDATURA = case_when(
      DS_SITUACAO_CANDIDATURA == "apto"   ~ 1L,
      DS_SITUACAO_CANDIDATURA == "inapto" ~ 2L,
      TRUE                                ~ NA
    )
  )

df_votos %>%
  group_by(DS_DETALHE_SITUACAO_CAND, CD_DETALHE_SITUACAO_CAND) %>%
  summarise(n_observacoes = n())

df_votos <- df_votos %>%
  mutate(
    D_RENUNCIOU = as.integer(CD_DETALHE_SITUACAO_CAND == 6),
    D_CASSADO = as.integer(CD_DETALHE_SITUACAO_CAND %in% c(10, 18)),
    
    DS_DETALHE_SITUACAO_CAND = case_when(
      CD_DETALHE_SITUACAO_CAND %in% c(2, 16)                                    ~ "deferido",
      CD_DETALHE_SITUACAO_CAND %in% c(4, 5, 6, 7, 10, 14, 17, 18)               ~ "indeferido",
      TRUE                                                                       ~ NA_character_
    ) %>%
      str_to_lower(),
    CD_DETALHE_SITUACAO_CAND = case_when(
      DS_DETALHE_SITUACAO_CAND == "deferido"   ~ 1L,
      DS_DETALHE_SITUACAO_CAND == "indeferido" ~ 2L,
      TRUE                                     ~ NA
    )
  )

df_votos %>%
  group_by(DS_SIT_TOT_TURNO, CD_SIT_TOT_TURNO) %>%
  summarise(n_observacoes = n())

df_votos <- df_votos %>%
  mutate(
    D_2oT         = as.integer(CD_SIT_TOT_TURNO == 6),
    D_SUBSTITUIDO = as.integer(CD_SIT_TOT_TURNO == 10),
    
    DS_SIT_TOT_TURNO = case_when(
      CD_SIT_TOT_TURNO %in% c(1, 10)              ~ "eleito",
      CD_SIT_TOT_TURNO == 6                       ~ "2o turno",
      CD_SIT_TOT_TURNO %in% c(4, 3, 11)           ~ "nao eleito",
      TRUE                                        ~ NA_character_
    ) %>% 
      str_to_lower(),
    
    CD_SIT_TOT_TURNO = case_when(
      DS_SIT_TOT_TURNO == "eleito"     ~ 1L,
      DS_SIT_TOT_TURNO == "nao eleito" ~ 2L,
      DS_SIT_TOT_TURNO == "2o turno"   ~ 3L,
      TRUE                              ~ NA_integer_
    )
  )

df_votos %>%
  group_by(NR_TURNO, DS_SIT_TOT_TURNO, CD_SIT_TOT_TURNO) %>%
  summarise(n_observacoes = n())

unique(df_votos$SG_UF)
n_distinct(df_votos$SG_UE)
n_distinct(df_votos$NM_UE)
n_distinct(df_votos$CD_MUNICIPIO)

tmp <- list.files(path_input, pattern = "tse", full.names = TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  arrange(desc(mtime)) %>%
  slice(1) %>%
  pull(path) %>%
  read_csv(show_col_types = FALSE)

tmp <- tmp %>%
  rename(
    NR_MUNICIPIO_TSE       = codigo_tse,
    SG_UF        = uf,
    NM_MUNICIPIO = nome_municipio,
    D_CAPITAL      = capital,
    NR_MUNICIPIO_IBGE      = codigo_ibge
  ) %>%
  mutate(
    NR_MUNICIPIO = substr(NR_MUNICIPIO_IBGE, 3, 7),
    NR_MUNICIPIO_TSE       = as.character(NR_MUNICIPIO_TSE),
    SG_UF        = as.character(SG_UF),
    NR_MUNICIPIO_IBGE      = as.character(NR_MUNICIPIO_IBGE),
    NR_UF        = substr(NR_MUNICIPIO_IBGE, 1, 2),
    NR_MUNICIPIO_TSE       = str_pad(NR_MUNICIPIO_TSE, width = 5, side = "left", pad = "0")
  )

df_votos <- df_votos %>%
  select(-NM_MUNICIPIO) %>%
  left_join(tmp,
            by = c("CD_MUNICIPIO" = "NR_MUNICIPIO_TSE", 
                   "SG_UF"))

n_distinct(df_votos$NR_MUNICIPIO_IBGE)
sum(is.na(df_votos$NR_MUNICIPIO_IBGE))
n_distinct(df_votos$NM_MUNICIPIO)
sum(is.na(df_votos$NM_MUNICIPIO))
n_distinct(df_votos$NM_MUNICIPIO, df_votos$SG_UF)
n_distinct(df_votos$NR_MUNICIPIO)
sum(is.na(df_votos$NR_MUNICIPIO))
n_distinct(df_votos$NR_MUNICIPIO, df_votos$SG_UF)

tmp <- df_votos %>%
  group_by(ANO_ELEICAO, NR_PARTIDO, SG_PARTIDO) %>%
  summarise(n_observacoes = n(),
            .groups = "drop") %>%
  arrange(NR_PARTIDO)

df_votos <- df_votos %>%
  mutate(
    SG_PARTIDO = case_when(
      ANO_ELEICAO == 2016 ~ recode(as.character(NR_PARTIDO), !!!partidos2016, .default = SG_PARTIDO),
      ANO_ELEICAO == 2012 ~ recode(as.character(NR_PARTIDO), !!!partidos2012, .default = SG_PARTIDO),
      ANO_ELEICAO == 2008 ~ recode(as.character(NR_PARTIDO), !!!partidos2008, .default = SG_PARTIDO),
      ANO_ELEICAO == 2004 ~ recode(as.character(NR_PARTIDO), !!!partidos2004, .default = SG_PARTIDO),
      ANO_ELEICAO == 2000 ~ recode(as.character(NR_PARTIDO), !!!partidos2000, .default = SG_PARTIDO),
      TRUE                ~ SG_PARTIDO
    )
  )

sum(is.na(df_votos$NM_CANDIDATO))
n_distinct(df_votos$NM_CANDIDATO)
n_distinct(df_votos$NM_CANDIDATO, df_votos$SG_UF)
n_distinct(df_votos$ANO_ELEICAO, df_votos$NM_CANDIDATO, df_votos$SG_UF)
n_distinct(df_votos$ANO_ELEICAO, df_votos$NM_CANDIDATO, df_votos$NR_MUNICIPIO)
n_distinct(df_votos$ANO_ELEICAO, df_votos$NM_CANDIDATO, df_votos$NR_MUNICIPIO, df_votos$SG_UF)
n_distinct(df_votos$ANO_ELEICAO, df_votos$NM_CANDIDATO, df_votos$NR_MUNICIPIO, df_votos$SG_UF, df_votos$NR_TURNO)

df_votos <- df_votos %>%
  mutate(
    NM_CANDIDATO_OG = NM_CANDIDATO %>%
      stri_trans_general("Latin-ASCII") %>%
      str_remove_all("[[:punct:]]|[0-9]+") %>% 
      str_replace_all("(.)\\1+", "\\1") %>%
      tolower() %>%
      str_squish(), 
    
    NM_SUFIXO = case_when(
      str_detect(NM_CANDIDATO_OG, "\\bfilho\\b")    ~ "filho",
      str_detect(NM_CANDIDATO_OG, "\\bfilha\\b")    ~ "filha",
      str_detect(NM_CANDIDATO_OG, "\\bneto\\b")     ~ "neto",
      str_detect(NM_CANDIDATO_OG, "\\bneta\\b")     ~ "neta",
      str_detect(NM_CANDIDATO_OG, "\\bbisneto\\b")  ~ "bisneto",
      str_detect(NM_CANDIDATO_OG, "\\bbisneta\\b")  ~ "bisneta",
      str_detect(NM_CANDIDATO_OG, "\\bjunior\\b")   ~ "junior",
      TRUE                                      ~ NA_character_
    ),
    
    NM_CANDIDATO = NM_CANDIDATO_OG %>%
      str_remove_all("\\b(de|da|do|dos|das|e|filho|neto|filha|neta|bisneto|bisneta|junior)\\b") %>%
      str_squish(),
    
    NM_COMPOSTO = if_else(
      str_count(NM_CANDIDATO_OG, "\\S+") >= 3 &
        word(NM_CANDIDATO_OG, 2) %in% common_second,
      word(NM_CANDIDATO_OG, 2),
      NA_character_
    ),
    
    NM_CANDIDATO = if_else(
      !is.na(NM_COMPOSTO),
      str_squish(str_remove(NM_CANDIDATO, str_c("\\b", NM_COMPOSTO, "\\b"))),
      NM_CANDIDATO
    ),
    
    NM_NOME      = word(NM_CANDIDATO, 1),
    NM_SOBRENOME = word(NM_CANDIDATO, start = 2, end = -1),
    
    NM_COMPLETO = str_c(
      NM_NOME,
      coalesce(NM_COMPOSTO,  ""),
      coalesce(NM_SOBRENOME, ""),
      coalesce(NM_SUFIXO,    ""),
      sep = " "
    ) %>%
      str_squish(),
    
    NM_SOBRENOME_CM  = str_count(
      str_to_lower(NM_SOBRENOME),
      regex(paste0("\\b(", str_c(common_surnames, collapse = "|"),")\\b"))
    ),
    
    NM_SOBRENOME_ICM = str_count(NM_SOBRENOME, "\\S+") - NM_SOBRENOME_CM
  )

df_votos <- df_votos %>%
  rename(
    NR_ANO = ANO_ELEICAO,
    NR_UE = SG_UE,
    NR_CARGO = CD_CARGO,
    NM_CARGO = DS_CARGO, 
    NR_SIT_APTO = CD_SITUACAO_CANDIDATURA,
    NM_SIT_APTO = DS_SITUACAO_CANDIDATURA,
    NR_SIT_DEFERIDO = CD_DETALHE_SITUACAO_CAND,
    NM_SIT_DEFERIDO = DS_DETALHE_SITUACAO_CAND,
    SG_COLIGACAO = DS_COMPOSICAO_COLIGACAO,
    NR_SIT_CANDIDATO = CD_SIT_TOT_TURNO,
    NM_SIT_CANDIDATO = DS_SIT_TOT_TURNO
  ) %>%
  select(
    -c(CD_MUNICIPIO)
  )

df_votos <- df_votos %>%
  relocate(
    NR_ANO, NR_TURNO, SG_UF, NR_UF, NR_UE, NM_UE, NR_MUNICIPIO_IBGE, NR_MUNICIPIO,
    NM_MUNICIPIO, D_CAPITAL, NR_CARGO, NM_CARGO, NM_CANDIDATO, NR_CANDIDATO, 
    SG_PARTIDO, NM_PARTIDO, NR_SIT_APTO, NM_SIT_APTO, NR_SIT_DEFERIDO, NM_SIT_DEFERIDO,
    NR_SIT_CANDIDATO, NM_SIT_CANDIDATO, QT_VT_CANDIDATO, D_RENUNCIOU, D_CASSADO,
    D_SUBSTITUIDO, D_2oT,
  ) %>%
  arrange(
    NR_ANO, NR_TURNO, NR_MUNICIPIO_IBGE, 
  )

tmp <- df_votos %>%
  group_by(NR_ANO, NR_TURNO, NR_MUNICIPIO_IBGE) %>%
  reframe(
    # quem foi eleito em N (para juntar em N+4)
    NR_ELEITO_CAND = first(
      NR_CANDIDATO[NM_SIT_CANDIDATO == "eleito"],
      default = NA_integer_
    ),
    NM_ELEITO_CAND = first(
      NM_CANDIDATO[NM_SIT_CANDIDATO == "eleito"],
      default = NA_character_
    ),
    
    # quem teve maior voto em N (em caso de empate, pega o primeiro)
    NR_MAX_CAND    = first(
      NR_CANDIDATO[QT_VT_CANDIDATO == max(QT_VT_CANDIDATO, na.rm = TRUE)],
      default = NA_integer_
    ),
    NM_MAX_CAND    = first( # NAO PEGAR O PRIMEIRO
      NM_CANDIDATO[QT_VT_CANDIDATO == max(QT_VT_CANDIDATO, na.rm = TRUE)],
      default = NA_character_
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    NR_ANO_SEGUINTE = NR_ANO + 4
  ) %>%
  select(
    NR_ANO_SEGUINTE, NR_TURNO, NR_MUNICIPIO_IBGE,
    NR_ELEITO_CAND, NM_ELEITO_CAND,
    NR_MAX_CAND,    NM_MAX_CAND
  )

df_votos <- df_votos %>%
  left_join(
    tmp,
    by = c(
      "NR_ANO"            = "NR_ANO_SEGUINTE",
      "NR_TURNO",
      "NR_MUNICIPIO_IBGE"
    )
  ) %>%
  mutate(
    # votos do eleito no ano vigente
    QT_VT_ELEITO = if_else(
      NM_CANDIDATO == NM_ELEITO_CAND,
      QT_VT_CANDIDATO,
      NA
    ),
    
    # votos do maior voto no ano vigente
    QT_VT_MAX    = if_else(
      NM_CANDIDATO == NM_MAX_CAND,
      QT_VT_CANDIDATO,
      NA
    ),
    
    # flags de incumbência
    D_ELEITO = as.integer(!is.na(QT_VT_ELEITO)),
    D_MAX_VT = as.integer(!is.na(QT_VT_MAX)),
    D_INCUMB1 = as.integer(D_ELEITO == 1),
    D_INCUMB2 = as.integer(D_MAX_VT == 1),
    D_INCUMB3 = as.integer(D_ELEITO == 1 | D_MAX_VT == 1),
    
    across(
      c(QT_VT_ELEITO, QT_VT_MAX),
      ~ replace_na(.x, 0)
    )
  )

tmp <- df_votos %>%
  group_by(NR_ANO, NR_TURNO, NR_MUNICIPIO_IBGE) %>%
  summarise(
    # 1) votos do incumbente eleito (ou NA se não houve eleito)
    QT_VT_ELEITO = max(QT_VT_ELEITO, na.rm = TRUE),
    
    # 2) maior votação naquele município/ano
    QT_VT_MAX    = max(QT_VT_MAX, na.rm = TRUE),
    
    # 3) flags de incumbência
    D_INCUMB1        = as.integer(any(D_INCUMB1 == 1, na.rm = TRUE)),
    D_INCUMB2        = as.integer(any(D_INCUMB2 == 1, na.rm = TRUE)),
    D_INCUMB3        = as.integer(any(D_INCUMB3 == 1, na.rm = TRUE)),
    
    # 4) flags de partido
    D_PT             = as.integer(any(NR_PARTIDO == 13, na.rm = TRUE)),
    D_PSDB           = as.integer(any(NR_PARTIDO == 45, na.rm = TRUE)),
    
    # 5) número de candidatos
    QT_CANDIDATOS = n_distinct(NR_CANDIDATO),
    
    # 6) total de votos PT
    QT_VOTOS_PT  = sum(
      if_else(NR_PARTIDO == 13, QT_VT_CANDIDATO, 0),
      na.rm = TRUE
    ),
    QT_VOTOS_PSDB  = sum(
      if_else(NR_PARTIDO == 45, QT_VT_CANDIDATO, 0),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

df_votos <- df_votos %>%
  select(
    -c(
      QT_VT_ELEITO, QT_VT_MAX, D_INCUMB1, D_INCUMB2, D_INCUMB3
    )
  ) %>%
  left_join(
    tmp,
    by = c(
      "NR_ANO",
      "NR_TURNO",
      "NR_MUNICIPIO_IBGE"
    )
  )

write.csv(
  df_votos,
  file = file.path(
    path_output, 
    paste0("tse - votos - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
