# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

# collecting the databases
df_eleicoes <- list.files(path_output,
                 pattern = "tse - eleições - ",
                 full.names = TRUE) %>%
  .[!grepl("tratada", basename(.), ignore.case = TRUE)] %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  dplyr::arrange(desc(mtime)) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(path) %>%
  readr::read_csv(show_col_types = FALSE)

# adjusting the database
colnames(df_eleicoes)

df_eleicoes <- df_eleicoes %>%
  select(
    ANO_ELEICAO, NR_TURNO, SG_UF, SG_UE, NM_UE, CD_MUNICIPIO, NM_MUNICIPIO,
    CD_CARGO, DS_CARGO, 
    
    QT_APTOS, QT_COMPARECIMENTO, QT_ABSTENCOES, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS,
    QT_VOTOS_NULOS, QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS
  )

df_eleicoes %>%
  group_by(CD_CARGO, DS_CARGO) %>%
  summarise(n_observacoes = n())

df_eleicoes <- df_eleicoes %>%
  filter(
    CD_CARGO == 11 |
      DS_CARGO == "Prefeito"
  )

df_eleicoes <- df_eleicoes %>%
  mutate(
    across(
      starts_with("QT_"),
      ~ {
        x <- as.numeric(.x)
        x[is.na(x) | x == -3] <- 0
        x
      })
  )

sum(is.na(df_eleicoes$QT_APTOS))
sum(is.na(df_eleicoes$QT_COMPARECIMENTO))
sum(is.na(df_eleicoes$QT_ABSTENCOES))
sum(is.na(df_eleicoes$QT_VOTOS_NOMINAIS))
sum(is.na(df_eleicoes$QT_VOTOS_BRANCOS))
sum(is.na(df_eleicoes$QT_VOTOS_NULOS))
sum(is.na(df_eleicoes$QT_VOTOS_LEGENDA))
sum(is.na(df_eleicoes$QT_VOTOS_ANULADOS))

unique(df_eleicoes$ANO_ELEICAO)
n_distinct(df_eleicoes$SG_UE)
n_distinct(df_eleicoes$NM_UE)
n_distinct(df_eleicoes$CD_MUNICIPIO)
n_distinct(df_eleicoes$NM_MUNICIPIO)
n_distinct(df_eleicoes$NM_MUNICIPIO, df_eleicoes$SG_UF)

df_eleicoes <- df_eleicoes %>%
  mutate(
    NM_MUNICIPIO = NM_MUNICIPIO %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish() %>%
      str_replace_all("([a-z])\\1+", "\\1"),
    
    NM_UE = NM_UE %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish() %>%
      str_replace_all("([a-z])\\1+", "\\1"),
    
    across(
      c(SG_UE, CD_MUNICIPIO),
      ~ str_pad(as.character(.), width = 5, side = "left", pad = "0")
    )
  )

df_eleicoes <- df_eleicoes %>%
  group_by(
    ANO_ELEICAO, NR_TURNO, SG_UF, SG_UE, CD_MUNICIPIO
  ) %>%
  summarise(
    across(
      c(
        QT_APTOS, QT_COMPARECIMENTO, QT_ABSTENCOES, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS,
        QT_VOTOS_NULOS, QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

n_distinct(df_eleicoes$CD_MUNICIPIO) * n_distinct(df_eleicoes$ANO_ELEICAO)

df_eleicoes %>%
  group_by(NR_TURNO) %>%
  summarise(n_observacoes = n())

df_eleicoes <- df_eleicoes %>%
  mutate(
    SH_INVALIDOS      = (QT_VOTOS_BRANCOS + QT_VOTOS_NULOS) / QT_COMPARECIMENTO,

    DIFF_VOTOS        = QT_COMPARECIMENTO -
      (QT_VOTOS_NOMINAIS + QT_VOTOS_BRANCOS + QT_VOTOS_NULOS),
    SH_DIFF_VOTOS     = DIFF_VOTOS / QT_COMPARECIMENTO,
    
    # share de comparecimento (turnout) e abstenção
    SH_TURNOUT   = QT_COMPARECIMENTO / QT_APTOS,
    SH_ABSTENCAO = 1 - SH_TURNOUT,
    SH_ABSTENCAO_OG = QT_ABSTENCOES    / QT_APTOS,
    
    # share de votos válidos, brancos e nulos (sobre os que compareceram)
    SH_VOTOS_VALIDOS  = if_else(QT_COMPARECIMENTO > 0,
                                QT_VOTOS_NOMINAIS   / QT_COMPARECIMENTO,
                                NA_real_),
    SH_VOTOS_BRANCOS  = if_else(QT_COMPARECIMENTO > 0,
                                QT_VOTOS_BRANCOS     / QT_COMPARECIMENTO,
                                NA_real_),
    SH_VOTOS_NULOS    = if_else(QT_COMPARECIMENTO > 0,
                                QT_VOTOS_NULOS       / QT_COMPARECIMENTO,
                                NA_real_),
    
    SH_VOTOS_ANULADOS  = if_else(QT_COMPARECIMENTO > 0,
                                 QT_VOTOS_ANULADOS  / QT_COMPARECIMENTO,
                                 NA_real_)
  )

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
    NR_MUNICIPIO_TSE       = as.character(NR_MUNICIPIO_TSE),
    SG_UF        = as.character(SG_UF),
    NR_MUNICIPIO_IBGE      = as.character(NR_MUNICIPIO_IBGE),
    NR_UF        = substr(NR_MUNICIPIO_IBGE, 1, 2),
    NR_MUNICIPIO_TSE       = str_pad(NR_MUNICIPIO_TSE, width = 5, side = "left", pad = "0")
  )

df_eleicoes <- df_eleicoes %>%
  left_join(tmp,
            by = c("CD_MUNICIPIO" = "NR_MUNICIPIO_TSE", 
                   "SG_UF"))

df_eleicoes <- df_eleicoes %>%
  rename(
    NR_ANO = ANO_ELEICAO,
    NR_UE = SG_UE,
    NR_MUNICIPIO = CD_MUNICIPIO,
    QT_VT_CANDIDATO = QT_VOTOS_NOMINAIS,
    QT_VT_PUDERAM = QT_APTOS,
    QT_VT_COMPARECERAM = QT_COMPARECIMENTO,
    QT_VT_ABSTERAM = QT_ABSTENCOES,
    QT_VT_CANDIDATOS = QT_VOTOS_NOMINAIS,
    QT_VT_BRANCO = QT_VOTOS_BRANCOS,
    QT_VT_NULO = QT_VOTOS_NULOS
  )

write.csv(
  df_eleicoes,
  file = file.path(
    path_output, 
    paste0("tse - eleições - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
