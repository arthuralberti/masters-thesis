# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

df_eleicoes <- list.files(path_output,
                     pattern = "tse - eleições - tratada", full.names = TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  arrange(desc(mtime)) %>%
  slice(1) %>%
  pull(path) %>%
  read_csv(show_col_types = FALSE)

df_votos <- list.files(path_output,
                          pattern = "tse - votos - tratada", full.names = TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  arrange(desc(mtime)) %>%
  slice(1) %>%
  pull(path) %>%
  read_csv(show_col_types = FALSE)

df_cadunico <- list.files(path_output, 
                          pattern = "cadunico - tratada", full.names = TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  arrange(desc(mtime)) %>%
  slice(1) %>%
  pull(path) %>%
  read_csv(show_col_types = FALSE)

# filters
n_distinct(df_cadunico$NR_ANO)                                                   # 13
n_distinct(df_cadunico$NR_MUNICIPIO)                                             # 5492

df_cadunico <- df_cadunico %>%
  filter(NR_ANO %in% c(2000, 2004, 2008, 2012, 2016)) %>%
  mutate(NR_MUNICIPIO = as.character(NR_MUNICIPIO))

unique(df_cadunico$NR_ANO)                                                   # 4
n_distinct(df_cadunico$NR_MUNICIPIO)                                             # 5492
n_distinct(df_cadunico$NR_ANO, df_cadunico$NR_MUNICIPIO)                         # deveria ser 4*5492 = 21968

# tse
colnames(df_eleicoes)

unique(df_eleicoes$NR_ANO)
n_distinct(df_eleicoes$NR_ANO)                                                   # 5
n_distinct(df_eleicoes$NR_MUNICIPIO_IBGE)                                        # 5568
n_distinct(df_eleicoes$NR_ANO, df_eleicoes$NR_MUNICIPIO_IBGE)                    # deveria ser 5*5568 = 27816
n_distinct(df_eleicoes$NR_ANO, df_eleicoes$NR_TURNO, df_eleicoes$NR_MUNICIPIO_IBGE)

# bases
## base completa
df <- df_eleicoes %>%
  mutate(NR_MUNICIPIO = substr(NR_MUNICIPIO_IBGE, 1, 6)) %>%
  select(-c(SG_UF)) %>%
  left_join(
    df_cadunico,
    by = c(
      "NR_ANO",
      "NR_MUNICIPIO"
    )
  )

mean(df$TREATMENT, na.rm = TRUE)

df %>%
  group_by(NR_TURNO) %>%
  summarise(n_observacoes = n())

df <- df %>%
  mutate(
    NR_TURNO = replace_na(NR_TURNO, 1L)
  )

df %>%
  group_by(TREATMENT, NR_TURNO) %>%
  summarise(n_observacoes = n())

df %>%
  group_by(NR_TURNO) %>%
  summarise(
    mean_treat = mean(TREATMENT, na.rm = TRUE),
    .groups = "drop"
  )

# votos 

n_distinct(df$NR_ANO, df$NR_TURNO, df$NR_MUNICIPIO_IBGE)
n_distinct(df_votos$NR_ANO, df_votos$NR_TURNO, df_votos$NR_MUNICIPIO_IBGE)

n_distinct(df$NR_MUNICIPIO_IBGE)
n_distinct(df_votos$NR_MUNICIPIO_IBGE)

unique(df_votos$NR_ANO)
sum(is.na(df_votos$QT_VT_CANDIDATO))

df <- df_votos %>%
  left_join(
    df %>%
      select(
        -c(SG_UF, NR_UF, NR_UE, NR_MUNICIPIO, NM_MUNICIPIO, D_CAPITAL)
      ),
    by = c(
      "NR_ANO",
      "NR_TURNO",
      "NR_MUNICIPIO_IBGE"
    )
  )

# saving adjusted final voting by municipality database
write.csv(
  df,
  file = file.path(
    file.path(path_output),
    paste0("base final - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
