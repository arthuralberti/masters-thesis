# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

df_candidatos <- list.files(path_output,
                            pattern = "tse - candidatos - ",
                            full.names = TRUE) %>%
  .[!grepl("tratada", basename(.), ignore.case = TRUE)] %>%
  file.info() %>%
  tibble::rownames_to_column("path") %>%
  dplyr::arrange(desc(mtime)) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(path) %>%
  readr::read_csv(show_col_types = FALSE)

df_candidatos <- df_candidatos %>%
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

colnames(df_candidatos)

df_candidatos %>%
  group_by(CD_CARGO, DS_CARGO) %>%
  summarise(n_observacoes = n())

df_vices <- df_candidatos %>%
  filter(
    CD_CARGO == 12 |
      DS_CARGO == "VICE-PREFEITO"
  )

df_candidatos <- df_candidatos %>%
  filter(
    CD_CARGO == 11 |
      DS_CARGO == "PREFEITO"
  )

df_candidatos %>%
  group_by(ST_CANDIDATO_INSERIDO_URNA) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  filter(
    ST_CANDIDATO_INSERIDO_URNA == "SIM"
  )

df_candidatos %>%
  group_by(NR_TURNO) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  filter(
    NR_TURNO == 1
  )

df_candidatos %>%
  group_by(DS_SITUACAO_CANDIDATURA, CD_SITUACAO_CANDIDATURA) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    DS_SITUACAO_CANDIDATURA = case_when(
      CD_SITUACAO_CANDIDATURA %in% c(2, 4, 12)              ~ "apto",
      CD_SITUACAO_CANDIDATURA %in% c(3, 6, 7, 8) ~ "inapto",
      TRUE                                                  ~ DS_SITUACAO_CANDIDATURA
    ),
    CD_SITUACAO_CANDIDATURA = case_when(
      DS_SITUACAO_CANDIDATURA == "apto"   ~ 1L,
      DS_SITUACAO_CANDIDATURA == "inapto" ~ 2L,
      TRUE                                ~ NA
    )
  )

df_candidatos %>%
  group_by(DS_DETALHE_SITUACAO_CAND, CD_DETALHE_SITUACAO_CAND) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    D_RENUNCIOU = as.integer(CD_DETALHE_SITUACAO_CAND == 6),
    D_CASSADO = as.integer(CD_DETALHE_SITUACAO_CAND %in% c(10, 18)),
    D_INELEGIVEL = as.integer(CD_DETALHE_SITUACAO_CAND == 9),
    
    DS_DETALHE_SITUACAO_CAND = case_when(
      CD_DETALHE_SITUACAO_CAND %in% c(2, 16)                                    ~ "deferido",
      CD_DETALHE_SITUACAO_CAND %in% c(4, 5, 6, 7, 10, 14, 17, 18, 9, 13)        ~ "indeferido",
      TRUE                                                                      ~ NA_character_
    ) %>%
      str_to_lower(),
    CD_DETALHE_SITUACAO_CAND = case_when(
      DS_DETALHE_SITUACAO_CAND == "deferido"   ~ 1L,
      DS_DETALHE_SITUACAO_CAND == "indeferido" ~ 2L,
      TRUE                                     ~ NA
    )
  )

df_candidatos %>%
  group_by(DS_SIT_TOT_TURNO, CD_SIT_TOT_TURNO) %>%
  summarise(n_observacoes = n())

#df_candidatos <- df_candidatos %>%
#  mutate(
#    D_2oT         = as.integer(CD_SIT_TOT_TURNO == 6),
#    D_SUBSTITUIDO = as.integer(CD_SIT_TOT_TURNO == 10),
#    
#    DS_SIT_TOT_TURNO = case_when(
#      CD_SIT_TOT_TURNO %in% c(1, 7, 10)              ~ "eleito",
#      CD_SIT_TOT_TURNO == 6                          ~ "2o turno",
#      CD_SIT_TOT_TURNO %in% c(4, 3, 11, 13, 8, 9)           ~ "nao eleito",
#      TRUE                                        ~ NA_character_
#    ) %>% 
#      str_to_lower(),
#    
#    CD_SIT_TOT_TURNO = case_when(
#      DS_SIT_TOT_TURNO == "eleito"     ~ 1L,
#      DS_SIT_TOT_TURNO == "nao eleito" ~ 2L,
#      DS_SIT_TOT_TURNO == "2o turno"   ~ 3L,
#      TRUE                              ~ NA_integer_
#    )
#  )
#
# df_candidatos %>%
#   group_by(NR_TURNO, DS_SIT_TOT_TURNO, CD_SIT_TOT_TURNO) %>%
#   summarise(n_observacoes = n())

unique(df_candidatos$SG_UF)
n_distinct(df_candidatos$SG_UF, df_candidatos$SG_UE)
n_distinct(df_candidatos$SG_UE)
n_distinct(df_candidatos$SG_UF, df_candidatos$NM_UE)

tmp <- df_candidatos %>%
  group_by(ANO_ELEICAO, NR_PARTIDO, SG_PARTIDO) %>%
  summarise(n_observacoes = n(),
            .groups = "drop") %>%
  arrange(NR_PARTIDO)

df_candidatos <- df_candidatos %>%
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

sum(is.na(df_candidatos$NM_CANDIDATO))
n_distinct(df_candidatos$NM_CANDIDATO)
n_distinct(df_candidatos$NM_CANDIDATO, df_candidatos$SG_UF)
n_distinct(df_candidatos$ANO_ELEICAO, df_candidatos$NM_CANDIDATO, df_candidatos$SG_UF)
n_distinct(df_candidatos$ANO_ELEICAO, df_candidatos$NM_CANDIDATO, df_candidatos$SG_UE)
n_distinct(df_candidatos$ANO_ELEICAO, df_candidatos$NM_CANDIDATO, df_candidatos$SG_UE, df_candidatos$NR_TURNO)

df_candidatos <- df_candidatos %>%
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

df_candidatos %>%
  group_by(CD_NACIONALIDADE,
           DS_NACIONALIDADE) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    DS_NACIONALIDADE = case_when(
      CD_NACIONALIDADE %in% c(1, 2) ~ "brasileiro",
      CD_NACIONALIDADE == 3        ~ "portugues",
      CD_NACIONALIDADE == 4        ~ "estrangeiro",
      TRUE                         ~ NA_character_
    ),
    CD_NACIONALIDADE = case_when(
      DS_NACIONALIDADE == "brasileiro"  ~ 1L,
      DS_NACIONALIDADE == "portugues"   ~ 2L,
      DS_NACIONALIDADE == "estrangeiro" ~ 3L,
      TRUE                              ~ NA_integer_
    )
  )

tmp <- df_candidatos %>%
  group_by(CD_MUNICIPIO_NASCIMENTO,
           NM_MUNICIPIO_NASCIMENTO) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    NM_MUNICIPIO_NASCIMENTO = NM_MUNICIPIO_NASCIMENTO %>%
      stri_trans_general("Latin-ASCII") %>%
      str_remove_all("[[:punct:]]|[0-9]+") %>% 
      str_replace_all("[^[:alpha:] ]+", "") %>%
      str_replace_all("(.)\\1+", "\\1") %>%
      tolower() %>%
      str_squish(), 
  )

df_candidatos %>%
  group_by(CD_GRAU_INSTRUCAO,
           DS_GRAU_INSTRUCAO) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    DS_GRAU_INSTRUCAO = case_when(
      CD_GRAU_INSTRUCAO %in% c(-4) ~ NA_character_,
      CD_GRAU_INSTRUCAO %in% c(0)  ~ "não informado",
      CD_GRAU_INSTRUCAO %in% c(1)  ~ "analfabeto",
      CD_GRAU_INSTRUCAO %in% c(2)  ~ "lê e escreve",
      CD_GRAU_INSTRUCAO %in% c(3)  ~ "fundamental incompleto",
      CD_GRAU_INSTRUCAO %in% c(4)  ~ "fundamental completo",
      CD_GRAU_INSTRUCAO %in% c(5)  ~ "médio incompleto",
      CD_GRAU_INSTRUCAO %in% c(6)  ~ "médio completo",
      CD_GRAU_INSTRUCAO %in% c(7)  ~ "superior incompleto",
      CD_GRAU_INSTRUCAO %in% c(8)  ~ "superior completo",
      TRUE                         ~ NA_character_
    ),
    
    CD_GRAU_INSTRUCAO = case_when(
      DS_GRAU_INSTRUCAO %in% c("não informado")           ~  0L,
      DS_GRAU_INSTRUCAO %in% c("analfabeto")              ~  1L,
      DS_GRAU_INSTRUCAO %in% c("lê e escreve")            ~  2L,
      DS_GRAU_INSTRUCAO %in% c("fundamental incompleto")  ~  3L,
      DS_GRAU_INSTRUCAO %in% c("fundamental completo")    ~  4L,
      DS_GRAU_INSTRUCAO %in% c("médio incompleto")        ~  5L,
      DS_GRAU_INSTRUCAO %in% c("médio completo")          ~  6L,
      DS_GRAU_INSTRUCAO %in% c("superior incompleto")     ~  7L,
      DS_GRAU_INSTRUCAO %in% c("superior completo")       ~  8L,
      TRUE                                                ~ NA_integer_
    )
  )

tmp <- df_candidatos %>%
  group_by(CD_SITUACAO_CANDIDATO_PLEITO,
           DS_SITUACAO_CANDIDATO_PLEITO) %>%
  summarise(n_observacoes = n())

df_candidatos %>%
  group_by(CD_SITUACAO_CANDIDATO_URNA,
           DS_SITUACAO_CANDIDATO_URNA) %>%
  summarise(n_observacoes = n())

df_candidatos %>%
  group_by(CD_SITUACAO_CANDIDATO_TOT,
           DS_SITUACAO_CANDIDATO_TOT) %>%
  summarise(n_observacoes = n())

df_candidatos <- df_candidatos %>%
  mutate(
    D_2oT = as.integer(CD_SIT_TOT_TURNO == 6),
  ) %>%
  filter(
    NR_TURNO == 1
  )

df_candidatos <- df_candidatos %>%
  select(
    ANO_ELEICAO,
    SG_UF,
    SG_UE,
    NM_UE,
    CD_CARGO,
    DS_CARGO,
    NR_CANDIDATO,
    NM_CANDIDATO,
    CD_SITUACAO_CANDIDATURA,
    DS_SITUACAO_CANDIDATURA,
    CD_DETALHE_SITUACAO_CAND,
    DS_DETALHE_SITUACAO_CAND,
    NR_PARTIDO,
    SG_PARTIDO,
    NM_PARTIDO,
    NM_COLIGACAO,
    DS_COMPOSICAO_COLIGACAO,
    CD_NACIONALIDADE,
    DS_NACIONALIDADE,
    SG_UF_NASCIMENTO,
    CD_MUNICIPIO_NASCIMENTO,
    NM_MUNICIPIO_NASCIMENTO,
    DT_NASCIMENTO,
    CD_GENERO,
    DS_GENERO,
    CD_GRAU_INSTRUCAO,
    DS_GRAU_INSTRUCAO,
    CD_ESTADO_CIVIL,
    DS_ESTADO_CIVIL,
    CD_COR_RACA,
    DS_COR_RACA,
    CD_OCUPACAO,
    DS_OCUPACAO,
    VR_DESPESA_MAX_CAMPANHA,
    CD_SIT_TOT_TURNO,
    DS_SIT_TOT_TURNO,
    ST_REELEICAO,
    CD_SITUACAO_CANDIDATO_PLEITO,
    DS_SITUACAO_CANDIDATO_PLEITO,
    CD_SITUACAO_CANDIDATO_URNA,
    DS_SITUACAO_CANDIDATO_URNA,
    ST_CANDIDATO_INSERIDO_URNA,
    CD_SITUACAO_CANDIDATO_TOT,
    DS_SITUACAO_CANDIDATO_TOT,
    ST_SUBSTITUIDO,
    DT_ACEITE_CANDIDATURA,
    NR_FEDERACAO,
    NM_FEDERACAO,
    SG_FEDERACAO,
    DS_COMPOSICAO_FEDERACAO
  )

df <- df_votos %>%
  full_join(df_candidatos,
            by = c(
              "ANO_ELEICAO",
              "SG_UF",
              "SG_UE",
              "CD_CARGO",
              "NR_CANDIDATO"
              #              "NM_CANDIDATO",
              #              "SG_PARTIDO"
            ))

tmp <- df %>%
  # conta quantas vezes cada candidato aparece ignorando turno
  add_count(
    ANO_ELEICAO, SG_UF, SG_UE, CD_CARGO, NR_CANDIDATO,
    name = "n_obs"
  ) %>%
  # filtra só quem aparece mais de uma vez
  filter(n_obs > 1) %>%
  # mantém apenas as colunas-chaves + o nome
  select(
    ANO_ELEICAO,
    SG_UF,
    SG_UE,
    CD_CARGO,
    NR_CANDIDATO, 
    n_obs
  ) %>%
  # elimina duplicatas exatas
  distinct()


colnames(df)

df <- df %>%
  rename(
    NR_ANO = ANO_ELEICAO,
    NR_UE = SG_UE,
    NR_MUNICIPIO = CD_MUNICIPIO,
    NR_CARGO = CD_CARGO,
    NM_CARGO = DS_CARGO, 
    NR_SIT_CANDIDATO = CD_SIT_TOT_TURNO,
    NM_SIT_CANDIDATO = DS_SIT_TOT_TURNO,
    NM_SIT_URNA_CANDIDATO = ST_CANDIDATO_INSERIDO_URNA,
    NM_NACIONALIDADE = DS_NACIONALIDADE, 
    NR_MUNICIPIO_NASCIMENTO = CD_MUNICIPIO_NASCIMENTO,
    NR_GENERO = CD_GENERO,
    NM_GENERO = DS_GENERO,
    NR_GRAU_INSTRUCAO = CD_GRAU_INSTRUCAO,
    NM_GRAU_INSTRUCAO = DS_GRAU_INSTRUCAO,
    NR_ESTADO_CIVIL = CD_ESTADO_CIVIL,
    NM_ESTADO_CIVIL = DS_ESTADO_CIVIL,
    NR_COR_RACA = CD_COR_RACA,
    NM_COR_RACA = DS_COR_RACA,
    NR_OCUPACAO = CD_OCUPACAO,
    NM_OCUPACAO = DS_OCUPACAO,
    QT_VT_CANDIDATO = QT_VOTOS_CANDIDATO,
    QT_VT_PUDERAM = QT_APTOS,
    QT_VT_COMPARECERAM = QT_COMPARECIMENTO,
    QT_VT_ABSTERAM = QT_ABSTENCOES,
    QT_VT_CANDIDATOS = QT_VOTOS_NOMINAIS,
    QT_VT_BRANCO = QT_VOTOS_BRANCOS,
    QT_VT_NULO = QT_VOTOS_NULOS
  )


# saving adjusted final voting by municipality database
write.csv(
  df,
  file = file.path(
    file.path(path_output),
    paste0("tse - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)



################################################################################



sum(is.na(df$QT_VT_CANDIDATO))
sum(is.na(df$QT_VT_PUDERAM))
sum(is.na(df$QT_VT_COMPARECERAM))
sum(is.na(df$QT_VT_ABSTERAM))
sum(is.na(df$QT_VT_CANDIDATOS))
sum(is.na(df$QT_VT_BRANCO))
sum(is.na(df$QT_VT_NULO))

tmp <- df %>%
  filter(is.na(QT_VT_CANDIDATO))

tmp %>% # tem 79 eleitos e sem votos
  distinct() %>%                        
  count(NM_SIT_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))

tmp %>% # tem 2060 candidatos com nome na urna e sem votos
  distinct() %>%                        
  count(NM_SIT_URNA_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))

tmp <- tmp %>%
  filter(NM_SIT_URNA_CANDIDATO == "SIM")

tmp %>% # tem 31 eleitos com nome na urna e sem votos
  distinct() %>%                        
  count(NM_SIT_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))

df <- df %>%
  filter(!is.na(QT_VT_CANDIDATO))

df %>% # tem 308 candidatos sem nome na urna e com votos
  distinct() %>%                        
  count(NM_SIT_URNA_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))

df %>%
  distinct() %>%                        
  count(NM_SIT_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))             

df %>%
  distinct() %>%                        
  count(NR_SIT_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))  

df <- df %>%
  mutate(
    NM_SIT_CANDIDATO = NM_SIT_CANDIDATO %>%
      trimws() %>%
      stri_trans_general("Latin-ASCII") %>%
      tolower() %>%
      str_replace_all("[^a-z0-9 ]", " ") %>%
      str_squish(),
    NM_SIT_CANDIDATO = case_when(
      NM_SIT_CANDIDATO %in% c("2 turno") ~ "2o turno",
      NM_SIT_CANDIDATO %in% c(
        "renuncia falecimento com substituicao",
        "indeferido com recurso"
      ) ~ "nao concorreu",
      NM_SIT_CANDIDATO %in% c("nulo") ~ NA,
      TRUE ~ NM_SIT_CANDIDATO),
    NM_SIT_CANDIDATO = case_when(
      is.na(NM_SIT_CANDIDATO) & NR_SIT_CANDIDATO == 1           ~ "eleito",
      is.na(NM_SIT_CANDIDATO) & NR_SIT_CANDIDATO == 4           ~ "nao eleito",
      is.na(NM_SIT_CANDIDATO) & NR_SIT_CANDIDATO %in% c(10, 11) ~ "nao concorreu",
      TRUE                                                      ~ NM_SIT_CANDIDATO
    ),
    
    NM_SIT_CANDIDATO_OG = NM_SIT_CANDIDATO
  )

tmp <- df %>%
  filter(NR_TURNO == 2) %>%
  select(
    NR_ANO,
    NR_MUNICIPIO_IBGE,
    NR_CANDIDATO,
    NM_SIT_2T = NM_SIT_CANDIDATO
  )

df <- df %>%
  left_join(tmp,
            by = c("NR_ANO","NR_MUNICIPIO_IBGE","NR_CANDIDATO")) %>%
  mutate(
    NM_SIT_CANDIDATO = case_when(
      NR_TURNO == 1 & NM_SIT_CANDIDATO_OG == "2o turno" ~ NM_SIT_2T,
      TRUE                                              ~ NM_SIT_CANDIDATO_OG
    )
  ) %>%
  select(-NM_SIT_2T)

df %>%
  distinct() %>%                        
  count(NM_SIT_CANDIDATO_OG, name = "n") %>%       
  mutate(prop = n / sum(n)) 

df %>%
  distinct() %>%                        
  count(NM_SIT_CANDIDATO, name = "n") %>%       
  mutate(prop = n / sum(n))             

tmp <- df %>% # 91 + 17 + 1 candidatos receberam votos e "nao concorreram"
  filter(NM_SIT_CANDIDATO %in% c("nao concorreu", NA))

df <- df %>% 
  mutate(
    NR_MUNICIPIO = str_pad(NR_MUNICIPIO, width = 6, side = "left", pad = "0"),
    
    NM_CARGO = NM_CARGO %>%
      trimws() %>%
      stri_trans_general("Latin-ASCII") %>%
      tolower() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all("vice([a-z]+)", "vice \\1") %>%
      str_squish(),
    
    NM_ORIGINAL = NM_CANDIDATO %>%
      str_replace_all("['`´‘’.,;-]", "") %>%
      stri_trans_general("Latin-ASCII") %>%
      str_remove_all("\\b[[:alpha:]]\\b") %>%
      tolower() %>%
      str_replace_all("(.)\\1+", "\\1") %>%
      str_squish(), 
    
    NM_CANDIDATO = NM_ORIGINAL %>%
      str_remove_all("\\b(de|da|do|dos|das|e|filho|neto|filha|neta|bisneto|bisneta|junior)\\b") %>%
      str_squish()
  )

df %>% 
  distinct() %>%                        
  count(NM_CARGO, name = "n") %>%       
  mutate(prop = n / sum(n))            

df %>% 
  distinct() %>%                        
  count(NR_CARGO, name = "n") %>%       
  mutate(prop = n / sum(n))

df <- df %>%
  mutate(
    NM_CARGO = replace_na(NM_CARGO, "prefeito")
  )

n_distinct(df$NR_MUNICIPIO_IBGE)
n_distinct(df$SG_UF, df$NR_MUNICIPIO)
n_distinct(df$NR_ANO, df$SG_UF, df$NR_MUNICIPIO)
n_distinct(df$NR_ANO) * n_distinct(df$NR_MUNICIPIO_IBGE) 

tmp <- df %>% ##################### CHECAR
  group_by(NR_ANO, NR_MUNICIPIO_IBGE) %>%
  mutate(
    is_eleito = as.integer(NM_SIT_CANDIDATO == "eleito"),
    is_maxv   = as.integer(QT_VT_CANDIDATO == max(QT_VT_CANDIDATO, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  filter(is_eleito == 1 | is_maxv == 1) %>%
  mutate(
    NR_ANO_SEGUINTE = NR_ANO + 4,
    QT_VT_ANTERIOR  = QT_VT_CANDIDATO
  ) %>%
  rename(
    NR_ANTERIOR = NR_CANDIDATO,
    NM_ANTERIOR = NM_CANDIDATO,
    D_ELEITO    = is_eleito,
    D_MAX_VT    = is_maxv
  ) %>%
  select(
    NR_ANO, NR_ANO_SEGUINTE, NR_MUNICIPIO_IBGE, NR_TURNO,
    NR_ANTERIOR, NM_ANTERIOR, QT_VT_ANTERIOR, D_ELEITO, D_MAX_VT
  ) %>%
  distinct()

df <- df %>%
  left_join(
    tmp,
    by = c(
      "NR_ANO" = "NR_ANO_SEGUINTE",
      "NR_MUNICIPIO_IBGE", "NR_TURNO",
      "NR_CANDIDATO" = "NR_ANTERIOR",
      "NM_CANDIDATO" = "NM_ANTERIOR"
    )
  ) %>%
  mutate(
    D_ELEITO   = replace_na(D_ELEITO, 0),
    D_MAX_VT     = replace_na(D_MAX_VT,   0),
    D_INCUMBENTE     = as.integer(D_ELEITO == 1 | D_MAX_VT == 1)
  )

tmp <- tmp %>%
  group_by(NR_ANO_SEGUINTE, NR_MUNICIPIO_IBGE) %>%
  summarise(
    # todos os candidatos eleitos (caso haja mais de um), colapsados
    NR_ELEITO_CAND    = paste(unique(NR_ANTERIOR[D_ELEITO == 1]), collapse = ";"),
    NM_ELEITO_CAND    = paste(unique(NM_ANTERIOR[D_ELEITO == 1]), collapse = ";"),
    QT_VT_ELEITO      = paste(unique(QT_VT_ANTERIOR[D_ELEITO == 1]), collapse = ";"),
    
    # todos os candidatos de maior voto (caso haja empate), colapsados
    NR_MAX_VT_CAND    = paste(unique(NR_ANTERIOR[D_MAX_VT == 1]), collapse = ";"),
    NM_MAX_VT_CAND    = paste(unique(NM_ANTERIOR[D_MAX_VT == 1]), collapse = ";"),
    QT_VT_MAX_VT      = paste(unique(QT_VT_ANTERIOR[D_MAX_VT == 1]), collapse = ";"),
    
    .groups = "drop"
  )

tmp %>%
  filter(str_detect(NR_ELEITO_CAND, ";")) %>%
  nrow()







#################################
df_mun <- df_mun %>%
  rename(
    NR_ANO = ANO_ELEICAO,
    NR_UE = SG_UE,
    QT_VT_PUDERAM = QT_APTOS,
    QT_VT_COMPARECERAM = QT_COMPARECIMENTO,
    QT_VT_ABSTERAM = QT_ABSTENCOES,
    QT_VT_CANDIDATOS = QT_VOTOS_NOMINAIS,
    QT_VT_BRANCO = QT_VOTOS_BRANCOS,
    QT_VT_NULO = QT_VOTOS_NULOS
  )





df_mun <- df_mun %>%
  left_join(
    tmp %>%
      filter(NR_ANO_SEGUINTE <= 2016),
    by = c(
      "NR_ANO" = "NR_ANO_SEGUINTE", 
      "NR_MUNICIPIO_IBGE"
    )
  )

df_mun <- df_mun %>%
  mutate(across(
    c(NR_ELEITO_CAND, NM_ELEITO_CAND, QT_VT_ELEITO),
    ~ ifelse(is.na(.) | . == "NA", NA, .)
  ))

df_mun <- df_mun %>%
  mutate(
    D_TEM_INCUMBENTE1 = as.integer(!is.na(NR_ELEITO_CAND)),
    D_TEM_INCUMBENTE2 = as.integer(!is.na(NR_MAX_VT_CAND)),
    D_TEM_INCUMBENTE3 = as.integer(TEM_INCUMBENTE1 == 1 | TEM_INCUMBENTE2 == 1)
  ) %>%
  
  mutate(
    # turnout e abstenção
    QT_VT_ABSTERAM2 = QT_VT_PUDERAM - QT_VT_COMPARECERAM,
    
    SH_TURNOUT     = QT_VT_COMPARECERAM / QT_VT_PUDERAM, # sh_turnout e sh_abstention deveria ser complementar
    SH_ABSTENTION_OG  = QT_VT_ABSTERAM    / QT_VT_PUDERAM,
    SH_ABSTENTION  = 1 - SH_TURNOUT,
    
    # votos válidos = total que compareceu – (brancos + nulos)
    QT_VT_VALIDOS  = QT_VT_COMPARECERAM - QT_VT_BRANCO - QT_VT_NULO,
    SH_VOTOS_VALIDOS = QT_VT_VALIDOS     / QT_VT_COMPARECERAM,
    SH_VOTOS_BRANCOS = QT_VT_BRANCO       / QT_VT_COMPARECERAM,
    SH_VOTOS_NULOS   = QT_VT_NULO         / QT_VT_COMPARECERAM,
    SH_VOTOS_B_N     = SH_VOTOS_BRANCOS + SH_VOTOS_NULOS
  ) %>%
  
  mutate(
    SH_INCUMBENTE11 = as.numeric(QT_VT_ELEITO   ) / QT_VT_PUDERAM,
    SH_INCUMBENTE12 = as.numeric(QT_VT_ELEITO   ) / QT_VT_COMPARECERAM,
    SH_INCUMBENTE13 = as.numeric(QT_VT_ELEITO   ) / QT_VT_VALIDOS,
    
    SH_INCUMBENTE21 = as.numeric(QT_VT_MAX_VT   ) / QT_VT_PUDERAM,
    SH_INCUMBENTE22 = as.numeric(QT_VT_MAX_VT   ) / QT_VT_COMPARECERAM,
    SH_INCUMBENTE23 = as.numeric(QT_VT_MAX_VT   ) / QT_VT_VALIDOS,
  )

write.csv(
  df_mun,
  file = file.path(
    file.path(path_output),
    paste0("tse - municípios - tratada - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
