# author: Arthur Alberti
# purpose: 

source(
  file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "0_raw",
            "functions.R"
  )
)

df_eleicao <- data.frame()
df_votos <- data.frame()
df_candidatos <- data.frame()

for (year in c(2000, 2004, 2008, 2012, 2016)) { 
  
  tmp_zip    <- tempfile(fileext = ".zip")
  
  url_eleicao <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_%d.zip",
    year
  )
  
  download.file(url_eleicao, tmp_zip, mode = "wb", quiet = TRUE, method = "libcurl")
  file_list <- unzip(tmp_zip, list = TRUE)$Name
  csv_file  <- grep("BRASIL.*\\.csv$", file_list, ignore.case = TRUE, value = TRUE)[1]
  
  tmp1 <- read_delim(
    unz(tmp_zip, csv_file),
    delim    = ";",
    locale   = locale(encoding = "ISO-8859-1"),
    quote    = "\"",
    trim_ws  = TRUE,
    show_col_types = FALSE
  ) %>%
    mutate(across(everything(), as.character))
  
  url_votos <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_%d.zip",
    year
  )
  
  download.file(url_votos, tmp_zip, mode = "wb", quiet = TRUE, method = "libcurl")
  file_list <- unzip(tmp_zip, list = TRUE)$Name
  csv_file  <- grep("BRASIL.*\\.csv$", file_list, ignore.case = TRUE, value = TRUE)[1]
  
  tmp2 <- read_delim(
    unz(tmp_zip, csv_file),
    delim    = ";",
    locale   = locale(encoding = "ISO-8859-1"),
    quote    = "\"",
    trim_ws  = TRUE,
    show_col_types = FALSE
  ) %>%
    mutate(across(everything(), as.character))
  
  url_candidatos <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_%d.zip",
    year
  )
  
  download.file(url_candidatos, tmp_zip, mode = "wb", quiet = TRUE, method = "libcurl")
  file_list <- unzip(tmp_zip, list = TRUE)$Name
  csv_file  <- grep("BRASIL.*\\.csv$", file_list, ignore.case = TRUE, value = TRUE)[1]
  
  tmp3 <- read_delim(
    unz(tmp_zip, csv_file),
    delim    = ";",
    locale   = locale(encoding = "ISO-8859-1"),
    quote    = "\"",
    trim_ws  = TRUE,
    show_col_types = FALSE
  ) %>%
    mutate(across(everything(), as.character))
  
  df_eleicao <- bind_rows(df_eleicao, 
                          tmp1)
  
  df_votos <- bind_rows(df_votos,
                        tmp2)
  
  df_candidatos <- bind_rows(df_candidatos,
                             tmp3)
  
  rm(tmp1, tmp2, tmp3)
  
}

# saving the tse databases
write.csv(
  df_eleicao,
  file = file.path(
    path_output,
    paste0("tse - eleições - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)

write.csv(
  df_votos,
  file = file.path(
    path_output, 
    paste0("tse - votos - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)

write.csv(
  df_candidatos,
  file = file.path(
    path_output, 
    paste0("tse - candidatos - ", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  ),
  row.names = FALSE
)
