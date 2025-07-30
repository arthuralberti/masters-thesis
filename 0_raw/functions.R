# author: Arthur Alberti
# purpose: Load required libraries and set up project paths for analysis of 
# Bolsa Família 2009 policy design

rm(list = ls()) 
gc()

# libraries
# core “tidy” data manipulation and visualization
library(tidyverse)    # dplyr, tidyr, readr, purrr, ggplot2, tibble, stringr, forcats

# data import from various formats
library(haven)        # read stata/sas/spss (.dta, .sas7bdat, .sav)
library(readxl)       # read excel files (.xls, .xlsx)
library(writexl)

# high-performance data handling
library(data.table)   # fast i/o and in-memory data.table syntax

# string processing (beyond stringr in tidyverse)
library(stringi)      # advanced string operations and unicode handling

# econometric and statistical modeling
library(fixest)       # fast fixed-effects panel/regressions
library(broom)        # tidy extraction of model outputs
library(zoo)          # time-series utilities (rolling, index)

# plot enhancements
library(scales)       # axis scale functions for ggplot2
library(extrafont)    # use system fonts in graphics

library(did)
library(geobr)
library(fixest)
library(sandwich)
library(lmtest)
library(readxl)
library(janitor)
library(dplyr)
library(readr)   # parse_number()
library(stringr)
library(rdrobust)
library(rddensity)
library(ggeffects)
library(rdd)          # IKbandwidth()

library(ggplot2)
library(gridExtra)

# define working directory as the current script location
file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file_directory)

# define relevant paths
path_dropbox   <- "~/Library/CloudStorage/Dropbox/masters-thesis"
path_input    <- "~/Library/CloudStorage/Dropbox/masters-thesis/input"
path_output   <- "~/Library/CloudStorage/Dropbox/masters-thesis/output"

uf_dict <- c(
  "AC" = "12", "AL" = "27", "AM" = "13", "AP" = "16", "BA" = "29",
  "CE" = "23", "DF" = "53", "ES" = "32", "GO" = "52", "MA" = "21",
  "MG" = "31", "MS" = "50", "MT" = "51", "PA" = "15", "PB" = "25",
  "PE" = "26", "PI" = "22", "PR" = "41", "RJ" = "33", "RN" = "24",
  "RO" = "11", "RR" = "14", "RS" = "43", "SC" = "42", "SE" = "28",
  "SP" = "35", "TO" = "17"
)

partidos2016 <- c(
  "10" = "PRB",     # Partido Republicano Brasileiro
  "11" = "PP",      # Progressistas
  "12" = "PDT",     # Partido Democrático Trabalhista
  "13" = "PT",      # Partido dos Trabalhadores
  "14" = "PTB",     # Partido Trabalhista Brasileiro
  "15" = "PMDB",    # Partido do Movimento Democrático Brasileiro
  "16" = "PSTU",    # Partido Socialista dos Trabalhadores Unificado
  "17" = "PSL",     # Partido Social Liberal
  "18" = "REDE",    # Rede Sustentabilidade
  "19" = "PTN",     # Partido Trabalhista Nacional
  "20" = "PSC",     # Partido Social Cristão
  "21" = "PCB",     # Partido Comunista Brasileiro
  "22" = "PR",      # Partido da República
  "23" = "PPS",     # Partido Popular Socialista
  "25" = "DEM",     # Democratas
  "27" = "PSDC",    # Partido Social Democrata Cristão
  "28" = "PRTB",    # Partido Renovador Trabalhista Brasileiro
  "29" = "PCO",     # Partido da Causa Operária
  "30" = "NOVO",    # Partido Novo
  "31" = "PHS",     # Partido Humanista da Solidariedade
  "33" = "PMN",     # Partido da Mobilização Nacional
  "35" = "PMB",     # Partido da Mulher Brasileira
  "36" = "PTC",     # Partido Trabalhista Cristão
  "40" = "PSB",     # Partido Socialista Brasileiro
  "43" = "PV",      # Partido Verde
  "44" = "PRP",     # Partido Republicano Progressista
  "45" = "PSDB",    # Partido da Social Democracia Brasileira
  "50" = "PSOL",    # Partido Socialismo e Liberdade
  "51" = "PEN",     # Partido Ecológico Nacional
  "54" = "PPL",     # Partido Pátria Livre
  "55" = "PSD",     # Partido Social Democrático
  "65" = "PCdoB",   # Partido Comunista do Brasil
  "70" = "PTdoB",   # Partido Trabalhista do Brasil
  "77" = "SD",      # Solidariedade
  "90" = "PROS"     # Partido Republicano da Ordem Social
)

partidos2012 <- c(
  "10" = "PRB",     # Partido Republicano Brasileiro
  "11" = "PP",      # Progressistas
  "12" = "PDT",     # Partido Democrático Trabalhista
  "13" = "PT",      # Partido dos Trabalhadores
  "14" = "PTB",     # Partido Trabalhista Brasileiro
  "15" = "PMDB",    # Partido do Movimento Democrático Brasileiro
  "16" = "PSTU",    # Partido Socialista dos Trabalhadores Unificado
  "17" = "PSL",     # Partido Social Liberal
  "19" = "PTN",     # Partido Trabalhista Nacional
  "20" = "PSC",     # Partido Social Cristão
  "21" = "PCB",     # Partido Comunista Brasileiro
  "22" = "PR",      # Partido da República
  "23" = "PPS",     # Partido Popular Socialista
  "25" = "DEM",     # Democratas
  "27" = "PSDC",    # Partido Social Democrata Cristão
  "28" = "PRTB",    # Partido Renovador Trabalhista Brasileiro
  "29" = "PCO",     # Partido da Causa Operária
  "31" = "PHS",     # Partido Humanista da Solidariedade
  "33" = "PMN",     # Partido da Mobilização Nacional
  "36" = "PTC",     # Partido Trabalhista Cristão
  "40" = "PSB",     # Partido Socialista Brasileiro
  "43" = "PV",      # Partido Verde
  "44" = "PRP",     # Partido Republicano Progressista
  "45" = "PSDB",    # Partido da Social Democracia Brasileira
  "50" = "PSOL",    # Partido Socialismo e Liberdade
  "51" = "PEN",     # Partido Ecológico Nacional
  "54" = "PPL",     # Partido Pátria Livre
  "55" = "PSD",     # Partido Social Democrático
  "65" = "PCdoB",   # Partido Comunista do Brasil
  "70" = "PTdoB"    # Partido Trabalhista do Brasil
)

partidos2008 <- c(
  "10" = "PRB",     # Partido Republicano Brasileiro
  "11" = "PP",      # Progressistas
  "12" = "PDT",     # Partido Democrático Trabalhista
  "13" = "PT",      # Partido dos Trabalhadores
  "14" = "PTB",     # Partido Trabalhista Brasileiro
  "15" = "PMDB",    # Movimento Democrático Brasileiro
  "16" = "PSTU",    # Partido Socialista dos Trabalhadores Unificado
  "17" = "PSL",     # Partido Social Liberal
  "19" = "PTN",     # Partido Trabalhista Nacional
  "20" = "PSC",     # Partido Social Cristão
  "21" = "PCB",     # Partido Comunista Brasileiro
  "22" = "PR",      # Partido da República
  "23" = "PPS",     # Partido Popular Socialista
  "25" = "DEM",     # Democratas
  "27" = "PSDC",    # Partido Social Democrata Cristão
  "28" = "PRTB",    # Partido Renovador Trabalhista Brasileiro
  "29" = "PCO",     # Partido da Causa Operária
  "31" = "PHS",     # Partido Humanista da Solidariedade
  "33" = "PMN",     # Partido da Mobilização Nacional
  "36" = "PTC",     # Partido Trabalhista Cristão
  "40" = "PSB",     # Partido Socialista Brasileiro
  "43" = "PV",      # Partido Verde
  "44" = "PRP",     # Partido Republicano Progressista
  "45" = "PSDB",    # Partido da Social Democracia Brasileira
  "50" = "PSOL",    # Partido Socialismo e Liberdade
  "65" = "PCdoB",   # Partido Comunista do Brasil
  "70" = "PTdoB"    # Partido Trabalhista do Brasil
)

partidos2004 <- c(
  "11" = "PP",     # Progressistas                     ─ PP 11
  "12" = "PDT",    # Partido Democrático Trabalhista   ─ PDT 12
  "13" = "PT",     # Partido dos Trabalhadores         ─ PT 13
  "14" = "PTB",    # Partido Trabalhista Brasileiro    ─ PTB 14
  "15" = "PMDB",   # Movimento Democrático Brasileiro  ─ PMDB 15
  "16" = "PSTU",   # Partido Socialista dos Trabalh.   ─ PSTU 16
  "17" = "PSL",    # Partido Social Liberal            ─ PSL 17
  "19" = "PTN",    # Partido Trabalhista Nacional      ─ PTN 19
  "20" = "PSC",    # Partido Social Cristão            ─ PSC 20
  "21" = "PCB",    # Partido Comunista Brasileiro      ─ PCB 21
  "22" = "PL",     # Partido Liberal                   ─ PL 22
  "23" = "PPS",    # Partido Popular Socialista        ─ PPS 23
  "25" = "PFL",    # Partido da Frente Liberal         ─ PFL 25
  "26" = "PAN",    # Partido dos Aposentados da Nação  ─ PAN 26
  "27" = "PSDC",   # Partido Social Democrata Cristão  ─ PSDC 27
  "28" = "PRTB",   # Partido Renovador Trab. Bras.     ─ PRTB 28
  "29" = "PCO",    # Partido da Causa Operária         ─ PCO 29
  "31" = "PHS",    # Partido Humanista da Solidariedade─ PHS 31
  "33" = "PMN",    # Partido da Mobilização Nacional   ─ PMN 33
  "36" = "PTC",    # Partido Trabalhista Cristão       ─ PTC 36
  "40" = "PSB",    # Partido Socialista Brasileiro     ─ PSB 40
  "43" = "PV",     # Partido Verde                     ─ PV 43
  "44" = "PRP",    # Partido Republicano Progressista  ─ PRP 44
  "45" = "PSDB",   # Partido da Social Democracia Bras ─ PSDB 45
  "56" = "PRONA",  # Partido de Reedificação da Ordem  ─ PRONA 56
  "65" = "PCdoB",  # Partido Comunista do Brasil       ─ PCdoB 65
  "70" = "PTdoB"   # Partido Trabalhista do Brasil     ─ PTdoB 70
)

partidos2000 <- c(
  "11" = "PPB",    # Partido Progressista Brasileiro
  "12" = "PDT",    # Partido Democrático Trabalhista
  "13" = "PT",     # Partido dos Trabalhadores
  "14" = "PTB",    # Partido Trabalhista Brasileiro
  "15" = "PMDB",   # Movimento Democrático Brasileiro
  "16" = "PSTU",   # Partido Socialista dos Trabalhadores Unificado
  "17" = "PSL",    # Partido Social Liberal
  "18" = "PST",    # Partido Social Trabalhista
  "19" = "PTN",    # Partido Trabalhista Nacional
  "20" = "PSC",    # Partido Social Cristão
  "21" = "PCB",    # Partido Comunista Brasileiro
  "22" = "PL",     # Partido Liberal
  "23" = "PPS",    # Partido Popular Socialista
  "25" = "PFL",    # Partido da Frente Liberal
  "26" = "PAN",    # Partido dos Aposentados da Nação
  "27" = "PSDC",   # Partido Social Democrata Cristão
  "28" = "PRTB",   # Partido Renovador Trabalhista Brasileiro
  "29" = "PCO",    # Partido da Causa Operária
  "30" = "PGT",    # Partido Geral dos Trabalhadores
  "31" = "PHS",    # Partido Humanista da Solidariedade (ex-PSN)
  "33" = "PMN",    # Partido da Mobilização Nacional
  "36" = "PRN",    # Partido da Reconstrução Nacional (mudaria para PTC em 2000/2001)
  "40" = "PSB",    # Partido Socialista Brasileiro
  "41" = "PSD",    # Partido Social Democrático (1987-2003)
  "43" = "PV",     # Partido Verde
  "44" = "PRP",    # Partido Republicano Progressista
  "45" = "PSDB",   # Partido da Social Democracia Brasileira
  "56" = "PRONA",  # Partido de Reedificação da Ordem Nacional
  "65" = "PCdoB",  # Partido Comunista do Brasil
  "70" = "PTdoB"   # Partido Trabalhista do Brasil
)

# nomes mais comuns - pelo menos 10 repetidos em deputados federais
common_second <- c(
  "abrao",     "adilson",   "afonso",    "alberto",   "alexandre", "ana",
  "anderson",  "angela",    "angelo",    "antonio",   "arthur",    "augusta",
  "augusto",   "beatriz",   "bernardo",  "bruno",     "carlos",    "carolina",
  "cecilia",   "cesar",     "cezar",     "christina", "claudia",   "claudio",
  "cristiano", "cristina",  "daniel",    "daniela",   "danilo",    "david",
  "diego",     "edilson",   "edmundo",   "eduardo",   "egidio",    "elias",
  "emilia",    "emilio",    "fatima",    "fausto",    "felipe",    "fernanda",
  "fernando",  "flavia",    "flavio",    "francisco", "franco",    "gabriel",
  "gilberto",  "gregorio",  "guilherme", "gustavo",   "helena",    "henrique",
  "hugo",      "inacio",    "izabel",    "jeferson",  "jesus",     "joao",
  "joaquim",   "jorge",     "jose",      "juliano",   "lasaro",    "lazaro",
  "leandro",   "leonardo",  "lucas",     "luis",      "luisa",     "luiz",
  "luiza",     "manoel",    "manuel",    "marcela",   "marcelo",   "marcia",
  "marcos",    "maria",     "mariana",   "mariano",   "mario",     "miguel",
  "moises",    "murilo",    "napoleao",  "nelson",    "oscar",     "oswaldo",
  "otavio",    "paula",     "paulo",     "pedro",     "rafael",    "rafaela",
  "raimundo",  "regina",    "ricardo",   "roberta",   "roberto",   "rodrigo",
  "rogerio",   "ruy",       "salvador",  "sergio",    "silvio",    "thomas",
  "thomaz",    "tomas",     "tomaz",     "vicente",   "victor",    "vinicius",
  "wagner",    "welington", "wilson",    "xavier", 
  
  "rui",       "edith",     "celso",     "julio",     "julia",     "helio",
  "lourdes",   "renan",     "fabio",     "aparecida"
) %>%
  stri_trans_general("Latin-ASCII") %>% 
  tolower()

common_surnames <- c(
  "almeida",    "alves",      "araujo",     "barros",     "cardoso",  
  "carvalho",   "costa",      "dias",       "fereira",    "gomes",    
  "lima",       "martins",    "melo",       "moreira",    "nascimento",
  "oliveira",   "pereira",    "pinto",      "ribeiro",    "rocha",    
  "rodrigues",  "santos",     "silva",      "souza",      "teixeira",
  "sousa",      "pareira"
) %>%
  stri_trans_general("Latin-ASCII") %>% 
  tolower()

# parameter for image
# font_import() # rodar apenas uma vez
# loadfonts(device = "pdf")
base_font <- ifelse("CMU Serif" %in% fonts(), "CMU Serif", "serif")

plot_event_study <- function(model,
                             prefix    = "TREATED",
                             base_year = 2009,
                             title     = NULL) {
  
  # 1) extrai todos os termos do modelo
  df_coef <- broom::tidy(model)
  
  # 2) filtra só aqueles que começam com o prefixo + 4 dígitos
  pattern <- paste0("^", prefix, "(\\d{4})$")
  df_plot <- df_coef %>%
    filter(str_detect(term, pattern)) %>%
    mutate(
      # extrai o ano: pega os 4 dígitos depois do prefixo
      year       = as.integer(str_extract(term, "(?<=^TREATED)\\d{4}")),
      # converte em p.p. e calcula IC95% já na escala p.p.
      estimate_pp = estimate * 100,
      se_pp       = std.error * 100,
      ci_low_pp   = estimate_pp - 1.96 * se_pp,
      ci_high_pp  = estimate_pp + 1.96 * se_pp
    )
  
  # checagem básica
  if (nrow(df_plot) == 0) {
    stop("Não foram encontrados termos '", prefix, "####' no modelo. Verifique o nome das suas dummies.")
  }
  if (is.null(base_year)) {
    base_year <- min(df_plot$year, na.rm = TRUE)
  }
  
  # 3) monta o gráfico
  ggplot(df_plot, aes(x = year, y = estimate_pp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = base_year, linetype = "dashed", color = "red") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_low_pp, ymax = ci_high_pp), width = 0.5) +
    scale_x_continuous(breaks = sort(unique(df_plot$year))) +
    labs(
      title = title,
      x     = "",
      y     = "efeito estimado em p.p."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray85"),
      plot.title         = element_text(face = "bold", hjust = 0.5)
    )
}