library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(sidrar)
library(rvest)
library(httr)

file.remove(list.files(pattern = "bancovde-202.*\\.xlsx$"))

url <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/dados-nacionais-1/base-de-dados-e-notas-metodologicas-dos-gestores-estaduais-sinesp-vde-2022-e-2023"

page <- read_html(url)

links <- page %>% html_nodes("a") %>% html_attr("href")

#xlsx_links <- links[grepl("\\.xlsx", links)] #todos os xlsx
xlsx_links <- links[grep("bancovde-202", links)]


# Função para baixar arquivos e salvar com nome original com tentativas de reintento
download_xlsx <- function(url, attempts = 5) {
  # Extrair o nome do arquivo da URL
  file_name <- sub(".*/(bancovde-\\d{4}\\-1.xlsx)/.*", "\\1", url) ## antes não tinha esse -1 antes do xlsx no nome do arquivo original
  
  success <- FALSE
  attempt <- 1
  
  while (!success && attempt <= attempts) {
    tryCatch({
      download.file(url, destfile = file_name, mode = "wb")
      message("Downloaded: ", file_name)
      success <- TRUE
    }, error = function(e) {
      message("Failed to download: ", url, "\nError: ", e)
      attempt <- attempt + 1
      if (attempt <= attempts) {
        message("Retrying... Attempt ", attempt, " of ", attempts)
      }
    })
  }
  
  if (!success) {
    message("All attempts to download ", url, " have failed.")
  }
}

# Loop para baixar cada arquivo
for (link in xlsx_links) {
  download_xlsx(link)
}



arquivos <- list.files(pattern = "bancovde-202.*\\.xlsx$", full.names = TRUE)

# Função para ler um arquivo Excel
ler_excel <- function(caminho) {
  read_excel(caminho, 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric"))
}

serie_violencia <- do.call(rbind, lapply(arquivos, ler_excel))


file.remove(list.files(pattern = "\\.xlsx$"))


ajuste <-read.csv2('data/municipios.csv')



populacao_sexo <- get_sidra(x = 9514,
                            variable = 93,
                            period = c("last" = 1),
                            geo = "City",
                            classific = c("C2"),
                            geo.filter = list("State" = 28))



populacao_sexo <- populacao_sexo |> 
  select(c(7,13, 5)) |> 
  mutate(Município = str_remove(Município, " - SE")) |> 
  mutate(Município = case_when(Município  == 'Gracho Cardoso' ~ 'Graccho Cardoso',
                               Município  == "Itaporanga d'Ajuda" ~ "Itaporanga D'Ajuda",
                               TRUE ~ Município)) |> 
  rename(População = Valor) |> 
  mutate(
    Sexo = case_when(Sexo  == 'Homens' ~ 'Feminino',
                     Sexo  == 'Mulheres' ~ 'Masculino',
                     TRUE ~ Sexo))


sergipe_mun <- serie_violencia |> 
  filter(uf == 'SE' & municipio!='NÃO INFORMADO') |> 
  left_join(ajuste, by= "municipio") |> 
  select(-municipio) |> 
  relocate(Município)

violencia_mensal_sexo <- sergipe_mun |> 
  filter(evento != 'Mandado de prisão cumprido') |> 
  mutate(Mês = ymd(data_referencia)) |> 
  group_by(Município, evento, Mês) |> 
  summarise(
    feminino = sum(feminino, na.rm = TRUE),
    masculino = sum(masculino, na.rm = TRUE),
    nao_informado = sum(nao_informado, na.rm = TRUE),
    Total = sum(total_vitimas, na.rm = TRUE),
    .groups = 'drop') |> 
  rename(Feminino = feminino, Masculino = masculino, `Não informado` = nao_informado) |> 
  pivot_longer(cols = c(Feminino, Masculino, `Não informado`, Total),
               names_to = "Sexo",
               values_to = "Valor") |> 
  filter(Valor != 0) |> 
  rename(Variável = evento) |> 
  left_join(populacao_sexo, by=c('Município', 'Sexo')) |> 
  drop_na() |> 
  mutate(`Taxa por 100 mil hab` = round((Valor / População) * 100000, 2)) |> 
  group_by(Variável, Mês, Sexo) |> 
  arrange(desc(`Taxa por 100 mil hab`)) |> 
  mutate(`Ranking taxa por 100 mil hab` = min_rank(desc(`Taxa por 100 mil hab`))) |>
  arrange(Mês) |> 
  ungroup() |> 
  arrange(Município, Variável, Mês)


saveRDS(violencia_mensal_sexo, "data/violencia_mensal.rds")
