# ==========================================================
# Organizador Lattes (Script 1 + Script 2 juntos)
#  - Busca a lista de docentes no site do PPG (fica só na memória do R)
#  - Lê os .html da subpasta 'dados' pelo CONTEÚDO
#  - Renomeia para nome_do_docente_IDLATTES.html
#  - Salva UM csv na pasta atual: docentes_organizados.csv
#
# Se der erro de 'rlang' / 'dplyr' ao carregar os pacotes, rode UMA vez
# (logo após reiniciar o R):
#   install.packages(c("rlang","dplyr","readr","rvest","stringr","stringi"))
# ==========================================================
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(stringi)

# ----------------------------------------------------------
# 1. Trabalha na pasta onde este script .R está salvo
# ----------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  pasta_script <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (pasta_script != "") setwd(pasta_script)
}

url_ppg       <- "https://ppgeas.eeca.ufg.br/p/9452-corpo-docente"
pasta_dados   <- file.path(getwd(), "dados")
caminho_saida <- file.path(getwd(), "docentes_organizados.csv")

if (!dir.exists(pasta_dados)) {
  dir.create(pasta_dados)
  stop("A subpasta 'dados' foi criada. Coloque os arquivos .html dentro dela e rode de novo.")
}

arquivos <- list.files(pasta_dados, pattern = "\\.html?$", full.names = TRUE,
                       ignore.case = TRUE)
if (length(arquivos) == 0) stop("Nenhum arquivo .html encontrado na subpasta 'dados'.")

# ----------------------------------------------------------
# 2. Lista de docentes do PPG (só na memória, não salva csv)
# ----------------------------------------------------------
lista_vazia <- tibble(nome = character(), id_lattes = character(), url_lattes = character())

buscar_lista_docentes <- function(url) {
  pagina <- read_html(url)
  
  linhas <- pagina %>% html_elements("tr:has(a[href*='lattes.cnpq.br'])")
  if (length(linhas) == 0) {
    linhas <- pagina %>%
      html_elements("li:has(a[href*='lattes.cnpq.br']), p:has(a[href*='lattes.cnpq.br'])")
  }
  
  lapply(linhas, function(linha) {
    url_lattes <- linha %>% html_element("a[href*='lattes.cnpq.br']") %>% html_attr("href")
    
    nome_encontrado <- ""
    celulas <- linha %>% html_elements("td")
    if (length(celulas) > 0) {
      textos <- celulas %>% html_text(trim = TRUE)
      candidatos <- textos[!str_detect(textos, "(?i)lattes|curr[ií]culo|acesse|http") &
                             nchar(textos) > 0]
      if (length(candidatos) > 0) nome_encontrado <- candidatos[1]
    }
    if (nome_encontrado == "") {
      negritos <- linha %>% html_elements("strong, b") %>% html_text(trim = TRUE)
      validos  <- negritos[!str_detect(negritos, "(?i)lattes|curr[ií]culo") & nchar(negritos) > 0]
      if (length(validos) > 0) nome_encontrado <- validos[1]
    }
    
    nome_limpo <- nome_encontrado %>% str_replace_all("[\\r\\n\\t]+", " ") %>% str_squish()
    if (nome_limpo == "") nome_limpo <- "Docente Não Identificado"
    
    tibble(nome = nome_limpo, url_lattes = url_lattes)
  }) %>%
    bind_rows() %>%
    mutate(id_lattes = str_extract(url_lattes, "\\d{16}")) %>%
    filter(!is.na(id_lattes)) %>%
    distinct(id_lattes, .keep_all = TRUE) %>%
    select(nome, id_lattes, url_lattes)
}

df_docentes <- tryCatch(
  buscar_lista_docentes(url_ppg),
  error = function(e) {
    warning("Não consegui acessar o site do PPG (", conditionMessage(e),
            "). Sigo sem a lista de docentes: não haverá checagem de faltantes.")
    lista_vazia
  }
)
message("Docentes encontrados no site do PPG: ", nrow(df_docentes))

# ----------------------------------------------------------
# 3. Funções auxiliares
# ----------------------------------------------------------
limpar_nome_arquivo <- function(texto) {
  texto %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove_all("^_+|_+$")
}

# Lê o arquivo detectando o encoding (UTF-8 ou Windows-1252)
ler_html_texto <- function(caminho) {
  bruto <- readBin(caminho, "raw", n = file.info(caminho)$size)
  enc   <- if (stri_enc_isutf8(bruto)) "UTF-8" else "windows-1252"
  stri_encode(bruto, from = enc, to = "UTF-8")
}

extrair_id <- function(txt) {
  m <- str_match(txt, "(?is)ID\\s*Lattes.{0,300}?(?<!\\d)(\\d{16})(?!\\d)")[, 2]
  if (is.na(m)) m <- str_match(txt, "(?i)lattes\\.cnpq\\.br/(\\d{16})")[, 2]
  m
}

extrair_nome <- function(txt) {
  doc <- tryCatch(read_html(charToRaw(enc2utf8(txt)), encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  
  nome <- doc %>% html_element("h2.nome") %>% html_text(trim = TRUE)
  
  if (is.na(nome) || nome == "") {
    h2s  <- doc %>% html_elements("h2") %>% html_text(trim = TRUE)
    h2s  <- h2s[nchar(h2s) > 0 & !str_detect(h2s, "(?i)c[oó]digo de seguran")]
    nome <- if (length(h2s) > 0) h2s[1] else NA_character_
  }
  if (!is.na(nome)) nome <- str_squish(nome)
  nome
}

eh_captcha <- function(txt) {
  str_detect(txt, "(?i)tituloCaptcha|grecaptcha|g-recaptcha")
}

# ----------------------------------------------------------
# 4. Lê TODOS os arquivos pelo conteúdo (o nome atual é ignorado)
# ----------------------------------------------------------
message("Lendo ", length(arquivos), " arquivos HTML...")

info <- lapply(arquivos, function(caminho) {
  txt  <- ler_html_texto(caminho)
  id   <- extrair_id(txt)
  nome <- extrair_nome(txt)
  
  # Se o currículo não trouxe o nome, completa pela lista do PPG usando o ID
  if (is.na(nome) && !is.na(id)) {
    achado <- df_docentes$nome[df_docentes$id_lattes == id]
    if (length(achado) > 0) nome <- achado[1]
  }
  
  status <- if (!is.na(id) && !is.na(nome)) {
    "OK"
  } else if (eh_captcha(txt) && is.na(id)) {
    "PAGINA_CAPTCHA (sem curriculo dentro)"
  } else {
    "NOME_OU_ID_NAO_ENCONTRADO"
  }
  
  tibble(arquivo_original = basename(caminho), caminho_antigo = caminho,
         nome = nome, id_lattes = id, status = status)
}) %>% bind_rows()

# ----------------------------------------------------------
# 5. Monta os novos nomes: nome_do_docente_IDLATTES.html
# ----------------------------------------------------------
info <- info %>%
  mutate(base_nova = ifelse(status == "OK",
                            paste0(limpar_nome_arquivo(nome), "_", id_lattes),
                            NA_character_)) %>%
  group_by(base_nova) %>%
  mutate(
    # mesmo docente baixado 2x: o segundo vira ..._2, o terceiro ..._3
    sufixo = ifelse(!is.na(base_nova) & row_number() > 1, paste0("_", row_number()), ""),
    arquivo_novo = ifelse(is.na(base_nova), NA_character_,
                          paste0(base_nova, sufixo, ".html"))
  ) %>%
  ungroup() %>%
  select(-base_nova, -sufixo) %>%
  left_join(select(df_docentes, id_lattes, url_lattes), by = "id_lattes")

# Log ANTES de renomear (mapa "nome antigo -> nome novo", caso precise desfazer)
write_excel_csv(select(info, -caminho_antigo), caminho_saida)

# ----------------------------------------------------------
# 6. Renomeia (nunca sobrescreve nada)
# ----------------------------------------------------------
info$renomeado <- FALSE
for (i in seq_len(nrow(info))) {
  if (is.na(info$arquivo_novo[i])) next
  destino <- file.path(pasta_dados, info$arquivo_novo[i])
  
  if (normalizePath(info$caminho_antigo[i], winslash = "/", mustWork = FALSE) ==
      normalizePath(destino, winslash = "/", mustWork = FALSE)) {
    info$renomeado[i] <- TRUE      # já está com o nome certo
    next
  }
  if (file.exists(destino)) {
    info$status[i] <- "DESTINO_JA_EXISTE (nao renomeado)"
    next
  }
  info$renomeado[i] <- file.rename(info$caminho_antigo[i], destino)
}
info$status[info$status == "OK" & !info$renomeado] <- "ERRO_AO_RENOMEAR"

# ----------------------------------------------------------
# 7. Junta com quem ainda FALTA baixar e salva o CSV final
# ----------------------------------------------------------
ids_ok <- info$id_lattes[info$status == "OK" & !is.na(info$id_lattes)]

faltantes <- df_docentes %>%
  filter(!(id_lattes %in% ids_ok)) %>%
  transmute(nome, id_lattes,
            arquivo_original = NA_character_,
            arquivo_novo     = NA_character_,
            url_lattes,
            status = "FALTA_BAIXAR")

resultado <- bind_rows(
  select(info, nome, id_lattes, arquivo_original, arquivo_novo, url_lattes, status),
  faltantes
) %>% arrange(status != "OK", nome)

write_excel_csv(resultado, caminho_saida)

# ----------------------------------------------------------
# 8. Relatório no console
# ----------------------------------------------------------
cat("\n==========================================\n")
cat("          RELATÓRIO DE CHECAGEM           \n")
cat("==========================================\n")
cat(sprintf("Arquivos lidos:                 %d\n", nrow(info)))
cat(sprintf("Currículos identificados (OK):  %d\n", sum(info$status == "OK")))
cat(sprintf("Páginas de captcha (inúteis):   %d\n", sum(str_detect(info$status, "CAPTCHA"))))
cat(sprintf("Outros problemas:               %d\n",
            sum(info$status != "OK" & !str_detect(info$status, "CAPTCHA"))))
if (nrow(df_docentes) > 0) {
  cat(sprintf("Docentes no site do PPG:        %d\n", nrow(df_docentes)))
  cat(sprintf("AINDA FALTAM baixar:            %d\n", nrow(faltantes)))
  if (nrow(faltantes) > 0) {
    cat("\nDocentes sem currículo válido ainda:\n")
    print(faltantes %>% select(nome, id_lattes, url_lattes))
  } else {
    cat("SUCESSO: todos os docentes da lista foram baixados!\n")
  }
}
if (any(str_detect(info$status, "CAPTCHA"))) {
  cat("\nATENÇÃO: alguns arquivos são a página do 'Código de segurança' (captcha),\n")
  cat("e não o currículo. Eles NÃO foram renomeados. Baixe de novo depois de\n")
  cat("resolver o captcha, salvando como 'Página da Web, completa'.\n")
}
cat("\nCSV salvo em:", caminho_saida, "\n")

