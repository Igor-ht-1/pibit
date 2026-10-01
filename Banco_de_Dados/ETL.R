#!/usr/bin/env Rscript
# =============================================================================
# ETL Lattes (HTML salvo do navegador) -> PESQUISADORES / ATIVIDADES /
#                                          PARTICIPACAO / VINCULOS
#
# Uso:
#   1) Coloque os .html dos currículos em ./lattes/
#   2) install.packages(c("xml2","stringi","digest","DBI","RSQLite"))
#   3) Rscript etl_lattes.R
#
# Saídas (em ./saida/): pesquisa.sqlite, CSVs das 4 tabelas e carga.sql
#
# Convenções adotadas (ajuste na seção 1 se necessário):
#   - PARTICIPACAO = autoria/inventoria (artigo, livro, capítulo, congresso,
#                    patente, texto, apresentação), com "ordem" do autor.
#   - VINCULOS     = relações acadêmicas: projetos (coordenador/integrante),
#                    orientações (orientador/coorientador/orientando) e
#                    bancas (membro/candidato). "tipo" = tipo da atividade.
#   - IDs: pesquisadores dos CVs = P006, P007...; externos = X+hash;
#          atividades = A+hash (determinísticos: reexecutar não duplica).
# =============================================================================

suppressPackageStartupMessages({
  library(xml2); library(stringi); library(digest); library(DBI)
})

# ----------------------------------------------------------------------------
# 1. CONFIGURAÇÃO
# ----------------------------------------------------------------------------
DIR_LATTES       <- "C:/Users/Aluno/Desktop/pibit-main/pibit-main/dados"
DIR_SAIDA        <- "C:/Users/Aluno/Desktop/pibit-main/pibit-main"
GRAVAR_SQLITE    <- TRUE            # grava também em SQLite (requer RSQLite)
ARQ_SQLITE       <- file.path(DIR_SAIDA, "pesquisa.sqlite")
INCLUIR_EXTERNOS <- FALSE           # TRUE = coautores/orientandos viram pesquisadores tipo 'Externo'
P_INICIO         <- 6L              # primeiro id P### (P001-P005 são os exemplos)
PESO_PADRAO      <- 1.0
PESOS            <- c(ARTIGO = 1.0) # ex.: c(ARTIGO = 1, CAPITULO = 0.5, CONGRESSO_RESUMO = 0.2)

# Metadados que o Lattes não traz (chave = ID Lattes). Ajuste livremente.
META <- list(
  "3638597075849446" = list(tipo = "Docente", grupo = "GeoBioTec/UBI"),
  "3433455726987770" = list(tipo = "Docente", grupo = "EEC/UFG"),
  "1316502250729632" = list(tipo = "Docente", grupo = "EEC/UFG")
)

DDL <- c(
  "CREATE TABLE IF NOT EXISTS PESQUISADORES (
  id_pesquisador VARCHAR(10) PRIMARY KEY, nome VARCHAR(150) NOT NULL,
  tipo VARCHAR(50) NOT NULL, linha_pesquisa VARCHAR(100), grupo VARCHAR(100),
  instituicao VARCHAR(100), area_capes VARCHAR(100), orcid VARCHAR(30),
  periodo_vinculo_inicio DATE, periodo_vinculo_fim DATE)",
  "CREATE TABLE IF NOT EXISTS ATIVIDADES (
  id_atividade VARCHAR(10) PRIMARY KEY, tipo VARCHAR(50) NOT NULL,
  titulo VARCHAR(255) NOT NULL, ano INT NOT NULL, veiculo VARCHAR(150),
  doi VARCHAR(100), peso FLOAT DEFAULT 1.0)",
  "CREATE TABLE IF NOT EXISTS PARTICIPACAO (
  id_atividade VARCHAR(10) NOT NULL, id_pesquisador VARCHAR(10) NOT NULL,
  papel VARCHAR(50) NOT NULL, ordem INT,
  PRIMARY KEY (id_atividade, id_pesquisador),
  FOREIGN KEY (id_atividade) REFERENCES ATIVIDADES(id_atividade) ON DELETE CASCADE,
  FOREIGN KEY (id_pesquisador) REFERENCES PESQUISADORES(id_pesquisador) ON DELETE CASCADE)",
  "CREATE TABLE IF NOT EXISTS VINCULOS (
  id_atividade VARCHAR(10) NOT NULL, id_pesquisador VARCHAR(10) NOT NULL,
  papel VARCHAR(50) NOT NULL, tipo VARCHAR(50) NOT NULL,
  PRIMARY KEY (id_atividade, id_pesquisador, papel),
  FOREIGN KEY (id_atividade) REFERENCES ATIVIDADES(id_atividade) ON DELETE CASCADE,
  FOREIGN KEY (id_pesquisador) REFERENCES PESQUISADORES(id_pesquisador) ON DELETE CASCADE)"
)

# ----------------------------------------------------------------------------
# 2. UTILITÁRIOS
# ----------------------------------------------------------------------------
vazio <- function(x) is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))
`%||%` <- function(a, b) if (vazio(a)) b else a
limpa  <- function(x) trimws(gsub("[\\s\u00a0]+", " ", x, perl = TRUE))
ascii  <- function(x) stri_trans_general(x, "Latin-ASCII")

# Chave de nome: sem acento, maiúsculas, tokens ordenados.
# Faz "ALBUQUERQUE, ANTONIO" == "Antonio Albuquerque" == "A. ALBUQUERQUE"(iniciais casam com iniciais).
norm_nome <- function(x) {
  x <- toupper(ascii(as.character(x)))
  x <- gsub("[^A-Z0-9]+", " ", x)
  vapply(strsplit(trimws(x), " +"),
         function(t) paste(sort(t[nzchar(t)]), collapse = " "), character(1))
}
norm_titulo <- function(x) substr(gsub("[^a-z0-9]", "", tolower(ascii(x))), 1, 150)
norm_doi <- function(x) {
  if (vazio(x)) return(NA_character_)
  tolower(sub("^https?://(dx\\.)?doi\\.org/", "", trimws(x)))
}
hash8 <- function(x) substr(digest(x, algo = "md5", serialize = FALSE), 1, 8)
peso_de <- function(tipo) { p <- PESOS[tipo]; if (length(p) == 0 || is.na(p)) PESO_PADRAO else unname(p) }

# O HTML do Lattes vem com codificação MISTA (bytes windows-1252 + trechos UTF-8).
# Decodifica: mantém sequências UTF-8 válidas e converte os bytes restantes de cp1252.
carrega_html <- function(path) {
  r <- readBin(path, "raw", file.size(path))
  r <- r[r != as.raw(0)]
  txt <- iconv(rawToChar(r), "UTF-8", "UTF-8", sub = "byte")
  m <- gregexpr("<[0-9a-f]{2}>", txt)
  if (m[[1]][1] != -1) {
    regmatches(txt, m) <- lapply(regmatches(txt, m), function(v)
      vapply(v, function(h) stri_conv(as.raw(strtoi(substr(h, 2, 3), 16L)),
                                      "windows-1252", "UTF-8"),
             character(1), USE.NAMES = FALSE))
  }
  txt <- sub("<meta http-equiv=\"Content-Type\"[^>]*>", "", txt)
  tmp <- tempfile(fileext = ".html")
  writeBin(charToRaw(enc2utf8(txt)), tmp)
  read_html(tmp, encoding = "UTF-8")
}

# ----------------------------------------------------------------------------
# 3. DADOS DO PESQUISADOR (dono do CV)
# ----------------------------------------------------------------------------
instituicao_vinculo <- function(doc) {
  sec <- xml_find_first(doc, "//a[@name='AtuacaoProfissional']/ancestor::div[contains(@class,'title-wrapper')][1]")
  vazioR <- list(inst = NA_character_, ini = NA_character_, fim = NA_character_)
  if (inherits(sec, "xml_missing")) return(vazioR)
  nos <- xml_find_all(sec, paste0(
    ".//div[contains(@class,'inst_back')] | ",
    ".//div[contains(@class,'layout-cell-pad-5')][contains(.,'nculo:')]"))
  inst <- NA_character_; linhas <- list()
  for (i in seq_along(nos)) {
    n <- nos[[i]]
    if (grepl("inst_back", xml_attr(n, "class"))) {
      inst <- limpa(xml_text(n))
    } else {
      per <- limpa(xml_text(xml_find_first(n, "../preceding-sibling::div[1]")))
      linhas[[length(linhas) + 1]] <- data.frame(inst = inst, per = per,
                                                 txt = limpa(xml_text(n)),
                                                 stringsAsFactors = FALSE)
    }
  }
  if (!length(linhas)) return(vazioR)
  d <- do.call(rbind, linhas)
  cand <- which(grepl("Atual", d$per) & grepl("Servidor|Celetista|Professor", d$txt))
  alvo <- d$inst[if (length(cand)) cand[1] else 1]
  s <- d[d$inst == alvo, ]
  ini <- suppressWarnings(min(as.integer(sub("^\\D*(\\d{4}).*$", "\\1", s$per)), na.rm = TRUE))
  fim <- if (any(grepl("Atual", s$per))) NA_character_ else {
    f <- suppressWarnings(max(as.integer(sub("^.*-\\s*(\\d{4}).*$", "\\1", s$per)), na.rm = TRUE))
    if (is.finite(f)) sprintf("%d-12-31", f) else NA_character_ }
  list(inst = sub(",[^,]*,[^,]*$", "", alvo),
       ini  = if (is.finite(ini)) sprintf("%d-01-01", ini) else NA_character_,
       fim  = fim)
}

info_pesquisador <- function(doc, id) {
  nome <- limpa(xml_text(xml_find_first(doc, "//h2[@class='nome']")))
  li   <- paste(xml_text(xml_find_all(doc, "//ul[@class='informacoes-autor']/li")), collapse = " ")
  lid  <- regmatches(li, regexpr("[0-9]{16}", li))
  orc  <- xml_attr(xml_find_first(doc, "//a[contains(@href,'orcid.org')]"), "href")
  orc  <- if (is.na(orc)) NA_character_ else sub("^.*orcid.org/", "", orc)
  
  cel  <- xml_find_first(doc, "//div[contains(@class,'layout-cell-3')][contains(.,'bibliogr')]/following-sibling::div[1]")
  variantes <- c(nome, if (!inherits(cel, "xml_missing")) strsplit(xml_text(cel), ";")[[1]])
  keys <- unique(norm_nome(limpa(variantes))); keys <- keys[nzchar(keys)]
  
  lp <- limpa(xml_text(xml_find_all(doc,
                                    "//a[@name='LinhaPesquisa']/ancestor::div[contains(@class,'title-wrapper')][1]//div[contains(@class,'layout-cell-9')]/div")))
  ar <- limpa(xml_text(xml_find_all(doc,
                                    "//a[@name='AreasAtuacao']/ancestor::div[contains(@class,'title-wrapper')][1]//div[contains(@class,'layout-cell-9')]/div")))
  area <- NA_character_; sub_area <- NA_character_
  if (length(ar)) {
    a <- regmatches(ar[1], regexec("/\\s*.rea:\\s*([^/.]+)", ar[1]))[[1]]
    if (length(a) > 1) area <- limpa(a[2])
    s <- regmatches(ar[1], regexec("Sub.rea:\\s*([^/.]+)", ar[1]))[[1]]
    if (length(s) > 1) sub_area <- limpa(s[2])
  }
  linha <- (if (length(lp)) lp[1] else NA_character_) %||% sub_area
  iv <- instituicao_vinculo(doc)
  meta <- META[[lid %||% ""]] %||% list()
  list(id = id, nome = nome, lattes = lid, keys = keys,
       tipo = meta$tipo %||% "Docente", grupo = meta$grupo %||% NA_character_,
       linha = substr(linha, 1, 100), inst = substr(iv$inst, 1, 100),
       area = substr(area, 1, 100), orcid = orc, ini = iv$ini, fim = iv$fim)
}

# ----------------------------------------------------------------------------
# 4. PARSERS DE REGISTROS
# ----------------------------------------------------------------------------
# Separa "AUTORES . Resto" (o Lattes usa ' . ' ou '..' depois do último autor)
split_head <- function(txt, bold = character()) {
  if (length(bold) == 1 && nzchar(bold) && startsWith(txt, bold) &&
      substr(txt, nchar(bold) + 1, nchar(bold) + 2) == ". ")
    return(list(auth = bold, rest = trimws(substr(txt, nchar(bold) + 3, nchar(txt)))))
  m <- regexpr("\\s\\.\\s|\\.\\.\\s", txt)
  if (m < 0) return(list(auth = "", rest = txt))
  ml <- attr(m, "match.length")
  cut_auth <- if (grepl("^\\s", substr(txt, m, m))) m - 1 else m
  list(auth = substr(txt, 1, cut_auth), rest = trimws(substr(txt, m + ml, nchar(txt))))
}

titulo_e_resto <- function(rest) {
  m <- regexpr("\\.\\s+In:\\s", rest)
  if (m > 0) return(list(t = substr(rest, 1, m - 1), r = trimws(substr(rest, m + 1, nchar(rest)))))
  m <- regexpr("(?<!\\bDr)(?<!\\bProf)(?<!\\bSr)\\.\\s", rest, perl = TRUE)
  if (m > 0) list(t = substr(rest, 1, m - 1),
                  r = trimws(substr(rest, m + attr(m, "match.length"), nchar(rest))))
  else list(t = sub("\\.$", "", rest), r = "")
}

ano_de <- function(x) {
  for (p in c(",\\s*((?:19|20)\\d{2})\\b", "\\b((?:19|20)\\d{2})\\b")) {
    m <- regmatches(x, regexec(p, x, perl = TRUE))[[1]]
    if (length(m) > 1) return(as.integer(m[2]))
  }
  NA_integer_
}

veiculo_prod <- function(tipo, r) {
  r <- trimws(r)
  if (grepl("^CONGRESSO", tipo)) {
    m <- regmatches(r, regexec("In:\\s*(.*?),\\s*(?:19|20)\\d{2}", r, perl = TRUE))[[1]]
    return(if (length(m) > 1) limpa(m[2]) else NA_character_)
  }
  if (tipo == "CAPITULO") {
    x <- sub("^In:.*?\\(Org\\.\\)\\.\\s*", "", r, perl = TRUE)
    x <- sub("\\.\\s*\\d+\\s*ed.*$", "", x)
    return(if (nzchar(x)) limpa(x) else NA_character_)
  }
  if (tipo == "LIVRO") {
    m <- regmatches(r, regexec(":\\s*([^,:]+),\\s*(?:19|20)\\d{2}", r, perl = TRUE))[[1]]
    return(if (length(m) > 1) limpa(m[2]) else NA_character_)
  }
  if (tipo == "TEXTO_JORNAL") return(limpa(sub(",.*$", "", r)))
  NA_character_
}

inst_final <- function(txt) {
  m <- regmatches(txt, regexpr("\\s-\\s[^-]+$", txt))
  if (!length(m)) return(NA_character_)
  limpa(sub("[,.(].*$", "", sub("^\\s-\\s", "", m)))
}

autores_de <- function(a) sub("\\.+$", "", limpa(unlist(strsplit(a, ";", fixed = TRUE))))

# data.frame de participantes; o dono do CV é sempre incluído
membros_de <- function(nomes, papel, owner, extra_keys = character(), ordem = NULL) {
  nomes <- limpa(nomes); nomes <- nomes[nzchar(nomes)]
  keys  <- norm_nome(nomes)
  dono  <- keys %in% c(owner$keys, extra_keys[nzchar(extra_keys)])
  ordem <- if (is.null(ordem)) seq_along(nomes) else rep(ordem, length.out = length(nomes))
  papel <- rep(papel, length.out = length(nomes))
  if (!any(dono)) {
    nomes <- c(nomes, owner$nome); dono <- c(dono, TRUE)
    ordem <- c(ordem, NA); papel <- c(papel, if (length(papel)) papel[1] else "Autor")
  }
  data.frame(nome = nomes, papel = papel, ordem = as.integer(ordem),
             dono = dono, stringsAsFactors = FALSE)
}

reg <- function(tipo, titulo, ano, veiculo, doi, extra, membros)
  list(tipo = tipo, titulo = limpa(titulo), ano = as.integer(ano),
       veiculo = veiculo %||% NA_character_, doi = doi %||% NA_character_,
       extra = extra %||% "", membros = membros)

# --- Artigos (usa o atributo cvuri, que traz título/periódico/DOI limpos) ---
parse_artigo <- function(nd, owner) {
  span <- xml_find_first(nd, ".//span[@class='transform']")
  cv   <- xml_attr(xml_find_first(span, ".//span[contains(@class,'citado')]"), "cvuri")
  ano  <- as.integer(xml_text(xml_find_first(span, "./span[@data-tipo-ordenacao='ano']")))
  href <- xml_attr(xml_find_first(span, ".//a[contains(@class,'icone-doi')]"), "href")
  bold <- limpa(xml_text(xml_find_all(span, "./b")))
  xml_remove(xml_find_all(span, paste0(".//sup | .//span[contains(@class,'citado')] | ",
                                       ".//span[contains(@class,'informacao-artigo')] | .//a[contains(@class,'icone-producao')]")))
  txt <- limpa(xml_text(span))
  h <- split_head(txt, bold)
  pick <- function(p) {
    if (is.na(cv)) return(NA_character_)
    m <- regmatches(cv, regexec(p, cv, perl = TRUE))[[1]]
    if (length(m) > 1) m[2] else NA_character_
  }
  titulo <- pick("titulo=(.*?)&sequencial=") %||% titulo_e_resto(h$rest)$t
  veic   <- pick("nomePeriodico=(.*)$")
  doi    <- norm_doi(pick("doi=(.*?)&issn=") %||% href)
  reg("ARTIGO", titulo, ano, veic, doi, "",
      membros_de(autores_de(h$auth), "Autor", owner, norm_nome(bold)))
}

# --- Livros, capítulos, textos, congressos, apresentações ---
parse_prod <- function(nd, tipo, owner) {
  span <- xml_find_first(nd, ".//span[@class='transform']")
  bold <- limpa(xml_text(xml_find_all(span, "./b")))
  txt  <- limpa(xml_text(span))
  h  <- split_head(txt, bold)
  tr <- titulo_e_resto(h$rest)
  ano <- ano_de(tr$r) %||% ano_de(txt)
  reg(tipo, tr$t, ano, veiculo_prod(tipo, tr$r), NA, "",
      membros_de(autores_de(h$auth), "Autor", owner, norm_nome(bold)))
}

# --- Patentes ---
parse_patente <- function(nd, owner) {
  span <- xml_find_first(nd, ".//span[@class='transform']")
  bold <- limpa(xml_text(xml_find_all(span, "./b")))
  txt  <- limpa(xml_text(span))
  h <- split_head(txt, bold)
  t <- regmatches(txt, regexec("t.tulo:\\s*[\"\u201c\u201d]([^\"\u201c\u201d]+)[\"\u201c\u201d]", txt))[[1]]
  titulo <- if (length(t) > 1) t[2] else titulo_e_resto(h$rest)$t
  d <- regmatches(txt, regexec("Dep.sito:\\s*\\d{2}/\\d{2}/(\\d{4})", txt))[[1]]
  ano <- if (length(d) > 1) as.integer(d[2]) else ano_de(h$rest)
  v <- regmatches(txt, regexec("Institui.{1,4}de registro:\\s*([^.]+)\\.", txt))[[1]]
  reg("PATENTE", titulo, ano, if (length(v) > 1) limpa(v[2]) else NA, NA, "",
      membros_de(autores_de(h$auth), "Inventor", owner, norm_nome(bold)))
}

# --- Projetos (VINCULOS) ---
parse_projeto <- function(a, sec, owner) {
  sib <- function(i) xml_text(xml_find_first(a, sprintf("following-sibling::div[%d]", i)))
  ano <- as.integer(regmatches(sib(1), regexpr("\\d{4}", sib(1))))
  titulo <- limpa(sib(2)); det <- limpa(sib(4))
  if (!grepl("Integrantes:", det)) return(NULL)
  integ <- sub("^.*Integrantes:\\s*", "", det)
  integ <- sub("\\.?\\s*Financiador\\(es\\):.*$", "", integ)
  integ <- sub("\\.\\s*$", "", integ)
  itens <- trimws(strsplit(integ, "\\s+/\\s+")[[1]])
  nomes <- sub("\\s+-\\s+[^-]+$", "", itens)
  papel <- ifelse(grepl("\\s-\\s", itens), sub("^.*\\s+-\\s+", "", itens), "Integrante")
  fin <- NA_character_
  if (grepl("Financiador\\(es\\):", det)) {
    fin <- sub("^.*Financiador\\(es\\):\\s*", "", det)
    fin <- limpa(sub("\\.\\s*$", "", sub("\\s+-\\s+[^-]+$", "", fin)))
  }
  tipo <- if (sec == "PROJ_EXT" || grepl("Natureza: Extens", det)) "PROJETO_EXTENSAO" else "PROJETO_PESQUISA"
  m <- membros_de(nomes, papel, owner); m$ordem <- NA_integer_
  reg(tipo, titulo, ano, fin, NA, "", m)
}

# --- Orientações (VINCULOS) ---
ori_tipo <- function(h) {
  h <- tolower(ascii(h))
  if (grepl("pos-doutorado", h)) "ORIENTACAO_POS_DOC"
  else if (grepl("monografia|aperfeicoamento|especializacao", h)) "ORIENTACAO_ESPECIALIZACAO"
  else if (grepl("mestrado", h)) "ORIENTACAO_MESTRADO"
  else if (grepl("doutorado", h)) "ORIENTACAO_DOUTORADO"
  else if (grepl("iniciacao", h)) "ORIENTACAO_IC"
  else if (grepl("conclusao de curso|graduacao", h)) "ORIENTACAO_TCC"
  else "ORIENTACAO_OUTRA"
}

parse_orientacao <- function(nd, sub, status, owner) {
  span <- xml_find_first(nd, ".//span[@class='transform']")
  txt  <- limpa(xml_text(span))
  papel <- if (grepl("Coorientador", txt)) "Coorientador" else "Orientador"
  m <- if (grepl("In.cio:\\s*(19|20)\\d{2}", txt))
    regmatches(txt, regexec("^(.+?)\\.\\s+(?:(.+?)\\s+)?In.cio:\\s*((?:19|20)\\d{2})", txt, perl = TRUE))[[1]]
  else
    regmatches(txt, regexec("^(.+?)\\.\\s+(?:(.+?)\\.?\\s+)?((?:19|20)\\d{2})\\.\\s", txt, perl = TRUE))[[1]]
  if (length(m) < 4) return(NULL)
  orientandos <- unlist(strsplit(m[2], "\\s*;\\s*|\\s+e\\s+"))
  titulo <- sub("\\.+$", "", m[3])
  if (!nzchar(titulo)) titulo <- paste("Orientacao de", m[2])
  mem <- rbind(
    data.frame(nome = owner$nome, papel = papel, ordem = NA_integer_, dono = TRUE, stringsAsFactors = FALSE),
    data.frame(nome = orientandos, papel = "Orientando", ordem = NA_integer_, dono = FALSE, stringsAsFactors = FALSE))
  reg(ori_tipo(sub), titulo, m[4], inst_final(txt), NA, norm_nome(m[2]), mem)
}

# --- Bancas (VINCULOS) ---
banca_tipo <- function(h) {
  h <- tolower(ascii(h))
  if (grepl("qualificacao", h)) "BANCA_QUALIFICACAO"
  else if (grepl("doutorado", h)) "BANCA_DOUTORADO"
  else if (grepl("mestrado", h)) "BANCA_MESTRADO"
  else if (grepl("monografia|aperfeicoamento|especializacao", h)) "BANCA_ESPECIALIZACAO"
  else if (grepl("graduacao|conclusao", h)) "BANCA_TCC"
  else "BANCA_OUTRA"
}

parse_banca <- function(nd, sub, owner) {
  span <- xml_find_first(nd, ".//span[@class='transform']")
  bold <- limpa(xml_text(xml_find_all(span, "./b")))
  txt  <- limpa(xml_text(span))
  h <- regexpr("Participa.{2,4}\\s+em banca de\\s+", txt)
  if (h < 0) return(NULL)
  cab   <- substr(txt, 1, h - 1)
  corpo <- substr(txt, h + attr(h, "match.length"), nchar(txt))
  m <- regmatches(corpo, regexec("^(.+?)\\.\\s*(.+?)\\.?\\s+((?:19|20)\\d{2})\\.\\s", corpo, perl = TRUE))[[1]]
  if (length(m) < 4) return(NULL)
  membros <- unlist(strsplit(sub("\\.+\\s*$", "", cab), ";"))
  cand <- unlist(strsplit(m[2], "\\s*;\\s*|\\s+e\\s+"))
  mem <- rbind(
    membros_de(membros, "Membro de banca", owner, norm_nome(bold), ordem = NA),
    data.frame(nome = cand, papel = "Candidato", ordem = NA_integer_, dono = FALSE, stringsAsFactors = FALSE))
  reg(banca_tipo(sub), sub("\\.+$", "", m[3]), m[4], inst_final(corpo), NA, norm_nome(m[2]), mem)
}

# --- Cabeçalhos de seção de produção ---
tipo_prod <- function(h) {
  h <- tolower(ascii(h))
  if (grepl("artigos completos", h)) "ARTIGO"
  else if (grepl("livros publicados", h)) "LIVRO"
  else if (grepl("capitulos de livros", h)) "CAPITULO"
  else if (grepl("textos em jornais", h)) "TEXTO_JORNAL"
  else if (grepl("resumos expandidos", h)) "CONGRESSO_RESUMO_EXP"
  else if (grepl("resumos publicados", h)) "CONGRESSO_RESUMO"
  else if (grepl("trabalhos completos", h)) "CONGRESSO_COMPLETO"
  else if (grepl("apresentacoes de trabalho", h)) "APRESENTACAO"
  else if (grepl("outras producoes", h)) "OUTRA_PRODUCAO"
  else NA_character_
}

SEC <- c(ProjetosPesquisa = "PROJ", ProjetosExtensao = "PROJ_EXT",
         ProducoesCientificas = "PROD", ProducaoBibliografica = "PROD",
         PatentesRegistros = "PAT", Orientacoes = "ORI",
         Bancas = "BANCA", ParticipacaoBancasTrabalho = "BANCA",
         # seções ignoradas:
         OutrosProjetos = "SKIP", PotencialInovacao = "SKIP", Eventos = "SKIP",
         ProducaoTecnica = "SKIP", ParticipacaoBancasComissoes = "SKIP",
         Identificacao = "SKIP", Endereco = "SKIP", FormacaoAcademicaTitulacao = "SKIP",
         FormacaoAcademicaPosDoutorado = "SKIP", FormacaoComplementar = "SKIP",
         AtuacaoProfissional = "SKIP", LinhaPesquisa = "SKIP", MembroCorpoEditorial = "SKIP",
         MembroComiteAssessoramento = "SKIP", RevisorPeriodico = "SKIP",
         RevisorProjetoFomento = "SKIP", AreasAtuacao = "SKIP", Idiomas = "SKIP",
         PremiosTitulos = "SKIP")

# Varre o documento em ordem, mantendo o "estado" (seção / subtítulo / andamento)
extrair_registros <- function(doc, owner) {
  regs <- list(); falhas <- 0L
  add  <- function(r) if (!is.null(r)) { r$owner_id <- owner$id; regs[[length(regs) + 1]] <<- r }
  safe <- function(expr) tryCatch(expr, error = function(e) { falhas <<- falhas + 1L; NULL })
  sel <- paste(
    "//a[@name]",
    "//div[contains(@class,'cita-artigos')]",
    "//div[@class='artigo-completo']",
    "//div[contains(@class,'layout-cell-11')][.//span[@class='transform']][not(ancestor::div[@class='artigo-completo'])]",
    sep = " | ")
  nodes <- xml_find_all(doc, sel)
  sec <- "SKIP"; status <- ""; sub <- ""
  for (i in seq_along(nodes)) {
    nd <- nodes[[i]]
    if (xml_name(nd) == "a") {
      n <- xml_attr(nd, "name")
      if (n %in% names(SEC))              { sec <- SEC[[n]]; sub <- "" }
      else if (n == "Orientacaoemandamento") { status <- "andamento"; sub <- "" }
      else if (n == "Orientacoesconcluidas") { status <- "concluida"; sub <- "" }
      else if (grepl("^PP_", n) && sec %in% c("PROJ", "PROJ_EXT")) add(safe(parse_projeto(nd, sec, owner)))
      next
    }
    cls <- xml_attr(nd, "class")
    if (grepl("cita-artigos", cls)) { sub <- limpa(xml_text(nd)); next }
    if (sec == "PROD" && cls == "artigo-completo") { add(safe(parse_artigo(nd, owner))); next }
    if (grepl("layout-cell-11", cls)) {
      if (sec == "PROD") {
        tp <- tipo_prod(sub)
        if (!is.na(tp)) add(safe(parse_prod(nd, tp, owner)))
      } else if (sec == "PAT")   add(safe(parse_patente(nd, owner)))
      else if (sec == "ORI")     add(safe(parse_orientacao(nd, sub, status, owner)))
      else if (sec == "BANCA")   add(safe(parse_banca(nd, sub, owner)))
    }
  }
  if (falhas) message(sprintf("  [%s] %d entradas com erro de parsing (ignoradas)", owner$nome, falhas))
  regs
}

# ----------------------------------------------------------------------------
# 5. MONTAGEM DAS TABELAS (deduplicação entre CVs + resolução de pessoas)
# ----------------------------------------------------------------------------
chave_reg <- function(r)
  paste(r$tipo, if (!vazio(r$doi)) r$doi else norm_titulo(r$titulo), r$extra %||% "", sep = "|")

montar_tabelas <- function(owners, regs) {
  dict <- character()
  for (o in owners) dict[o$keys] <- o$id
  ext <- list()
  resolver <- function(nome) {
    k <- norm_nome(nome)
    if (!nzchar(k)) return(NA_character_)
    if (k %in% names(dict)) return(unname(dict[k]))
    if (!INCLUIR_EXTERNOS) return(NA_character_)
    id <- paste0("X", hash8(k))
    if (is.null(ext[[id]])) ext[[id]] <<- limpa(nome)
    id
  }
  
  ativ <- list(); part <- list(); vinc <- list(); vistos <- character(); descartados <- 0L
  for (r in regs) {
    if (vazio(r$titulo) || is.na(r$ano)) { descartados <- descartados + 1L; next }
    aid <- paste0("A", hash8(chave_reg(r)))
    if (!aid %in% vistos) {
      vistos <- c(vistos, aid)
      ativ[[length(ativ) + 1]] <- data.frame(
        id_atividade = aid, tipo = r$tipo, titulo = substr(r$titulo, 1, 255),
        ano = r$ano, veiculo = substr(r$veiculo, 1, 150), doi = substr(r$doi, 1, 100),
        peso = peso_de(r$tipo), stringsAsFactors = FALSE)
    }
    para_vinculos <- grepl("^(PROJETO|ORIENTACAO|BANCA)", r$tipo)
    m <- r$membros
    for (j in seq_len(nrow(m))) {
      pid <- if (m$dono[j]) r$owner_id else resolver(m$nome[j])
      if (is.na(pid)) next
      if (para_vinculos)
        vinc[[length(vinc) + 1]] <- data.frame(id_atividade = aid, id_pesquisador = pid,
                                               papel = m$papel[j], tipo = r$tipo, stringsAsFactors = FALSE)
      else
        part[[length(part) + 1]] <- data.frame(id_atividade = aid, id_pesquisador = pid,
                                               papel = m$papel[j], ordem = m$ordem[j], stringsAsFactors = FALSE)
    }
  }
  bind <- function(l, cols) if (length(l)) do.call(rbind, l) else
    setNames(data.frame(matrix(character(), 0, length(cols)), stringsAsFactors = FALSE), cols)
  
  pesq <- do.call(rbind, lapply(owners, function(o) data.frame(
    id_pesquisador = o$id, nome = substr(o$nome, 1, 150), tipo = o$tipo,
    linha_pesquisa = o$linha, grupo = o$grupo, instituicao = o$inst,
    area_capes = o$area, orcid = o$orcid,
    periodo_vinculo_inicio = o$ini, periodo_vinculo_fim = o$fim, stringsAsFactors = FALSE)))
  if (length(ext)) pesq <- rbind(pesq, data.frame(
    id_pesquisador = names(ext), nome = substr(unlist(ext), 1, 150), tipo = "Externo",
    linha_pesquisa = NA, grupo = NA, instituicao = NA, area_capes = NA, orcid = NA,
    periodo_vinculo_inicio = NA, periodo_vinculo_fim = NA, stringsAsFactors = FALSE))
  
  part <- bind(part, c("id_atividade", "id_pesquisador", "papel", "ordem"))
  vinc <- bind(vinc, c("id_atividade", "id_pesquisador", "papel", "tipo"))
  list(
    PESQUISADORES = pesq,
    ATIVIDADES    = bind(ativ, c("id_atividade", "tipo", "titulo", "ano", "veiculo", "doi", "peso")),
    PARTICIPACAO  = part[!duplicated(part[c("id_atividade", "id_pesquisador")]), ],
    VINCULOS      = vinc[!duplicated(vinc[c("id_atividade", "id_pesquisador", "papel")]), ],
    descartados   = descartados)
}

# ----------------------------------------------------------------------------
# 6. CARGA (CSV, SQL, SQLite)
# ----------------------------------------------------------------------------
lit <- function(x) {
  if (is.numeric(x)) ifelse(is.na(x), "NULL", format(x, scientific = FALSE, trim = TRUE))
  else as.character(dbQuoteString(ANSI(), as.character(x)))
}
gerar_insert <- function(tab, df, lote = 200) {
  if (!nrow(df)) return(character())
  linhas <- paste0("(", do.call(paste, c(lapply(df, lit), sep = ", ")), ")")
  idx <- split(seq_along(linhas), ceiling(seq_along(linhas) / lote))
  vapply(idx, function(i) sprintf("INSERT INTO %s (%s) VALUES\n%s;\n",
                                  tab, paste(names(df), collapse = ", "), paste(linhas[i], collapse = ",\n")), "")
}
anexar <- function(con, tab, df, chave) {
  ex <- dbGetQuery(con, sprintf("SELECT %s FROM %s", paste(chave, collapse = ", "), tab))
  if (nrow(ex) && nrow(df)) {
    k_ex  <- do.call(paste, c(ex, sep = "\r"))
    k_new <- do.call(paste, c(df[chave], sep = "\r"))
    df <- df[!k_new %in% k_ex, , drop = FALSE]
  }
  if (nrow(df)) dbAppendTable(con, tab, df)
  nrow(df)
}

# ----------------------------------------------------------------------------
# 7. EXECUÇÃO
# ----------------------------------------------------------------------------
arquivos <- sort(list.files(DIR_LATTES, pattern = "\\.html?$", full.names = TRUE, ignore.case = TRUE))
if (!length(arquivos)) stop("Nenhum .html encontrado em '", DIR_LATTES, "/'")
dir.create(DIR_SAIDA, showWarnings = FALSE, recursive = TRUE)

message("Lendo ", length(arquivos), " currículo(s)...")
docs   <- lapply(arquivos, carrega_html)
owners <- lapply(seq_along(docs), function(i)
  info_pesquisador(docs[[i]], sprintf("P%03d", P_INICIO + i - 1L)))
regs   <- unlist(lapply(seq_along(docs), function(i) {
  message("  extraindo: ", owners[[i]]$nome)
  extrair_registros(docs[[i]], owners[[i]])
}), recursive = FALSE)

tab <- montar_tabelas(owners, regs)
if (tab$descartados) message(tab$descartados, " registros descartados (sem título ou ano).")

for (t in c("PESQUISADORES", "ATIVIDADES", "PARTICIPACAO", "VINCULOS"))
  write.csv(tab[[t]], file.path(DIR_SAIDA, paste0(t, ".csv")),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

con_sql <- file(file.path(DIR_SAIDA, "carga.sql"), "w", encoding = "UTF-8")
writeLines(c(DDL, ""), con_sql, sep = ";\n")
for (t in c("PESQUISADORES", "ATIVIDADES", "PARTICIPACAO", "VINCULOS"))
  writeLines(gerar_insert(t, tab[[t]]), con_sql, sep = "\n")
close(con_sql)

if (GRAVAR_SQLITE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) stop("Instale o RSQLite ou use GRAVAR_SQLITE <- FALSE")
  con <- dbConnect(RSQLite::SQLite(), ARQ_SQLITE)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  for (q in DDL) dbExecute(con, q)
  n <- c(
    PESQUISADORES = anexar(con, "PESQUISADORES", tab$PESQUISADORES, "id_pesquisador"),
    ATIVIDADES    = anexar(con, "ATIVIDADES",    tab$ATIVIDADES,    "id_atividade"),
    PARTICIPACAO  = anexar(con, "PARTICIPACAO",  tab$PARTICIPACAO,  c("id_atividade", "id_pesquisador")),
    VINCULOS      = anexar(con, "VINCULOS",      tab$VINCULOS,      c("id_atividade", "id_pesquisador", "papel")))
  message("\nLinhas novas gravadas no SQLite:"); print(n)
}

message("\nAtividades por tipo:"); print(table(tab$ATIVIDADES$tipo))
message("Pronto. Arquivos em ./", DIR_SAIDA, "/")

