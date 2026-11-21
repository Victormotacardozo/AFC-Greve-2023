pacotes <- c("FactoMineR", "ggplot2", "ggrepel", "readxl", "dplyr", "tidyr",
             "forcats", "stringr", "scales", "writexl")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) {
  install.packages(faltando, repos = "https://cloud.r-project.org", quiet = TRUE)
}
suppressPackageStartupMessages(invisible(lapply(pacotes, library, character.only = TRUE)))
cat("Pacotes carregados:", paste(pacotes, collapse = ", "), "\n")

# ---- Arquivo --------------------------------------------------------------
ARQUIVO     <- "Matriz_ACM.xlsx"   # Colab: suba o arquivo no painel "Arquivos" (à esquerda)
ABA         <- "Página1"
PASTA_SAIDA <- "saida_acm"         # figuras (PNG) e tabelas (xlsx) são salvas aqui
DPI         <- 300                 # resolução das figuras salvas

# ---- Regra 1.1 do codebook (baixa frequência) -----------------------------
LIM_PCT  <- 0.05   # categoria com menos de 5% das observações...
LIM_ABS  <- 5      # ...ou com menos de 5 observações é "de baixa frequência"
CRITERIO <- "max"  # "max": vale o limiar MAIS ALTO dos dois (5% de 180 = 9 obs.) -> leitura "mais restritivo"
                   # "min": vale o limiar mais baixo (5 obs.). Rode os dois e compare.

# ---- "Não se aplica" nas variáveis ativas ---------------------------------
# "passiva_se_rara": se tiver menos obs. que o limiar, a modalidade fica PASSIVA (ACM específica:
#                    não entra no cálculo dos eixos, mas as unidades continuam na análise)
# "passiva_sempre" : toda modalidade "Não se aplica" fica passiva
# "ativa_sempre"   : "Não se aplica" é tratada como qualquer outra modalidade
NSA_TRATAMENTO <- "passiva_se_rara"

# ---- Variáveis ativas -----------------------------------------------------
# Por padrão: Fichas 15, 16, 17, 18, 19 e 24 do codebook v2.
# Para rebaixar uma variável a suplementar (Seção 1.1, passo 4), liste o código. Ex.: c("TL")
REBAIXAR <- character(0)

# ---- ACM e gráficos -------------------------------------------------------
N_EIXOS    <- 10                   # eixos retidos no cálculo
PLANOS     <- list(c(1, 2), c(1, 3))  # planos fatoriais a desenhar
PCT_EIXO   <- "bruta"              # "bruta" (como no script-base) ou "modificada" (Benzécri) nos rótulos dos eixos
MIN_N_SUP  <- 5                    # modalidades suplementares com menos obs. não são desenhadas
VTEST_CRIT <- 1.96                 # |v-test| >= 1,96 ~ p < 0,05

# ---- Módulos opcionais ----------------------------------------------------
RODAR_HCPC          <- TRUE        # tipologia de enquadramentos (bloco 10)
N_CLUSTERS          <- NULL        # NULL = número sugerido automaticamente; ou um inteiro (ex.: 4)
RODAR_SENSIBILIDADE <- TRUE        # bloco 11
SEMENTE             <- 2026

dir.create(PASTA_SAIDA, showWarnings = FALSE)
set.seed(SEMENTE)

dic <- tribble(
  ~cod,      ~nome,                              ~ficha, ~papel, ~familia,                  ~pref,
  "EMISSOR", "Emissor",                               3, "sup",  "Identificação do emissor", "EM_",
  "POLO",    "Polo da Disputa",                       5, "sup",  "Identificação do emissor", "POLO_",
  "TE",      "Tipo de Emissor",                       4, "sup",  "Identificação do emissor", "TE_",
  "GC",      "Gênero Comunicativo",                  10, "sup",  "Documento e foco",         "GC_",
  "TF",      "Tipo de Foco",                         14, "sup",  "Documento e foco",         "TF_",
  "TR",      "Tipo de Reivindicação",                20, "sup",  "Documento e foco",         "TR_",
  "PA",      "Público Alvo",                         21, "sup",  "Documento e foco",         "PA_",
  "DC",      "Destinatário da Crítica",              25, "sup",  "Documento e foco",         "DC_",
  "MG",      "Momento da Greve",                     11, "sup",  "Contexto do conflito",     "MG_",
  "ED",      "Evento Desencadeador",                 12, "sup",  "Contexto do conflito",     "ED_",
  "JOP",     "Janela de Oportunidade Política",      26, "sup",  "Contexto do conflito",     "JOP_",
  "RR",      "Reação da Reitoria",                   27, "sup",  "Contexto do conflito",     "RR_",
  "TC",      "Tipo de Chamada",                      23, "sup",  "Ação coletiva",            "TC_",
  "AC",      "Ampliação do Conflito",                28, "sup",  "Ação coletiva",            "AC_",
  "AOA",     "Apoio de Outros Atores",               29, "sup",  "Ação coletiva",            "AOA_",
  "IC",      "Intensidade do Confronto",             30, "sup",  "Ação coletiva",            "IC_",
  "RAC",     "Repertório de Ação Coletiva",          31, "sup",  "Ação coletiva",            "RAC_",
  "PD",      "Problema Definido (diagnóstico)",      15, "ativa","Enquadramento (ativas)",   "PD_",
  "RA",      "Responsabilidade Atribuída (diagnóstico)", 16, "ativa","Enquadramento (ativas)", "RA_",
  "P",       "Prognóstico",                          17, "ativa","Enquadramento (ativas)",   "P_",
  "M",       "Motivação",                            18, "ativa","Enquadramento (ativas)",   "M_",
  "TL",      "Tipo de Legitimidade",                 19, "ativa","Enquadramento (ativas)",   "TL_",
  "TD",      "Tom Predominante do Discurso",         24, "ativa","Enquadramento (ativas)",   "TD_"
)
# Rebaixamentos pedidos pelo usuário (Seção 1.1, passo 4)
dic$papel[dic$cod %in% REBAIXAR] <- "sup"
dic$familia[dic$cod %in% REBAIXAR] <- "Documento e foco"

vars_ativas <- dic$cod[dic$papel == "ativa"]
vars_sup    <- dic$cod[dic$papel == "sup"]
vars_todas  <- c(vars_ativas, vars_sup)
nome_de     <- setNames(dic$nome, dic$cod)

# ---- Paletas (Okabe-Ito para as ativas; uma cor por família para as suplementares) ----
pal_ativas <- c(PD = "#0072B2", RA = "#D55E00", P = "#009E73",
                M = "#CC79A7", TL = "#E69F00", TD = "#56B4E9")
pal_ativas <- pal_ativas[names(pal_ativas) %in% vars_ativas]
pal_fam <- c("Identificação do emissor" = "#7B2D8E", "Documento e foco" = "#2A9D8F",
             "Contexto do conflito"     = "#E76F51", "Ação coletiva"    = "#264653")
pal_polo <- c("Movimento estudantil" = "#C1121F", "Administração universitária" = "#003049")
pal_emissor <- c(DCE = "#C1121F", Correnteza = "#F77F00", Juntos = "#6A994E",
                 UJC = "#7B2CBF", Reitoria = "#003049", Instituto = "#4EA8DE")

# ---- Tema e funções auxiliares ----
options(encoding = "UTF-8", dplyr.summarise.inform = FALSE,
        repr.plot.width = 12, repr.plot.height = 8.5, repr.plot.res = 110)

tema_acm <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(plot.title    = element_text(face = "bold", size = base + 5, hjust = 0, margin = margin(b = 4)),
          plot.subtitle = element_text(colour = "grey30", size = base, hjust = 0, margin = margin(b = 10)),
          plot.caption  = element_text(colour = "grey45", size = base - 3, hjust = 0),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title = element_text(face = "italic", colour = "grey25"),
          legend.position = "bottom", legend.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = base),
          plot.background = element_rect(fill = "white", colour = NA))
}
pct_br <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d, decimal.mark = ","), "%")
salvar <- function(p, nome, w = 12, h = 8.5) {
  ggsave(file.path(PASTA_SAIDA, paste0(nome, ".png")), p, width = w, height = h, dpi = DPI, bg = "white")
  print(p); invisible(p)
}
rot <- function(mod) sub("_", " · ", mod)   # "RA_Reitoria" -> "RA · Reitoria"
FONTE <- "Fonte: Matriz_ACM.xlsx (N = 180 publicações) · codebook v2 · elaboração própria"

# ---- Leitura da matriz (por posição, porque o nome "PD" está duplicado) ----
if (!file.exists(ARQUIVO)) {
  stop("Arquivo '", ARQUIVO, "' não encontrado em ", getwd(),
       ".\n- Colab: abra o painel 'Arquivos' (esquerda) e faça upload.\n- RStudio: use setwd() ou coloque o arquivo na pasta do projeto.")
}
raw <- read_excel(ARQUIVO, sheet = ABA, .name_repair = "minimal")

cab_esperada <- c("ID","Emissor","PD","TE","GC","MG","ED","TF","PD","RA","P","M","TL","TR",
                  "PA","TC","TD","DC","JOP","RR","AC","AOA","IC","RAC")
cab <- str_squish(names(raw))
if (!identical(cab, cab_esperada)) {
  stop("As colunas da matriz não batem com o esperado.\nEncontrado: ", paste(cab, collapse = ", "),
       "\nEsperado:   ", paste(cab_esperada, collapse = ", "))
}
names(raw) <- c("ID","EMISSOR","POLO","TE","GC","MG","ED","TF","PD","RA","P","M","TL","TR",
                "PA","TC","TD","DC","JOP","RR","AC","AOA","IC","RAC")

dados <- raw %>%
  mutate(ID = as.character(ID)) %>%
  mutate(across(-ID, ~ str_squish(as.character(.x))))

# ---- Checagens (a regra do codebook é: nunca deixar célula em branco) ----
n_vazias <- colSums(is.na(dados) | dados == "")
cat("Unidades de análise:", nrow(dados), "| IDs duplicados:", sum(duplicated(dados$ID)), "\n")
cat("Células vazias:", sum(n_vazias), "\n")
if (sum(n_vazias) > 0) print(n_vazias[n_vazias > 0])

# Consistência Emissor -> Tipo de Emissor -> Polo (Fichas 3, 4 e 5: variáveis derivadas)
mapa_te   <- c(Reitoria = "Administração central", `Pró-Reitoria` = "Órgão administrativo",
               Instituto = "Unidade acadêmica", DCE = "Representação estudantil",
               Correnteza = "Força política estudantil", Juntos = "Força política estudantil",
               UJC = "Força política estudantil", `Jornal da USP` = "Veículo jornalístico", Outro = "Outro")
mapa_polo <- c(Reitoria = "Administração universitária", `Pró-Reitoria` = "Administração universitária",
               Instituto = "Administração universitária", DCE = "Movimento estudantil",
               Correnteza = "Movimento estudantil", Juntos = "Movimento estudantil",
               UJC = "Movimento estudantil", `Jornal da USP` = "Externo", Outro = "Externo")
incons <- dados %>%
  filter(tolower(TE)   != tolower(mapa_te[EMISSOR]) |
         tolower(POLO) != tolower(mapa_polo[EMISSOR])) %>% select(ID, EMISSOR, TE, POLO)
if (nrow(incons) == 0) {
  cat("Consistência Emissor/Tipo de Emissor/Polo: OK\n")
} else { cat("ATENÇÃO: linhas inconsistentes com as Fichas 3-5:\n"); print(incons) }

cat("\nComposição do corpus por Polo e Emissor:\n")
print(dados %>% count(POLO, EMISSOR) %>% arrange(POLO, desc(n)))

N_ref  <- nrow(dados)
limiar <- if (CRITERIO == "max") max(LIM_PCT * N_ref, LIM_ABS) else min(LIM_PCT * N_ref, LIM_ABS)
cat(sprintf("Limiar de baixa frequência: n < %.0f observações (critério '%s')\n", limiar, CRITERIO))

trata_var <- function(x, cod) {
  tb <- as.data.frame(table(x), stringsAsFactors = FALSE); names(tb) <- c("cat", "n")
  tb <- tb %>% mutate(
    pct  = 100 * n / sum(n),
    tipo = case_when(grepl("^Outr[oa]$", cat) ~ "outro", cat == "Não se aplica" ~ "nsa", TRUE ~ "regular"),
    acao = ifelse(tipo == "regular" & n < limiar, "agrupada", "mantida"))
  nome_outro <- if (any(tb$tipo == "outro")) tb$cat[tb$tipo == "outro"] else if (cod == "TL") "Outra" else "Outro"
  x_novo <- ifelse(x %in% tb$cat[tb$acao == "agrupada"], nome_outro, x)
  n_outro <- sum(x_novo == nome_outro)
  list(x = x_novo, nome_outro = nome_outro, n_outro = n_outro,
       outro_baixo = n_outro > 0 && n_outro < limiar,
       log = tb %>% mutate(var = cod, limiar = limiar, destino = ifelse(acao == "agrupada", nome_outro, cat)))
}
trat <- setNames(lapply(vars_ativas, function(v) trata_var(dados[[v]], v)), vars_ativas)

log_freq <- bind_rows(lapply(trat, `[[`, "log")) %>% left_join(dic %>% select(cod, nome), by = c("var" = "cod"))

# ---- Resumo por variável ativa ----
resumo_trat <- bind_rows(lapply(vars_ativas, function(v) {
  t <- trat[[v]]
  tibble(variavel = nome_de[[v]], cod = v,
         modalidades_antes = nrow(t$log),
         modalidades_depois = length(unique(t$x)),
         categorias_agrupadas = paste(t$log$cat[t$log$acao == "agrupada"], collapse = "; "),
         n_agrupado = sum(t$log$n[t$log$acao == "agrupada"]),
         n_outro_final = t$n_outro,
         ALERTA_outro_baixo = t$outro_baixo)
}))
print(as.data.frame(resumo_trat), right = FALSE)

alerta_vars <- vars_ativas[sapply(trat, `[[`, "outro_baixo")]
if (length(alerta_vars) > 0) {
  cat("\n>>> PASSO 4 DA SEÇÃO 1.1: o 'Outro' resultante segue abaixo do limiar em:",
      paste(nome_de[alerta_vars], collapse = ", "),
      "\n    Avalie o rebaixamento a suplementar (parâmetro REBAIXAR) e veja o bloco 11 (sensibilidade).\n")
}

# ---- Figura: frequências ANTES do tratamento ----
plot_freq <- function(tab, titulo, subt) {
  tab <- tab %>% mutate(id = paste(var, cat, sep = "___"))
  tab$id <- fct_reorder(tab$id, tab$n)
  ggplot(tab, aes(n, id, fill = estado)) +
    geom_col(width = .75) +
    geom_text(aes(label = paste0(n, " (", formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%)")),
              hjust = -0.08, size = 3.2, colour = "grey20") +
    geom_vline(xintercept = limiar, linetype = "dashed", colour = "grey30") +
    facet_wrap(~ nome, scales = "free_y", ncol = 2) +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    scale_x_continuous(expand = expansion(mult = c(0, .30))) +
    scale_fill_manual(values = c("Mantida" = "#2A6F97", "Será agrupada em Outro" = "#D62828",
                                 "Outro / Outra" = "#F4A261", "Não se aplica" = "#9E9E9E",
                                 "Outro ainda abaixo do limiar" = "#9D0208"), name = NULL) +
    labs(title = titulo, subtitle = subt, x = "Número de publicações", y = NULL, caption = FONTE) +
    tema_acm(12) + theme(panel.grid.major.y = element_blank())
}
f_antes <- log_freq %>% mutate(estado = case_when(
  acao == "agrupada" ~ "Será agrupada em Outro", tipo == "outro" ~ "Outro / Outra",
  tipo == "nsa" ~ "Não se aplica", TRUE ~ "Mantida"))
p <- plot_freq(f_antes, "Variáveis ativas: frequência das modalidades (antes do tratamento)",
               sprintf("Linha tracejada = limiar de baixa frequência (n < %.0f). Em vermelho: modalidades que serão agrupadas em “Outro”.", limiar))
salvar(p, "01_frequencias_antes", 13, 11)

# ---- Figura: frequências DEPOIS do tratamento ----
f_depois <- bind_rows(lapply(vars_ativas, function(v) {
  t <- as.data.frame(table(trat[[v]]$x), stringsAsFactors = FALSE); names(t) <- c("cat", "n")
  t %>% mutate(var = v, pct = 100 * n / sum(n),
               tipo = case_when(grepl("^Outr[oa]$", cat) ~ "outro", cat == "Não se aplica" ~ "nsa", TRUE ~ "regular"))
})) %>% left_join(dic %>% select(cod, nome), by = c("var" = "cod")) %>%
  mutate(estado = case_when(tipo == "outro" & n < limiar ~ "Outro ainda abaixo do limiar",
                            tipo == "outro" ~ "Outro / Outra", tipo == "nsa" ~ "Não se aplica", TRUE ~ "Mantida"))
p <- plot_freq(f_depois, "Variáveis ativas: frequência das modalidades (depois do tratamento)",
               "Em vinho: “Outro” que continua abaixo do limiar (passo 4 da Seção 1.1: avaliar a permanência da variável como ativa).")
salvar(p, "02_frequencias_depois", 13, 11)

# Passo 6: modalidades ativas x N
K_nominal <- sum(sapply(vars_ativas, function(v) length(unique(trat[[v]]$x))))
cat(sprintf("\nPasso 6: %d variáveis ativas, %d modalidades ativas (antes de passivar 'Não se aplica'), N = %d -> %.2f modalidades por 10 unidades\n",
            length(vars_ativas), K_nominal, N_ref, 10 * K_nominal / N_ref))

ordena_niveis <- function(x, cod) {
  fixa <- list(MG = c("Pré-Greve", "Deflagração", "Escalada", "Negociação", "Encerramento", "Pós-Greve"),
               IC = c("Baixa", "Média", "Alta"),
               EMISSOR = c("DCE", "Correnteza", "Juntos", "UJC", "Reitoria", "Instituto"),
               POLO = c("Movimento estudantil", "Administração universitária"))
  u <- unique(x)
  if (cod %in% names(fixa)) {
    lv <- c(intersect(fixa[[cod]], u), setdiff(u, fixa[[cod]]))
  } else {
    lv <- names(sort(table(x), decreasing = TRUE))
    fim <- intersect(c(lv[grepl("^Outr[oa]$", lv)], "Não se aplica"), lv)
    lv <- c(setdiff(lv, fim), fim)
  }
  factor(x, levels = lv)
}

dados_f  <- dados %>% select(ID, all_of(vars_todas))   # rótulos limpos (para legendas dos gráficos)
base_acm <- dados_f                                    # rótulos com prefixo (para a ACM)
for (v in vars_ativas) { dados_f[[v]] <- trat[[v]]$x; base_acm[[v]] <- trat[[v]]$x }
for (v in vars_todas) {
  f <- ordena_niveis(base_acm[[v]], v)
  dados_f[[v]]  <- f
  pref <- dic$pref[dic$cod == v]
  base_acm[[v]] <- factor(paste0(pref, as.character(f)), levels = paste0(pref, levels(f)))
}

tab_n <- function(vars) bind_rows(lapply(vars, function(v) {
  t <- table(base_acm[[v]]); tibble(mod = names(t), cod = v, n = as.integer(t)) }))

roda_acm <- function(ativas, sup) {
  X <- as.data.frame(base_acm[, c(ativas, sup)]); rownames(X) <- base_acm$ID
  niv <- unlist(lapply(ativas, function(v) levels(X[[v]])))
  nsa <- niv[grepl("Não se aplica$", niv)]
  n_nsa <- sapply(nsa, function(l) sum(as.matrix(X[, ativas, drop = FALSE]) == l))
  passivas <- switch(NSA_TRATAMENTO, passiva_sempre = nsa, passiva_se_rara = nsa[n_nsa < limiar], character(0))
  excl <- which(niv %in% passivas)
  res <- MCA(X, ncp = N_EIXOS, quali.sup = match(sup, names(X)),
             excl = if (length(excl)) excl else NULL, graph = FALSE)
  res$passivas <- passivas
  res
}

res <- roda_acm(vars_ativas, vars_sup)
cat("Modalidades passivas (fora do cálculo dos eixos):", if (length(res$passivas)) paste(res$passivas, collapse = ", ") else "nenhuma", "\n")
cat("Modalidades ativas efetivas (K):", nrow(res$var$coord), "| variáveis ativas (Q):", length(vars_ativas), "\n")

# ---- Tabelas-base reutilizadas em todos os gráficos ----
Q <- length(vars_ativas)
eig <- tibble(dim = seq_len(nrow(res$eig)), ei = res$eig[, 1], pct = res$eig[, 2], pct_ac = res$eig[, 3]) %>%
  mutate(lam_mod = ifelse(ei > 1 / Q, ((Q / (Q - 1)) * (ei - 1 / Q))^2, 0),
         pct_mod = 100 * lam_mod / sum(lam_mod), pct_mod_ac = cumsum(pct_mod))
lab_eixo <- function(d) sprintf("Eixo %d (%s)", d, pct_br(if (PCT_EIXO == "modificada") eig$pct_mod[d] else eig$pct[d]))

nm <- function(m, pre) { m <- as.data.frame(m); names(m) <- paste0(pre, seq_len(ncol(m))); m }
modA <- bind_cols(tibble(mod = rownames(res$var$coord)), nm(res$var$coord, "D"),
                  nm(res$var$contrib, "ctr"), nm(res$var$cos2, "cos")) %>%
  left_join(tab_n(vars_ativas), by = "mod") %>%
  mutate(rotulo = rot(mod), nome_var = nome_de[cod])
modS <- bind_cols(tibble(mod = rownames(res$quali.sup$coord)), nm(res$quali.sup$coord, "D"),
                  nm(res$quali.sup$v.test, "vt"), nm(res$quali.sup$cos2, "cos")) %>%
  left_join(tab_n(vars_sup), by = "mod") %>%
  mutate(rotulo = rot(mod), nome_var = nome_de[cod]) %>%
  left_join(dic %>% select(cod, familia), by = "cod")
ind <- bind_cols(tibble(ID = rownames(res$ind$coord)), nm(res$ind$coord, "D")) %>%
  left_join(dados_f %>% select(ID, all_of(vars_sup)), by = "ID")

cat("\nTaxas de inércia (primeiros 6 eixos):\n")
print(as.data.frame(eig[1:6, c("dim", "ei", "pct", "pct_mod")] %>% mutate(across(-dim, ~ round(.x, 2)))))

n_show <- min(10, nrow(eig))
d <- bind_rows(eig[1:n_show, ] %>% transmute(dim, tipo = "Taxa bruta (FactoMineR)", v = pct),
               eig[1:n_show, ] %>% transmute(dim, tipo = "Taxa modificada (Benzécri)", v = pct_mod)) %>%
  mutate(dest = dim <= 2)
p <- ggplot(d, aes(factor(dim), v, fill = dest)) +
  geom_col(width = .75) +
  geom_text(aes(label = pct_br(v)), vjust = -0.5, size = 3.6, fontface = "bold") +
  facet_wrap(~ tipo, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "#C1121F", "FALSE" = "#9FB7CC"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(title = "Inércia explicada por eixo", subtitle = "Em vermelho: eixos 1 e 2 (plano principal)",
       x = "Eixo", y = "% da inércia", caption = FONTE) + tema_acm()
salvar(p, "03_inercia_eixos", 13, 6)

eta <- bind_rows(
  as.data.frame(res$var$eta2) %>% mutate(cod = rownames(.), papel = "Variáveis ATIVAS (constroem os eixos)"),
  as.data.frame(res$quali.sup$eta2) %>% mutate(cod = rownames(.), papel = "Variáveis SUPLEMENTARES (projetadas)")) %>%
  pivot_longer(starts_with("Dim"), names_to = "eixo", values_to = "eta2") %>%
  mutate(eixo = as.integer(sub("Dim ", "", eixo))) %>% filter(eixo <= 5) %>%
  mutate(nome = nome_de[cod])
ord <- eta %>% filter(eixo %in% 1:2) %>% group_by(papel, nome) %>% summarise(s = sum(eta2)) %>% ungroup()
eta$nome <- factor(eta$nome, levels = ord$nome[order(ord$papel, ord$s)])
p <- ggplot(eta, aes(factor(eixo), nome, fill = eta2)) +
  geom_tile(colour = "white", linewidth = .8) +
  geom_text(aes(label = formatC(eta2, format = "f", digits = 2, decimal.mark = ","),
                colour = eta2 > .45), size = 3.4, fontface = "bold") +
  facet_grid(papel ~ ., scales = "free_y", space = "free_y") +
  scale_fill_gradient(low = "white", high = "#08306B", name = "η²", limits = c(0, 1)) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey20"), guide = "none") +
  labs(title = "Poder de discriminação das variáveis em cada eixo (η²)",
       subtitle = "Quanto mais escura a célula, mais a variável organiza as unidades naquele eixo",
       x = "Eixo", y = NULL, caption = FONTE) + tema_acm(12) + theme(panel.grid = element_blank())
salvar(p, "04_eta2_variaveis", 11, 10)

ctr_plano <- function(ex, ey) (modA[[paste0("ctr", ex)]] * eig$ei[ex] + modA[[paste0("ctr", ey)]] * eig$ei[ey]) / (eig$ei[ex] + eig$ei[ey])

plot_mod <- function(ex = 1, ey = 2, foco = FALSE) {
  K <- nrow(modA)
  d <- modA %>% mutate(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]],
                       ctr_pl = ctr_plano(ex, ey), acima = ctr_pl >= 100 / K)
  dl <- if (foco) filter(d, acima) else d
  ggplot(d, aes(x, y)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = .4) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = .4) +
    geom_point(aes(size = ctr_pl, colour = cod, alpha = if (foco) acima else TRUE)) +
    geom_text_repel(data = dl, aes(label = rotulo, colour = cod), size = 3.7, fontface = "bold",
                    max.overlaps = Inf, box.padding = .35, point.padding = .25,
                    min.segment.length = 0, segment.alpha = .4, seed = SEMENTE, show.legend = FALSE) +
    scale_colour_manual(values = pal_ativas, labels = nome_de[names(pal_ativas)], name = "Variável") +
    scale_size_area(max_size = 13, name = "Contribuição ao plano (%)") +
    scale_alpha_manual(values = c("TRUE" = .9, "FALSE" = .3), guide = "none") +
    coord_fixed() +
    labs(title = if (foco) "Nuvem das modalidades ativas: só as que mais contribuem" else "Nuvem das modalidades ativas",
         subtitle = if (foco) sprintf("Rótulos apenas para contribuição ≥ média (%s) ao plano %d-%d", pct_br(100 / K), ex, ey)
                    else sprintf("Plano %d-%d · tamanho = contribuição ao plano · cor = variável", ex, ey),
         x = lab_eixo(ex), y = lab_eixo(ey), caption = FONTE) +
    tema_acm() + guides(colour = guide_legend(override.aes = list(size = 5), nrow = 2), size = guide_legend(nrow = 1))
}
for (pl in PLANOS) {
  salvar(plot_mod(pl[1], pl[2], FALSE), sprintf("05_modalidades_plano%d%d", pl[1], pl[2]), 13, 9.5)
  salvar(plot_mod(pl[1], pl[2], TRUE),  sprintf("06_modalidades_foco_plano%d%d", pl[1], pl[2]), 13, 9.5)
}

K <- nrow(modA)
cl <- bind_rows(lapply(1:3, function(dd) {
  tibble(eixo = lab_eixo(dd), rotulo = modA$rotulo, cod = modA$cod,
         ctr = modA[[paste0("ctr", dd)]], lado = ifelse(modA[[paste0("D", dd)]] > 0, "(+)", "(−)"))
})) %>% group_by(eixo) %>% slice_max(ctr, n = 12, with_ties = FALSE) %>% ungroup() %>%
  mutate(id = paste(eixo, rotulo, sep = "___"))
cl$id <- fct_reorder(cl$id, cl$ctr)
p <- ggplot(cl, aes(ctr, id, fill = cod)) +
  geom_col(width = .75) +
  geom_text(aes(label = lado), hjust = -0.2, size = 3.8, fontface = "bold") +
  geom_vline(xintercept = 100 / K, linetype = "dashed", colour = "grey25") +
  facet_wrap(~ eixo, scales = "free_y", ncol = 3) +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_fill_manual(values = pal_ativas, labels = nome_de[names(pal_ativas)], name = "Variável") +
  scale_x_continuous(expand = expansion(mult = c(0, .15))) +
  labs(title = "Modalidades que mais contribuem para cada eixo",
       subtitle = sprintf("12 maiores contribuições por eixo · tracejado = contribuição média (%s)", pct_br(100 / K)),
       x = "Contribuição (%)", y = NULL, caption = FONTE) +
  tema_acm(12) + theme(panel.grid.major.y = element_blank()) + guides(fill = guide_legend(nrow = 1))
salvar(p, "07_contribuicoes_eixos", 17, 7.5)

mapa_ind <- function(var, pal, ex = 1, ey = 2, titulo, rotular_ids = TRUE) {
  d <- ind %>% mutate(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]], g = as.character(.data[[var]]),
                      dist = sqrt(x^2 + y^2))
  cen <- d %>% group_by(g) %>% summarise(x = mean(x), y = mean(y), n = n())
  ggplot(d, aes(x, y, colour = g, fill = g)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = .4) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = .4) +
    stat_ellipse(geom = "polygon", level = .8, alpha = .08, linewidth = .9, show.legend = FALSE) +
    geom_point(size = 2.6, alpha = .65) +
    geom_point(data = cen, size = 7, shape = 23, colour = "white", stroke = 1.2) +
    geom_label_repel(data = cen, aes(label = paste0(g, " (n=", n, ")")), colour = "white", fontface = "bold",
                     size = 4, label.size = 0, box.padding = .6, seed = SEMENTE, show.legend = FALSE) +
    { if (rotular_ids) geom_text_repel(data = d %>% group_by(g) %>% slice_max(dist, n = 6, with_ties = FALSE) %>% ungroup(), aes(label = ID), colour = "grey15",
                                       size = 3.2, seed = SEMENTE, max.overlaps = Inf, show.legend = FALSE) } +
    scale_colour_manual(values = pal, name = NULL) + scale_fill_manual(values = pal, name = NULL) +
    coord_fixed() +
    labs(title = titulo, subtitle = sprintf("Plano %d-%d · cada ponto = 1 publicação · elipse = 80%% das unidades do grupo", ex, ey),
         x = lab_eixo(ex), y = lab_eixo(ey), caption = FONTE) +
    tema_acm() + guides(colour = guide_legend(override.aes = list(size = 4, alpha = 1)))
}
for (pl in PLANOS) {
  salvar(mapa_ind("POLO", pal_polo, pl[1], pl[2], "Nuvem das unidades por Polo da Disputa"),
         sprintf("08_unidades_polo_plano%d%d", pl[1], pl[2]), 12, 9)
  salvar(mapa_ind("EMISSOR", pal_emissor, pl[1], pl[2], "Nuvem das unidades por Emissor", rotular_ids = FALSE),
         sprintf("09_unidades_emissor_plano%d%d", pl[1], pl[2]), 12, 9)
}

# ---- Mesma ideia em painéis (small multiples): um painel por grupo, com o corpus todo ao fundo ----
facetas <- function(var, pal, titulo, ncol = 3, ex = 1, ey = 2) {
  d <- ind %>% mutate(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]], g = .data[[var]])
  n_g <- d %>% count(g) %>% mutate(lab = paste0(g, "  (n = ", n, ")"))
  d <- d %>% left_join(n_g, by = "g") %>% mutate(lab = factor(lab, levels = n_g$lab[order(match(n_g$g, levels(d$g)))]))
  fundo <- d %>% select(x, y)
  cen <- d %>% group_by(lab) %>% summarise(x = mean(x), y = mean(y))
  ggplot(d, aes(x, y)) +
    geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
    geom_point(data = fundo, colour = "grey85", size = 1.6) +
    geom_point(aes(colour = g), size = 2.8, alpha = .85, show.legend = FALSE) +
    geom_point(data = cen, shape = 23, size = 5.5, fill = "black", colour = "white", stroke = 1) +
    facet_wrap(~ lab, ncol = ncol) + scale_colour_manual(values = pal) + coord_fixed() +
    labs(title = titulo, subtitle = "Cinza = todas as publicações · cor = publicações do grupo · losango preto = centroide do grupo",
         x = lab_eixo(ex), y = lab_eixo(ey), caption = FONTE) + tema_acm(12)
}
salvar(facetas("EMISSOR", pal_emissor, "Onde cada emissor se posiciona no espaço dos enquadramentos"),
       "10_unidades_emissor_paineis", 14, 9.5)

mapa_sup <- function(ex = 1, ey = 2) {
  s <- modS %>% filter(n >= MIN_N_SUP) %>% mutate(familia = factor(familia, levels = names(pal_fam))) %>%
    mutate(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]],
           sig = abs(.data[[paste0("vt", ex)]]) >= VTEST_CRIT | abs(.data[[paste0("vt", ey)]]) >= VTEST_CRIT)
  fundo <- modA %>% transmute(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]])
  ggplot(s, aes(x, y)) +
    geom_hline(yintercept = 0, colour = "grey75") + geom_vline(xintercept = 0, colour = "grey75") +
    geom_point(data = fundo, colour = "grey82", size = 2) +
    geom_point(data = filter(s, !sig), aes(colour = familia), shape = 1, size = 2.4, stroke = .9, alpha = .7, show.legend = FALSE) +
    geom_point(data = filter(s, sig), aes(colour = familia), shape = 17, size = 4.2) +
    geom_text_repel(data = filter(s, sig), aes(label = rotulo, colour = familia), size = 3.3, fontface = "bold",
                    max.overlaps = Inf, box.padding = .3, min.segment.length = 0, segment.alpha = .35,
                    seed = SEMENTE, show.legend = FALSE) +
    facet_wrap(~ familia, ncol = 2) +
    scale_colour_manual(values = pal_fam, guide = "none") + coord_fixed() +
    labs(title = "Variáveis suplementares sobre o espaço dos enquadramentos",
         subtitle = sprintf("Plano %d-%d · rótulos: |v-test| ≥ %s em pelo menos um dos eixos · n ≥ %d", ex, ey,
                            format(VTEST_CRIT, decimal.mark = ","), MIN_N_SUP),
         x = lab_eixo(ex), y = lab_eixo(ey), caption = FONTE) + tema_acm(12)
}
for (pl in PLANOS) salvar(mapa_sup(pl[1], pl[2]), sprintf("11_suplementares_plano%d%d", pl[1], pl[2]), 17, 14)

# ---- Quais modalidades suplementares mais se associam a cada eixo? (v-test) ----
vt <- bind_rows(lapply(1:3, function(dd) {
  modS %>% filter(n >= MIN_N_SUP) %>%
    transmute(eixo = lab_eixo(dd), rotulo, familia, v = .data[[paste0("vt", dd)]])
})) %>% group_by(eixo) %>% slice_max(abs(v), n = 14, with_ties = FALSE) %>% ungroup() %>%
  mutate(id = paste(eixo, rotulo, sep = "___"))
vt$id <- fct_reorder(vt$id, vt$v)
p <- ggplot(vt, aes(v, id, colour = familia)) +
  geom_vline(xintercept = c(-VTEST_CRIT, VTEST_CRIT), linetype = "dashed", colour = "grey35") +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_segment(aes(x = 0, xend = v, yend = id), linewidth = 1.3) +
  geom_point(size = 3.8) +
  facet_wrap(~ eixo, scales = "free_y", ncol = 3) +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_colour_manual(values = pal_fam, name = "Família da variável") +
  labs(title = "Suplementares mais associadas a cada eixo (teste-valor)",
       subtitle = "Tracejado = ±1,96 (p < 0,05) · sinal indica o lado do eixo em que a modalidade se concentra",
       x = "v-test", y = NULL, caption = FONTE) +
  tema_acm(12) + theme(panel.grid.major.y = element_blank()) + guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))
salvar(p, "12_vtest_suplementares", 17, 7.5)

trajetoria <- function(var = "MG", ex = 1, ey = 2, titulo = "Trajetória do conflito no espaço dos enquadramentos") {
  d <- ind %>% mutate(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]], g = .data[[var]])
  cen <- d %>% group_by(g) %>% summarise(x = mean(x), y = mean(y), n = n()) %>% mutate(ordem = row_number())
  bg  <- modA %>% filter(ctr_plano(ex, ey) >= 100 / nrow(modA)) %>% transmute(x = .data[[paste0("D", ex)]], y = .data[[paste0("D", ey)]], rotulo)
  ggplot() +
    geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
    geom_point(data = d, aes(x, y, colour = g), size = 1.8, alpha = .25) +
    geom_text_repel(data = bg, aes(x, y, label = rotulo), colour = "grey55", size = 3, max.overlaps = Inf,
                    segment.alpha = 0, seed = SEMENTE) +
    geom_path(data = cen, aes(x, y), arrow = arrow(length = unit(.35, "cm"), type = "closed"),
              colour = "grey20", linewidth = 1.1) +
    geom_point(data = cen, aes(x, y, fill = g, size = n), shape = 21, colour = "white", stroke = 1.4) +
    geom_label_repel(data = cen, aes(x, y, label = paste0(ordem, ". ", g, " (n=", n, ")"), colour = g),
                     fontface = "bold", size = 4.2, box.padding = .7, seed = SEMENTE, show.legend = FALSE) +
    scale_fill_viridis_d(option = "plasma", end = .9, guide = "none") +
    scale_colour_viridis_d(option = "plasma", end = .9, guide = "none") +
    scale_size_area(max_size = 12, guide = "none") + coord_fixed() +
    labs(title = titulo, subtitle = sprintf("Plano %d-%d · setas ligam os centroides na ordem do ciclo · pontos claros = unidades", ex, ey),
         x = lab_eixo(ex), y = lab_eixo(ey), caption = FONTE) + tema_acm()
}
for (pl in PLANOS) salvar(trajetoria("MG", pl[1], pl[2]), sprintf("13_trajetoria_momento_plano%d%d", pl[1], pl[2]), 13, 9.5)
salvar(facetas("MG", setNames(scales::viridis_pal(option = "plasma", end = .9)(6), levels(ind$MG)),
               "Cada momento da greve, painel a painel", ncol = 3), "14_momento_paineis", 14, 9.5)

if (RODAR_HCPC) {
  set.seed(SEMENTE)
  hc <- HCPC(res, nb.clust = if (is.null(N_CLUSTERS)) -1 else N_CLUSTERS, graph = FALSE)
  ind$perfil <- factor(paste("Perfil", as.integer(hc$data.clust$clust)))
  k <- nlevels(ind$perfil)
  pal_perfil <- setNames(c("#E63946", "#2A9D8F", "#E9C46A", "#6A4C93", "#457B9D", "#F4A261", "#8D99AE", "#2B9348")[1:k], levels(ind$perfil))
  cat("Número de perfis:", k, "\n"); print(table(ind$perfil))

  # (a) Perfis sobre o plano fatorial
  hull <- ind %>% group_by(perfil) %>% slice(chull(D1, D2))
  cen <- ind %>% group_by(perfil) %>% summarise(D1 = mean(D1), D2 = mean(D2), n = n())
  p <- ggplot(ind, aes(D1, D2, colour = perfil, fill = perfil)) +
    geom_hline(yintercept = 0, colour = "grey70") + geom_vline(xintercept = 0, colour = "grey70") +
    geom_polygon(data = hull, alpha = .12, linewidth = .8) +
    geom_point(size = 2.8, alpha = .8) +
    geom_label_repel(data = cen, aes(label = paste0(perfil, " (n=", n, ")")), colour = "white", fontface = "bold",
                     size = 4.2, label.size = 0, box.padding = .6, seed = SEMENTE, show.legend = FALSE) +
    scale_colour_manual(values = pal_perfil, name = NULL) + scale_fill_manual(values = pal_perfil, name = NULL) +
    coord_fixed() + labs(title = "Perfis de enquadramento no plano 1-2", subtitle = "Cada ponto = 1 publicação · polígono = envoltória do perfil",
                         x = lab_eixo(1), y = lab_eixo(2), caption = FONTE) + tema_acm()
  salvar(p, "15_perfis_plano12", 12, 9)

  # (b) Assinatura de cada perfil: % de publicações do perfil em cada modalidade ativa vs. corpus
  assin <- bind_rows(lapply(vars_ativas, function(v) {
    x <- base_acm[[v]]
    tot <- prop.table(table(x)) * 100
    bind_rows(lapply(levels(ind$perfil), function(pf) {
      pc <- prop.table(table(factor(x[ind$perfil == pf], levels = levels(x)))) * 100
      tibble(perfil = pf, cod = v, mod = levels(x), pct_perfil = as.numeric(pc), pct_corpus = as.numeric(tot[levels(x)]))
    }))
  })) %>% mutate(dif = pct_perfil - pct_corpus, rotulo = rot(mod))
  mostrar <- assin %>% group_by(mod) %>% filter(max(abs(dif)) >= 15) %>% ungroup()
  mostrar$rotulo <- factor(mostrar$rotulo, levels = rev(unique(rot(unlist(lapply(vars_ativas, function(v) levels(base_acm[[v]])))))))
  p <- ggplot(mostrar, aes(perfil, rotulo, fill = dif)) +
    geom_tile(colour = "white", linewidth = .8) +
    geom_text(aes(label = round(pct_perfil), colour = abs(dif) > 30), size = 3.6, fontface = "bold") +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, name = "Diferença\nvs. corpus (p.p.)") +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey20"), guide = "none") +
    labs(title = "Assinatura de cada perfil de enquadramento",
         subtitle = "Número = % das publicações do perfil que têm a modalidade\nCor = quanto acima (vermelho) ou abaixo (azul) do corpus · só modalidades com diferença ≥ 15 p.p.",
         x = NULL, y = NULL, caption = FONTE) + tema_acm(12) + theme(panel.grid = element_blank(), legend.position = "right")
  salvar(p, "16_assinatura_perfis", 11, 13)

  # (c) Quem usa cada perfil? Emissor / Polo / Momento da Greve
  comp <- bind_rows(
    ind %>% count(grupo = "Polo da Disputa", cat = as.character(POLO), perfil),
    ind %>% count(grupo = "Emissor", cat = as.character(EMISSOR), perfil),
    ind %>% count(grupo = "Momento da Greve", cat = as.character(MG), perfil)) %>%
    group_by(grupo, cat) %>% mutate(pct = 100 * n / sum(n), tot = sum(n)) %>% ungroup() %>%
    mutate(grupo = factor(grupo, levels = c("Polo da Disputa", "Emissor", "Momento da Greve")),
           cat = factor(cat, levels = unique(c(levels(ind$POLO), levels(ind$EMISSOR), levels(ind$MG)))),
           lab = paste0(cat, "\n(n=", tot, ")"))
  comp$lab <- factor(comp$lab, levels = unique(comp$lab[order(comp$cat)]))
  p <- ggplot(comp, aes(lab, pct, fill = perfil)) +
    geom_col(width = .8, colour = "white") +
    geom_text(aes(label = ifelse(pct >= 8, paste0(round(pct), "%"), "")), position = position_stack(vjust = .5),
              colour = "white", fontface = "bold", size = 3.6) +
    facet_grid(~ grupo, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = pal_perfil, name = NULL) + scale_y_continuous(expand = expansion(mult = c(0, .02))) +
    labs(title = "Quais perfis de enquadramento cada grupo mobiliza?",
         subtitle = "Composição (%) das publicações de cada polo, emissor e momento da greve, por perfil",
         x = NULL, y = "% das publicações do grupo", caption = FONTE) + tema_acm(12)
  salvar(p, "17_perfis_por_grupo", 16, 7.5)
}

if (RODAR_SENSIBILIDADE && length(alerta_vars) > 0 && length(vars_ativas) > 2) {
  sens <- bind_rows(lapply(alerta_vars, function(v) {
    r2 <- roda_acm(setdiff(vars_ativas, v), c(vars_sup, v))
    cor_max <- function(d) max(abs(cor(res$ind$coord[, d], r2$ind$coord[, 1:2])))
    tibble(variavel_rebaixada = nome_de[[v]],
           pct_eixo1_sem = round(r2$eig[1, 2], 1), pct_eixo2_sem = round(r2$eig[2, 2], 1),
           cor_eixo1 = round(cor_max(1), 2), cor_eixo2 = round(cor_max(2), 2))
  }))
  cat("Solução principal: eixo 1 =", round(eig$pct[1], 1), "% | eixo 2 =", round(eig$pct[2], 1), "%\n\n")
  print(as.data.frame(sens), right = FALSE)
  sens_long <- sens %>% pivot_longer(c(cor_eixo1, cor_eixo2), names_to = "eixo", values_to = "cor") %>%
    mutate(eixo = ifelse(eixo == "cor_eixo1", "Eixo 1", "Eixo 2"))
  p <- ggplot(sens_long, aes(cor, reorder(variavel_rebaixada, cor), fill = eixo)) +
    geom_col(position = position_dodge(.8), width = .7) +
    geom_text(aes(label = formatC(cor, format = "f", digits = 2, decimal.mark = ",")),
              position = position_dodge(.8), hjust = -0.15, size = 3.8, fontface = "bold") +
    geom_vline(xintercept = .9, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 1.1), breaks = seq(0, 1, .25)) +
    scale_fill_manual(values = c("Eixo 1" = "#C1121F", "Eixo 2" = "#003049"), name = NULL) +
    labs(title = "Sensibilidade: a solução muda se a variável for rebaixada?",
         subtitle = "Correlação máxima entre os eixos da ACM principal e os da ACM sem a variável · tracejado = 0,90",
         x = "Correlação (valor absoluto)", y = NULL, caption = FONTE) + tema_acm()
  salvar(p, "18_sensibilidade", 12, 5.5)
} else { cat("Sem alertas de 'Outro' abaixo do limiar (ou sensibilidade desligada).\n") }

# ---- Registro da decisão (passo 5): tabela pronta para colar no Controle de Versão ----
registro <- log_freq %>% filter(acao == "agrupada") %>%
  group_by(Variavel = nome) %>%
  summarise(`Categorias agrupadas` = paste0(cat, " (n=", n, ")", collapse = "; "),
            `Destino` = first(destino), `Frequência agrupada` = sum(n),
            `Limiar aplicado` = paste0("n < ", first(limiar)), .groups = "drop") %>%
  mutate(Data = format(Sys.Date(), "%d/%m/%Y"))
print(as.data.frame(registro), right = FALSE)

# ---- Exportação ----
tabelas <- list(
  eixos = eig %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
  modalidades_ativas = modA %>% select(mod, variavel = nome_var, n, starts_with("D"), starts_with("ctr"), starts_with("cos")),
  suplementares = modS %>% select(mod, variavel = nome_var, familia, n, starts_with("D"), starts_with("vt"), starts_with("cos")),
  coordenadas_unidades = ind,
  tratamento_frequencia = log_freq %>% select(variavel = nome, categoria = cat, n, pct, acao, destino, limiar),
  registro_controle_versao = registro,
  resumo_tratamento = resumo_trat)
if (exists("sens")) tabelas$sensibilidade <- sens
if (exists("hc"))   tabelas$perfis_descricao <- bind_rows(lapply(names(hc$desc.var$category), function(cl_) {
  as.data.frame(hc$desc.var$category[[cl_]]) %>% mutate(perfil = cl_, modalidade = rownames(.)) }))
write_xlsx(tabelas, file.path(PASTA_SAIDA, "resultados_ACM.xlsx"))
cat("Tabelas e figuras salvas em:", normalizePath(PASTA_SAIDA), "\n")
list.files(PASTA_SAIDA)

# if (interactive()) {
#   if (!requireNamespace("explor", quietly = TRUE)) install.packages("explor")
#   explor::explor(res)
# }
sessionInfo()

