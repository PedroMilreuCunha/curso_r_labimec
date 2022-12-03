# Pedro Milreu Cunha - Doutorando em Economia Aplicada pelo PPGE-UFPB #
# Contato: pcunha.2107@gmail.com #

##########################################################################
###################** CURSO DE INTRODUÇÃO A R - LABIMEC **################
##########################################################################

#---- Script de exemplos nº2: PACOTE STARGAZER #----

# Bibliotecas ----

library(dplyr) # Manipulação de dados
library(plm) # Dados em painel
library(stargazer)

# Importação dos dados ----

load("data/analise_enade.Rdata")
str(dados_enade)

# Convertemos a variável de código da IES para fator
dados_enade$CO_IES <- as.factor(dados_enade$CO_IES)
str(dados_enade$CO_IES)

# Estatísticas descritivas em ASCII 

stargazer(dados_enade, type = "text") # Apenas dados numéricos ou lógicos

# Estatísticas descritivas em LaTeX 

stargazer(dados_enade, type = "latex", header = FALSE)

# Modelo simples em ASCII 

stargazer(didreg, type = "text", single.row = TRUE)

# Modelo simples em LaTeX

stargazer(didreg, type = "latex", header = FALSE, single.row = TRUE)

# Modelo complexo em ASCII

stargazer(didreg_fe, didreg_fe_logit, didreg_fe_entropia,
          type = "text", single.row = TRUE,
          title = "Efeito médio de tratamento sobre os tratados - Ciclo 2009-2012",
          decimal.mark = ",", digits = 5,
          column.labels = c("Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          covariate.labels = c("Tempo", "Idade média", "Idade média²", 
                               "Prop. brancos", "Prop. casados", "Prop. homens",
                               "Prop. ensino medio ou mais", "Prop. renda per capita 3S.M.",
                               "Tempo x Tratamento"),
          model.numbers = FALSE,
          initial.zero = TRUE,
          dep.var.labels = "ln(Nota média)", dep.var.caption = "Variável dependente",
          multicolumn = TRUE, model.names = FALSE)

# Modelo complexo em LaTeX 

stargazer(didreg_fe, didreg_fe_logit, didreg_fe_entropia,
          type = "latex",
          title = "Efeito médio de tratamento sobre os tratados - Ciclo 2009-2012",
          decimal.mark = ",", digits = 5,
          column.labels = c("Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          covariate.labels = c("Tempo", "Idade média", "Idade média²", 
                               "Prop. brancos", "Prop. casados", "Prop. homens",
                               "Prop. ensino medio ou mais", "Prop. renda per capita 3S.M.",
                               "Tempo x Tratamento"),
          model.numbers = FALSE,
          initial.zero = TRUE,
          dep.var.labels = "ln(Nota média)", dep.var.caption = "Variável dependente",
          multicolumn = TRUE, model.names = FALSE, align = TRUE, header = FALSE,
          table.placement = "H", no.space = TRUE)

