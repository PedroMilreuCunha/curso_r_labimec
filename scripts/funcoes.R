# Pedro Milreu Cunha - Doutorando em Economia Aplicada pelo PPGE-UFPB #
# Contato: pcunha.2107@gmail.com #

##########################################################################
###################** CURSO DE INTRODUÇÃO A R - LABIMEC **################
##########################################################################

#---- Script de exemplos nº3: FUNÇÕES #----

# Bibliotecas ----

library(dplyr) # Manipulação de dados

saudar <- function() { 
    
    print("Saudações, usuário!")
    
}

saudar()

custo_energia <- function(tipo_local, consumo_kwh, atraso = 0, desconto = 0) {
    
    # ENTRADA:
    # tipo_local: string indicando se o local em questão é residencial, comercial ou industrial;
    # consumo_kwh: float com o consumo de energia do local em kW/h;
    # atraso: float com o percentual a ser cobrado por atraso (padrão de 0%);
    # desconto: float com o percentual de desconto a ser aplicado (padrão de 0%);
    
    # SAÍDA:
    # Não retorna nada.
    
    custo_kwh <- ifelse(tolower(tipo_local) == "residencial",
                        0.72, ifelse(tolower(tipo_local) == "comercial",
                                     0.85, 1.07))
    total = custo_kwh*consumo_kwh*(1 + (atraso/100) - (desconto)/100)
    
    print(paste0("Consumo: ", consumo_kwh, "kW/h"))
    print(paste0("Tipo de local: ", tipo_local))
    print(paste0("Custo por atraso: R$", (atraso/100)*custo_kwh*consumo_kwh))
    print(paste0("Desconto: R$", (desconto/100)*custo_kwh*consumo_kwh))
    print(paste0("Custo do kW/h: R$", custo_kwh))
    print(paste0("Total a ser pago: R$", total))
    
}

custo_energia(tipo_local = "Comercial", consumo_kwh = 800, atraso = 3)

simulacao_aniversario <- function(n_pessoas) {
    # ENTRADA:
    # n_pessoas: sequência de número inteiros; 
    # SAÍDA:
    # resultados: lista contendo um data.frame com os resultados e um gráfico com a visualização deles.
    
    # Simulação 
    produtorio <- rep(1, length(n_pessoas))
    probabilidades <- rep(1, length(n_pessoas))
    for (i in 1:length(n_pessoas)) {
        for (j in 1:(n_pessoas[i] - 1)) {
            produtorio[i] <- produtorio[i] * (365- j)/365
        }
    }
    probabilidades <- 1 - produtorio
    
    # Gráfico da simulação
    dados <- data.frame(n = n_pessoas, p = probabilidades)
    dados$satisfazem <- ifelse(dados$p >= 0.5,
                               "Sim", "Não")
    g <- ggplot(dados, aes(x = n, y = p, color = satisfazem, linetype = satisfazem)) +
        geom_line(linewidth = 1.25, show.legend = FALSE ) + 
        geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 1, color = "black") + 
        geom_vline(xintercept = 23, linetype = "dashed", linewidth = 1, color = "black") +
        labs(x = "Nº de pessoas", y = "Probabilidade", color = ">= 50%")
    
    colnames(dados) <- c("Nº de pessoas", "Probabilidade", ">=50%")
    resultados <- list(dados, g)
    return(resultados)
}
resultados <- simulacao_aniversario(seq(12, 36, 1))

resultados[2]
