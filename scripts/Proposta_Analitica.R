install.packages("readxl")

library(readxl)


# Carregar a base inteira, pulando as primeiras 6 linhas
dados <- read_excel("C:\\Users\\Euller Nogueira\\Documents\\Atividades Faculdade\\PROJETO\\Dataset Projeto Dados 2016 - 2022.xlsx", 
                    skip = 6)

# Filtrar a linha 9
dados <- dados[-9, ]

# Remover as linhas 1 e 2
dados <- dados[-c(1, 2), ]

# Filtrar apenas os dados da Universidade de São Paulo (USP)
dados_usp <- dados[dados$`Nome da Instituição` == "UNIVERSIDADE DE SÃO PAULO", ]

# Visualizar os dados
View(dados_usp)

num_linhas <- nrow(dados_usp)
num_colunas <- ncol(dados_usp)
cat("Número de Linhas:", num_linhas, "\n")
cat("Número de Colunas:", num_colunas, "\n")

dados_usp_selecionados <- dados_usp[, c("Nome da Instituição", "Nome do Curso de Graduação", 
                                        "Ano de Ingresso", "Ano de Referência", 
                                        "Quantidade de Ingressantes no Curso", 
                                        "Quantidade de Permanência no Curso no ano de referência", 
                                        "Quantidade de Concluintes no Curso no ano de referência", 
                                        "Quantidade de Desistência no Curso no ano de referência")]


# Visualizar os dados selecionados
View(dados_usp_selecionados)


## ----------------------------------------------------------------------------------------------------------------------------------

##INGRESSANTES CURSO POR ANO

# Instalar e carregar o pacote dplyr
install.packages("dplyr")
library(dplyr)

# Filtrar apenas os dados da Universidade de São Paulo (USP)
dados_usp <- filter(dados, `Nome da Instituição` == "UNIVERSIDADE DE SÃO PAULO")

# Selecionar apenas as colunas necessárias
dados_usp_selecionados <- select(dados_usp, `Ano de Referência`, `Nome do Curso de Graduação`, `Quantidade de Ingressantes no Curso`)

# Converter a coluna 'Quantidade de Ingressantes no Curso' para numérica
dados_usp_selecionados$`Quantidade de Ingressantes no Curso` <- as.numeric(dados_usp_selecionados$`Quantidade de Ingressantes no Curso`)

# Agregar o total de ingressantes por curso e ano de referência
total_ingressantes_por_curso <- dados_usp_selecionados %>%
  group_by(`Ano de Referência`, `Nome do Curso de Graduação`) %>%
  summarise(Total_Ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE))

# Visualizar o total de ingressantes por curso
View(total_ingressantes_por_curso)

head(total_ingressantes_por_curso)


##----------------------------------------------------------------------


##RANKING INGRESSANTES TOTAIS CURSO 

# Calcula o total de ingressantes por curso
total_ingressantes_por_curso <- dados_usp_selecionados %>%
  group_by(`Nome do Curso de Graduação`) %>%
  summarise(Total_Ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE))

# Ordena os cursos pelo total de ingressantes em ordem decrescente
ranking_cursos <- total_ingressantes_por_curso %>%
  arrange(desc(Total_Ingressantes))

# Visualiza o ranking dos cursos com maior taxa de ingressantes
View(ranking_cursos)


##--------------------------------------------------------------------------------------------------------------

##CALCULANDO A TAXA DE RETENÇÃO 



dados_usp$`Quantidade de Concluintes no Curso no ano de referência` <- as.numeric(dados_usp$`Quantidade de Concluintes no Curso no ano de referência`)
dados_usp$`Quantidade de Desistência no Curso no ano de referência` <- as.numeric(dados_usp$`Quantidade de Desistência no Curso no ano de referência`)

dados_usp_selecionados <- dados_usp[, c("Nome do Curso de Graduação", 
                                        "Ano de Referência", 
                                        "Quantidade de Ingressantes no Curso", 
                                        "Quantidade de Permanência no Curso no ano de referência",
                                        "Quantidade de Concluintes no Curso no ano de referência",
                                        "Quantidade de Desistência no Curso no ano de referência")]


# Calcular as taxas de permanência, desistência e conclusão por curso e ano de referência
taxas <- dados_usp_selecionados %>%
  group_by(`Nome do Curso de Graduação`, `Ano de Referência`) %>%
  summarise(
    Taxa_Permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`) / sum(`Quantidade de Ingressantes no Curso`) * 100,
    Taxa_Desistencia = sum(`Quantidade de Desistência no Curso no ano de referência`) / sum(`Quantidade de Ingressantes no Curso`) * 100,
    Taxa_Conclusao = sum(`Quantidade de Concluintes no Curso no ano de referência`) / sum(`Quantidade de Ingressantes no Curso`) * 100
  ) %>%
  mutate(
    Taxa_Retencao = Taxa_Permanencia + Taxa_Conclusao
  )

View(taxas)


##------------------------------------------------------------------------------------------

##MEDIDAS DE POSIÇÃO E DISPERSÃO

#CONVERTENDO DADOS DA BASE PARA O TIPO NUMÉRICO 
dados_direito$`Quantidade de Desistência no Curso no ano de referência` <- as.numeric(dados_direito$`Quantidade de Desistência no Curso no ano de referência`)

#CALCULANDO MÉDIA E MEDIANA DOS DESISTENTES NO CURSO DE DIREITO
media_desistentes_direito <- mean(dados_direito$`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE)
mediana_desistentes_direito <- median(dados_direito$`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE)

cat("Média de Desistentes no Curso de Direito:", media_desistentes_direito, "\n")
cat("Mediana de Desistentes no Curso de Direito:", mediana_desistentes_direito, "\n")

#CONVERTENDO DADOS DA BASE PARA O TIPO NUMÉRICO 
dados_direito$`Quantidade de Desistência no Curso no ano de referência` <- as.numeric(dados_direito$`Quantidade de Desistência no Curso no ano de referência`)

# Filtrar os dados apenas para o curso de Direito
dados_direito <- filter(dados_usp_selecionados, `Nome do Curso de Graduação` == "DIREITO")

#CALCULANDO O DESVIO PADRÃO DOS DESISTENTES NO CURSO DE DIREITO
desvio_padrao_desistentes_direito <- sd(dados_direito$`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE)

print("Desvio padrão da quantidade de desistentes no curso de Direito:")
print(desvio_padrao_desistentes_direito)


#CALCULANDO O INTERVALO DA QUANTIDADE DE DESISTENTES DO CURSO DE DIREITO
intervalo_desistentes_direito <- range(dados_direito$`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE)

# Exibir o intervalo
print("Intervalo da quantidade de desistentes no curso de Direito:")
print(intervalo_desistentes_direito)







