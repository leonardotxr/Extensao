# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11412     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 


summary(dados_sinasc_1$CODMUNRES)
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)

# Considerando que a resolução é para o estado do Acre, ou seja, UF = 12
dados_sinasc_2 = dados_sinasc_1[UF == "12",]

# Exportar o arquivo com o nome dados_sinasc_2.csv e apagando arquivos não mais necessários
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

rm(dados_sinasc, dados_sinasc_1)
gc()

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, CONSPRENAT, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator


dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))



# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

# 1. Leitura da Tabela PIG
# Atenção: Ajuste sep e dec se a formatação do seu CSV for diferente (ex: sep = ",", dec = ".")
tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", sep = ";", dec = ",")

# 2. Agregar P10 e P90 ao banco principal
# Assumimos que as colunas na tabela externa também se chamam "SEMAGESTAC" e "SEXO".
# Certifique-se de que a coluna SEXO na tabela externa também possui os rótulos "Masculino" e "Feminino" 
# para que o merge funcione perfeitamente.
dados_sinasc_2 <- merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

# 3. Criar a nova variável F_PIG inicializada com NA
dados_sinasc_2$F_PIG <- NA

# 4. Criar um vetor lógico para mapear os casos válidos:
# Apenas GRAVIDEZ Única E que não possuem NA nas colunas cruciais
casos_validos <- dados_sinasc_2$GRAVIDEZ == "Única" &
  !is.na(dados_sinasc_2$SEMAGESTAC) &
  !is.na(dados_sinasc_2$PESO) &
  !is.na(dados_sinasc_2$SEXO) &
  !is.na(dados_sinasc_2$PESO_P10) &
  !is.na(dados_sinasc_2$PESO_P90)

# Substituir possíveis NAs resultantes da validação lógica por FALSE
casos_validos[is.na(casos_validos)] <- FALSE

# 5. Classificar em PIG, AIG e GIG usando indexação condicional
dados_sinasc_2$F_PIG[casos_validos & dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10] <- "PIG"
dados_sinasc_2$F_PIG[casos_validos & dados_sinasc_2$PESO >= dados_sinasc_2$PESO_P10 & dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90] <- "AIG"
dados_sinasc_2$F_PIG[casos_validos & dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90] <- "GIG"

# 6. Transformar F_PIG em fator
dados_sinasc_2$F_PIG <- factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))


# Tarefas 9 e 10 (reformulada)

# Crie um banco de dados contendo as 103 variáveis listadas no arquivo
# “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”
# O banco final deverá possuir:
#  103 colunas, correspondentes às variáveis especificadas;
#  n + 1 linhas, onde:
#  n corresponde ao número de municípios distintos da UF em análise
#  a primeira linha corresponde aos valores agregados para a UF como
# um todo;
# as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC,
# 
# respeitando os nomes e a ordem especificados 

# Base inicial (municípios)
# Cria um dataframe com uma única coluna (CODMUNRES) com valores ordenados e sem repetição
base = data.frame(CODMUNRES =sort(unique(dados_sinasc_2$CODMUNRES)))

# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)

# TNRC - completos nas 61 variáveis
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "12",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "12",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

# Frequências de variáveis categóricas
# Exemplos:
#Sexo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Feminino",
                                                                             "Masculino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRSEXO_F","TRSEXO_M")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Tipo de Parto
tab = table( dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal",
                                                                               "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV","TPC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Idade categorizada
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-
19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c( "TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44",
               "TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Medidas descritivas
#Exemplos de média e desvio-padrão
# Idade da mãe
media_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_idade$IDADEMAE = round(media_idade$IDADEMAE, 2)
names(media_idade)[2] = "IM_MD"
dp_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE = round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] = "IM_DP"
temp = merge(media_idade, dp_idade, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Exemplos de percentis
# Idade da mãe
p_idade = aggregate( IDADEMAE ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs =
c(0.25,0.5,0.75), na.rm = TRUE))
p_idade = do.call(data.frame, p_idade)
names(p_idade) = c("CODMUNRES","IM_P25","IM_P50", "IM_P75")
p_idade[, c("IM_P25","IM_P50","IM_P75")] = round(p_idade[, c("IM_P25","IM_P50","IM_P75")], 2)
base = merge(base, p_idade, by="CODMUNRES", all.x=TRUE)

# Exemplo no banco que só teria as variáveis "CODMUNRES","IM_MD","IM_DP","IM_P25","IM_P50",
# "IM_P75"

# Substituir NA por 0 (somente contagens)
cols_contagem = setdiff( names(base), c("CODMUNRES","IM_MD","IM_DP","IM_P25","IM_P50",
"IM_P75"))

base[cols_contagem][is.na(base[cols_contagem])] = 0

# Inserindo agora linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA

# colunas de contagem: indicar as variáveis contínuas, que por exclusão não terão valores somados
cols_contagem = setdiff( names(base), c("CODMUNRES","IM_MD","IM_DP","IM_P25",
"IM_P50","IM_P75"))
linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# medidas contínuas (idade da mãe)
linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
q = round(quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$IM_P25 = q[1]
linha_estado$IM_P50 = q[2]
linha_estado$IM_P75 = q[3]

# código da UF considerando o estado do Acre
linha_estado$CODMUNRES = 12 # ou = “12” dependendo do formato de CODMUNRES
SINASC_AC = rbind(linha_estado, base)
SINASC_AC$NIVEL = c("UF", rep("MUNICIPIO", nrow(SINASC_AC)-1))
SINASC_AC$ANO = 2015
SINASC_AC = SINASC_AC[, c( "ANO","NIVEL","CODMUNRES", names(SINASC_AC)[!names(SINASC_AC)
%in% c("ANO","NIVEL","CODMUNRES")])]

# Escolaridade da mãe
tab = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$ESCMAE2010)
df = as.data.frame.matrix(tab)

names(df) = c("EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC")

df$CODMUNRES = rownames(df)
base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Raça/cor da mãe
tab = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACORMAE)
df = as.data.frame.matrix(tab)

names(df) = c("TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I")

df$CODMUNRES = rownames(df)
base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Estado civil da mãe
tab = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$ESTCIVMAE)
df = as.data.frame.matrix(tab)

names(df) = c("TGSC","TGCC")

df$CODMUNRES = rownames(df)
base = merge(base, df, by="CODMUNRES", all.x=TRUE)

unique(dados_sinasc_2$PARIDADE)

# Paridade
dados_sinasc_2$TGPRI_ind <- ifelse(dados_sinasc_2$PARIDADE == "Nulípara", 1, 0)
dados_sinasc_2$TGNPRI_ind <- ifelse(dados_sinasc_2$PARIDADE == "Multípara", 1, 0)

paridade <- aggregate(cbind(TGPRI_ind, TGNPRI_ind) ~ CODMUNRES,
                      dados_sinasc_2, sum, na.rm = TRUE)

names(paridade) <- c("CODMUNRES", "TGPRI", "TGNPRI")

base <- merge(base, paridade, by = "CODMUNRES", all.x = TRUE)

# Tipo de gravidez
tab = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$GRAVIDEZ)
df = as.data.frame.matrix(tab)

names(df) = c("TGU","TGG")

df$CODMUNRES = rownames(df)
base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Media e percentis da gestação
media_gest = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, mean, na.rm=TRUE)
names(media_gest)[2] = "DG_MD"

dp_gest = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, sd, na.rm=TRUE)
names(dp_gest)[2] = "DG_DP"

p_gest = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2,
                   function(x) quantile(x, probs=c(0.25,0.5,0.75), na.rm=TRUE))

p_gest = do.call(data.frame, p_gest)
names(p_gest) = c("CODMUNRES","DG_P25","DG_P50","DG_P75")

temp = merge(media_gest, dp_gest, by="CODMUNRES")
temp = merge(temp, p_gest, by="CODMUNRES")

base = merge(base, temp, by="CODMUNRES", all.x=TRUE)

# Peso do recém nascido
media_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, mean, na.rm=TRUE)
names(media_peso)[2] = "PESO_MD"

dp_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, sd, na.rm=TRUE)
names(dp_peso)[2] = "PESO_DP"

p_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2,
                   function(x) quantile(x, probs=c(0.25,0.5,0.75), na.rm=TRUE))

p_peso = do.call(data.frame, p_peso)
names(p_peso) = c("CODMUNRES","PESO_P25","PESO_P50","PESO_P75")

temp = merge(media_peso, dp_peso, by="CODMUNRES")
temp = merge(temp, p_peso, by="CODMUNRES")

base = merge(base, temp, by="CODMUNRES", all.x=TRUE)

# Local do nascimento
dados_sinasc_2$LOC_H <- ifelse(dados_sinasc_2$LOCNASC == "Hospital", 1, 0)
dados_sinasc_2$LOC_ES <- ifelse(dados_sinasc_2$LOCNASC == "Estabelecimento de saúde", 1, 0)
dados_sinasc_2$LOC_D <- ifelse(dados_sinasc_2$LOCNASC == "Domicílio", 1, 0)
dados_sinasc_2$LOC_O <- ifelse(dados_sinasc_2$LOCNASC == "Outros", 1, 0)
dados_sinasc_2$LOC_AI <- ifelse(dados_sinasc_2$LOCNASC == "Aldeia indígena", 1, 0)

local <- aggregate(cbind(LOC_H, LOC_ES, LOC_D, LOC_O, LOC_AI) ~ CODMUNRES,
                   dados_sinasc_2, sum, na.rm = TRUE)

names(local) <- c("CODMUNRES","TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI")

base <- merge(base, local, by="CODMUNRES", all.x=TRUE)

# Sexo do recém nascido
dados_sinasc_2$TRS_M <- ifelse(dados_sinasc_2$SEXO == "Masculino", 1, 0)
dados_sinasc_2$TRS_F <- ifelse(dados_sinasc_2$SEXO == "Feminino", 1, 0)

sexo <- aggregate(cbind(TRS_M, TRS_F) ~ CODMUNRES,
                  dados_sinasc_2, sum, na.rm = TRUE)

base <- merge(base, sexo, by="CODMUNRES", all.x=TRUE)

# Raça cor do recém nascido
dados_sinasc_2$RC_B <- ifelse(dados_sinasc_2$RACACOR == "Branca", 1, 0)
dados_sinasc_2$RC_PT <- ifelse(dados_sinasc_2$RACACOR == "Preta", 1, 0)
dados_sinasc_2$RC_A <- ifelse(dados_sinasc_2$RACACOR == "Amarela", 1, 0)
dados_sinasc_2$RC_PD <- ifelse(dados_sinasc_2$RACACOR == "Parda", 1, 0)
dados_sinasc_2$RC_I <- ifelse(dados_sinasc_2$RACACOR == "Indígena", 1, 0)

raca <- aggregate(cbind(RC_B, RC_PT, RC_A, RC_PD, RC_I) ~ CODMUNRES,
                  dados_sinasc_2, sum, na.rm = TRUE)

names(raca) <- c("CODMUNRES","TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I")

base <- merge(base, raca, by="CODMUNRES", all.x=TRUE)

# Classificação do peso:
dados_sinasc_2$TRP_BP <- ifelse(dados_sinasc_2$PESO < 2500, 1, 0)
dados_sinasc_2$TRP_N <- ifelse(dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000, 1, 0)
dados_sinasc_2$TRP_M <- ifelse(dados_sinasc_2$PESO >= 4000, 1, 0)

peso_cat <- aggregate(cbind(TRP_BP, TRP_N, TRP_M) ~ CODMUNRES,
                      dados_sinasc_2, sum, na.rm = TRUE)

base <- merge(base, peso_cat, by="CODMUNRES", all.x=TRUE)

# Apgar 5
dados_sinasc_2$APG_B <- ifelse(dados_sinasc_2$APGAR5 < 7, 1, 0)
dados_sinasc_2$APG_N <- ifelse(dados_sinasc_2$APGAR5 >= 7, 1, 0)

apgar_cat <- aggregate(cbind(APG_B, APG_N) ~ CODMUNRES,
                       dados_sinasc_2, sum, na.rm = TRUE)

names(apgar_cat) <- c("CODMUNRES","TRAPG5_B","TRAPG5_N")

base <- merge(base, apgar_cat, by="CODMUNRES", all.x=TRUE)

# Anomalia congenita:
dados_sinasc_2$TRAC <- ifelse(dados_sinasc_2$IDANOMAL == "Sim", 1, 0)
dados_sinasc_2$TRSAC <- ifelse(dados_sinasc_2$IDANOMAL == "Não", 1, 0)

anomalia <- aggregate(cbind(TRAC, TRSAC) ~ CODMUNRES,
                      dados_sinasc_2, sum, na.rm = TRUE)

base <- merge(base, anomalia, by="CODMUNRES", all.x=TRUE)

# média e dp
apg_media <- aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, mean, na.rm=TRUE)
names(apg_media)[2] <- "APG5_MD"

apg_dp <- aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, sd, na.rm=TRUE)
names(apg_dp)[2] <- "APG5_DP"

temp <- merge(apg_media, apg_dp, by="CODMUNRES")

base <- merge(base, temp, by="CODMUNRES", all.x=TRUE)

# TGIF
dados_sinasc_2$TGIF_ind <- ifelse(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49, 1, 0)

TGIF <- aggregate(TGIF_ind ~ CODMUNRES, dados_sinasc_2, sum, na.rm = TRUE)

names(TGIF) <- c("CODMUNRES","TGIF")

base <- merge(base, TGIF, by="CODMUNRES", all.x=TRUE)

# Duração da gestação
dados_sinasc_2$TGD_22_ind <- ifelse(dados_sinasc_2$SEMAGESTAC < 22, 1, 0)
dados_sinasc_2$TGD_22_27_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 22 & dados_sinasc_2$SEMAGESTAC <= 27, 1, 0)
dados_sinasc_2$TGD_28_31_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 28 & dados_sinasc_2$SEMAGESTAC <= 31, 1, 0)
dados_sinasc_2$TGD_32_36_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 32 & dados_sinasc_2$SEMAGESTAC <= 36, 1, 0)
dados_sinasc_2$TGD_37_41_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41, 1, 0)
dados_sinasc_2$TGD_42_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 42, 1, 0)

TGD <- aggregate(cbind(TGD_22_ind,TGD_22_27_ind,TGD_28_31_ind,
                       TGD_32_36_ind,TGD_37_41_ind,TGD_42_ind) ~ CODMUNRES,
                 dados_sinasc_2, sum, na.rm=TRUE)

names(TGD) <- c("CODMUNRES","TGD_22","TGD_22_27","TGD_28_31",
                "TGD_32_36","TGD_37_41","TGD_42")

base <- merge(base, TGD, by="CODMUNRES", all.x=TRUE)

# Classificação final da gestação
dados_sinasc_2$TGD_PRT_ind <- ifelse(dados_sinasc_2$SEMAGESTAC < 37, 1, 0)
dados_sinasc_2$TGD_AT_ind  <- ifelse(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41, 1, 0)
dados_sinasc_2$TGD_PST_ind <- ifelse(dados_sinasc_2$SEMAGESTAC >= 42, 1, 0)

TGD_class <- aggregate(cbind(TGD_PRT_ind,TGD_AT_ind,TGD_PST_ind) ~ CODMUNRES,
                       dados_sinasc_2, sum, na.rm=TRUE)

names(TGD_class) <- c("CODMUNRES","TGD_PRT","TGD_AT","TGD_PST")

base <- merge(base, TGD_class, by="CODMUNRES", all.x=TRUE)

# Consultas pre natal
dados_sinasc_2$TKC_NR_ind  <- ifelse(dados_sinasc_2$CONSPRENAT == 0, 1, 0)
dados_sinasc_2$TKC_ID_ind  <- ifelse(dados_sinasc_2$CONSPRENAT >= 1 & dados_sinasc_2$CONSPRENAT <= 3, 1, 0)
dados_sinasc_2$TKC_IT_ind  <- ifelse(dados_sinasc_2$CONSPRENAT >= 4 & dados_sinasc_2$CONSPRENAT <= 6, 1, 0)
dados_sinasc_2$TKC_AD_ind  <- ifelse(dados_sinasc_2$CONSPRENAT >= 7 & dados_sinasc_2$CONSPRENAT <= 9, 1, 0)
dados_sinasc_2$TKC_MAD_ind <- ifelse(dados_sinasc_2$CONSPRENAT >= 10, 1, 0)

TKC <- aggregate(cbind(TKC_NR_ind,TKC_ID_ind,TKC_IT_ind,TKC_AD_ind,TKC_MAD_ind) ~ CODMUNRES,
                 dados_sinasc_2, sum, na.rm=TRUE)

names(TKC) <- c("CODMUNRES","TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD")

base <- merge(base, TKC, by="CODMUNRES", all.x=TRUE)

# Peregrinação
dados_sinasc_2$TGPRG_S <- ifelse(dados_sinasc_2$PERIG == "Sim", 1, 0)
dados_sinasc_2$TGPRG_N <- ifelse(dados_sinasc_2$PERIG == "Não", 1, 0)

TGPRG <- aggregate(cbind(TGPRG_S,TGPRG_N) ~ CODMUNRES,
                   dados_sinasc_2, sum, na.rm=TRUE)

base <- merge(base, TGPRG, by="CODMUNRES", all.x=TRUE)

# Posição do bebê
dados_sinasc_2$TRAP_C <- ifelse(dados_sinasc_2$TPAPRESENT == "Cefálico", 1, 0)
dados_sinasc_2$TRAP_P <- ifelse(dados_sinasc_2$TPAPRESENT == "Pélvica ou podálica", 1, 0)
dados_sinasc_2$TRAP_T <- ifelse(dados_sinasc_2$TPAPRESENT == "Transversa", 1, 0)

TRAP <- aggregate(cbind(TRAP_C, TRAP_P, TRAP_T) ~ CODMUNRES,
                  dados_sinasc_2, sum, na.rm = TRUE)

names(TRAP) <- c("CODMUNRES", "TRAP_C", "TRAP_P", "TRAP_T")

base <- merge(base, TRAP, by = "CODMUNRES", all.x = TRUE)

# Robson: 
dados_sinasc_2$TGROB_1  <- ifelse(dados_sinasc_2$TPROBSON == 1, 1, 0)
dados_sinasc_2$TGROB_2  <- ifelse(dados_sinasc_2$TPROBSON == 2, 1, 0)
dados_sinasc_2$TGROB_3  <- ifelse(dados_sinasc_2$TPROBSON == 3, 1, 0)
dados_sinasc_2$TGROB_4  <- ifelse(dados_sinasc_2$TPROBSON == 4, 1, 0)
dados_sinasc_2$TGROB_5  <- ifelse(dados_sinasc_2$TPROBSON == 5, 1, 0)
dados_sinasc_2$TGROB_6  <- ifelse(dados_sinasc_2$TPROBSON == 6, 1, 0)
dados_sinasc_2$TGROB_7  <- ifelse(dados_sinasc_2$TPROBSON == 7, 1, 0)
dados_sinasc_2$TGROB_8  <- ifelse(dados_sinasc_2$TPROBSON == 8, 1, 0)
dados_sinasc_2$TGROB_9  <- ifelse(dados_sinasc_2$TPROBSON == 9, 1, 0)
dados_sinasc_2$TGROB_10 <- ifelse(dados_sinasc_2$TPROBSON == 10, 1, 0)

robson <- aggregate(cbind(TGROB_1, TGROB_2, TGROB_3, TGROB_4, TGROB_5,
                          TGROB_6, TGROB_7, TGROB_8, TGROB_9, TGROB_10) ~ CODMUNRES,
                    dados_sinasc_2, sum, na.rm = TRUE)

base <- merge(base, robson, by = "CODMUNRES", all.x = TRUE)

#PIG / AIG / GIG
dados_sinasc_2$TRPIG_P <- ifelse(dados_sinasc_2$GRAVIDEZ == "Única" & dados_sinasc_2$PESO < 2500, 1, 0)
dados_sinasc_2$TRPIG_A <- ifelse(dados_sinasc_2$GRAVIDEZ == "Única" & dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000, 1, 0)
dados_sinasc_2$TRPIG_G <- ifelse(dados_sinasc_2$GRAVIDEZ == "Única" & dados_sinasc_2$PESO >= 4000, 1, 0)

TRPIG <- aggregate(cbind(TRPIG_P,TRPIG_A,TRPIG_G) ~ CODMUNRES,
                   dados_sinasc_2, sum, na.rm=TRUE)

base <- merge(base, TRPIG, by="CODMUNRES", all.x=TRUE)

names(base) # Observando os nomes para identificar as variaveis q estavam "sobrando" (a base acabou ficando com 107 variaveis inves de 103)

# As variaveis sobrantes eram colunas NA, removendo elas temos:
base <- base[, !is.na(names(base))]
base <- base[, !grepl("^NA", names(base))]
base <- base[, !duplicated(names(base))]

# Conferindo as variaveis:
ncol(base)
nrow(base)

# Agregando a linha da UF como um todo:
linha_UF <- base[1, ]
linha_UF[,] <- NA

# Preenchendo somatorios (todas as variaveis de contagem)
cols_contagem <- setdiff(names(base), c("CODMUNRES","IM_MD","IM_DP","IM_P25","IM_P50","IM_P75"))

linha_UF[cols_contagem] <- colSums(base[cols_contagem], na.rm = TRUE)

# Corrigindo variaveis continuas:
linha_UF$IM_MD <- round(mean(dados_sinasc_2$IDADEMAE, na.rm=TRUE),2)
linha_UF$IM_DP <- round(sd(dados_sinasc_2$IDADEMAE, na.rm=TRUE),2)

q <- quantile(dados_sinasc_2$IDADEMAE, probs=c(0.25,0.5,0.75), na.rm=TRUE)
linha_UF$IM_P25 <- q[1]
linha_UF$IM_P50 <- q[2]
linha_UF$IM_P75 <- q[3]

# Colocando codigo da UF:
linha_UF$CODMUNRES <- "12"

# Juntando a linha UF com o restante da base:
base_final <- rbind(linha_UF, base)

# Adcionando nivel e ano:
base_final$NIVEL <- c("UF", rep("MUNICIPIO", nrow(base_final)-1))
base_final$ANO <- 2015

# Organizando colunas:
base_final <- base_final[, c("ANO","NIVEL","CODMUNRES",
                             setdiff(names(base_final), c("ANO","NIVEL","CODMUNRES")))]

nrow(base_final)
ncol(base_final)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(base_final, "SINASC_AC.csv", row.names = FALSE)


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

