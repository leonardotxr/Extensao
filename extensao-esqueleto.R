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

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4,5), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))
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
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
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

dados_sinasc_2$PEREG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PEREG = factor(dados_sinasc_2$PEREG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))



# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)

dados_sinasc_2$F_PIG = ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                              ifelse(is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90), NA,
                                     ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                            ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG",
                                                   "GIG"))))

dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

# Tarefas 9 e 10 (reformulada) do script esqueleto:

# Crie um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”

# O banco final deverá possuir:
# • 103 colunas, correspondentes às variáveis especificadas;
# • n + 1 linhas, onde:
#  • n corresponde ao número de municípios distintos da UF em análise
# • a primeira linha corresponde aos valores agregados para a UF como um todo;
# • as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC (dados_sinasc, dados_sinasc_1 e dados_sinasc_2), respeitando os nomes e a ordem especificados.

#################################################  
# Base inicial (municípios)
#################################################
base = data.frame(CODMUNRES = sort(unique(dados_sinasc_2$CODMUNRES)))

# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")

base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)


# TNRC - completos nas 61 variáveis
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "12",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]

TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES,levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")

base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "12",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]

TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")

base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

#################################################
# Informações das gestantes
#################################################
# Idade

# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c("TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)

df$TGIF= df$TGI_15_19 + df$TGI_20_24 + df$TGI_25_29 + df$TGI_30_34 +
  df$TGI_35_39 + df$TGI_40_44 + df$TGI_45_49

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Percentis
p_idade = aggregate(IDADEMAE ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_idade = do.call(data.frame, p_idade)
names(p_idade) = c("CODMUNRES","IM_P25","IM_P50","IM_P75")
p_idade[, c("IM_P25","IM_P50","IM_P75")] = round(p_idade[, c("IM_P25","IM_P50","IM_P75")], 2)

base = merge(base, p_idade, by="CODMUNRES", all.x=TRUE)

# Média
media_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_idade$IDADEMAE = round(media_idade$IDADEMAE, 2)
names(media_idade)[2] = "IM_MD"

# Desvio-padrão
dp_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE = round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] = "IM_DP"
temp = merge(media_idade, dp_idade, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Escolaridade
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESCMAE2010, levels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo")))
df = as.data.frame.matrix(tab)
names(df) = c("EM_S","EM_FI","EM_FII","EM_M", "EM_SI","EM_SC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)


# Raca/Cor da Mãe
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACORMAE, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TGRC_B","TGRC_PT","TGRC_A","TGRC_PD", "TGRC_I")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Estado civil
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro")))
df = as.data.frame.matrix(tab)
names(df) = c("TGSC","TGCC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Primiparidade
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARIDADE, levels = c("Nulípara", "Multípara")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRI","TGNPRI")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


#################################################
# Informações das gestações
#################################################
# Tipo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GRAVIDEZ, levels = c("Única", "Dupla", "Tripla ou mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGU","TEMP1","TEMP2")
df$TGG = df$TEMP1 + df$TEMP2
df$CODMUNRES = rownames(df)

base = merge(base, df[,c("CODMUNRES","TGU","TGG")], by = "CODMUNRES", all.x = TRUE)

# Duração da gestação
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGD_22","TGD_22_27","TGD_28_31","TGD_32_36", "TGD_37_41", "TGD_42")
df$CODMUNRES = rownames(df)

df$TGD_PRT= df$TGD_22 + df$TGD_22_27 + df$TGD_28_31 + df$TGD_32_36 
df$TGD_AT= df$TGD_37_41
df$TGD_PST= df$TGD_42

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Percentis
p_duracao = aggregate(SEMAGESTAC ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_duracao = do.call(data.frame, p_duracao)
names(p_duracao) = c("CODMUNRES","DG_P25","DG_P50","DG_P75")
p_duracao[, c("DG_P25","DG_P50","DG_P75")] = round(p_duracao[, c("DG_P25","DG_P50","DG_P75")], 2)

base = merge(base, p_duracao, by="CODMUNRES", all.x=TRUE)

# Média
media_duracao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_duracao$SEMAGESTAC = round(media_duracao$SEMAGESTAC, 2)
names(media_duracao)[2] = "DG_MD"

# Desvio-padrão
dp_duracao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_duracao$SEMAGESTAC = round(dp_duracao$SEMAGESTAC, 2)
names(dp_duracao)[2] = "DG_DP"
temp = merge(media_duracao, dp_duracao, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Consultas de pre-natal
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$KOTELCHUCK, levels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado")))
df = as.data.frame.matrix(tab)
names(df) = c("TKC_NR","TKC_ID","TKC_IT","TKC_AD", "TKC_MAD")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)


#################################################
# Informações dos partos
#################################################
# Peregrinação
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PEREG, levels = c("Sim","Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRG_S","TGPRG_N")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Tipo de parto
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal", "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV","TPC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Posição do feto
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPAPRESENT, levels = c("Cefálico", "Pélvica ou podálica", "Transversa")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAP_C", "TRAP_P", "TRAP_T")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Grupo de Robson
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPROBSON, levels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10")))
df = as.data.frame.matrix(tab)
names(df) = c("TGROB_1","TGROB_2", "TGROB_3", "TGROB_4", "TGROB_5", "TGROB_6", "TGROB_7", "TGROB_8", "TGROB_9", "TGROB_10")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Local de nascimento
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$LOCNASC, levels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


#################################################
# Informações dos recém-nascidos
#################################################
# Sexo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Masculino","Feminino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRS_M", "TRS_F")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Raça/Cor
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACOR, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TRRC_B","TRRC_PT","TRRC_A", "TRRC_PD", "TRRC_I")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Peso
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia")))
df = as.data.frame.matrix(tab)
names(df) = c("TRP_BP", "TRP_N", "TRP_M")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Percentis
p_peso = aggregate(PESO ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_peso = do.call(data.frame, p_peso)
names(p_peso) = c("CODMUNRES","PESO_P25","PESO_P50","PESO_P75")
p_peso[, c("PESO_P25","PESO_P50","PESO_P75")] = round(p_peso[, c("PESO_P25","PESO_P50","PESO_P75")], 2)

base = merge(base, p_peso, by="CODMUNRES", all.x=TRUE)

# Média
media_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_peso$PESO = round(media_peso$PESO, 2)
names(media_peso)[2] = "PESO_MD"

# Desvio-padrão
dp_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_peso$PESO = round(dp_peso$PESO, 2)
names(dp_peso)[2] = "PESO_DP"
temp = merge(media_peso, dp_peso, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Peso por idade gestacional - gestações únicas
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG")))
df = as.data.frame.matrix(tab)
names(df) = c("TRPIG_P", "TRPIG_A", "TRPIG_G")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Apgar ao 5º minuto
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo","Normal")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAPG5_B","TRAPG5_N")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Média
media_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_apgar5$APGAR5 = round(media_apgar5$APGAR5, 2)
names(media_apgar5)[2] = "APG5_MD"

# Desvio-padrão
dp_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_apgar5$APGAR5 = round(dp_apgar5$APGAR5, 2)
names(dp_apgar5)[2] = "APG5_DP"
temp = merge(media_apgar5, dp_apgar5, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Anomalia congênita
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$IDANOMAL, levels = c("Sim", "Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAC","TRSAC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA

# colunas de contagem: indicar as variáveis contínuas, que por exclusão não terão valores somados
cols_contagem = setdiff(names(base), c("CODMUNRES","IM_P25","IM_P50","IM_P75", "IM_MD","IM_DP", 
                                       "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP", 
                                       "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
                                       "APG5_MD", "APG5_DP"))

linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# medidas para variáveis quantitativas 
# Idade da mãe
linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$IM_P25 = q[1]
linha_estado$IM_P50 = q[2]
linha_estado$IM_P75 = q[3]


# Duração da gestação
linha_estado$DG_MD = round(mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
linha_estado$DG_DP = round(sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$SEMAGESTAC, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$DG_P25 = q[1]
linha_estado$DG_P50 = q[2]
linha_estado$DG_P75 = q[3]


# Peso
linha_estado$PESO_MD = round(mean(dados_sinasc_2$PESO, na.rm = TRUE), 2)
linha_estado$PESO_DP = round(sd(dados_sinasc_2$PESO, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$PESO, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$PESO_P25 = q[1]
linha_estado$PESO_P50 = q[2]
linha_estado$PESO_P75 = q[3]


# Apgar ao 5º minuto
linha_estado$APG5_MD = round(mean(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)
linha_estado$APG5_DP = round(sd(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)


# código da UF e ordem das colunas
linha_estado$CODMUNRES = 12

# Banco de dados final para o Acre
SINASC_AC = rbind(linha_estado, base)

SINASC_AC$NIVEL = c("UF", rep("MUNICIPIO", nrow(SINASC_AC)-1))
SINASC_AC$ANO = 2015

SINASC_AC = SINASC_AC[, c("ANO","NIVEL","CODMUNRES", names(SINASC_AC)[!names(SINASC_AC) %in% c("ANO","NIVEL","CODMUNRES")])]


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv(SINASC_AC, "SINASC_AC.csv", row.names = FALSE)


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"


##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SIM no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

dados_sim <- read.csv("Mortalidade_Geral_2015.csv",
                      header = TRUE,
                      sep = ";",
                      dec = ",")

head(dados_sim)
str(dados_sim)

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

# Tarefa 2. Selecionar variáveis utilizadas

colunas_sim <- c(1, 3, 4, 8, 9, 10, 11, 14, 17,
                 35, 36, 37, 47, 77, 84)

dados_sim_1 <- dados_sim[, colunas_sim]

names(dados_sim_1) <- c(
  "CONTADOR",
  "TIPOBITO",
  "DTOBITO",
  "DTNASC",
  "IDADE",
  "SEXO",
  "RACACOR",
  "ESC2010",
  "CODMUNRES",
  "TPMORTEOCO",
  "OBITOGRAV",
  "OBITOPUERP",
  "CAUSABAS",
  "TPOBITOCOR",
  "MORTEPARTO"
)

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

dados_sim_2 <- subset(
  dados_sim_1,
  substr(as.character(CODMUNRES), 1, 2) == "12"
)

nrow(dados_sim_2)


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

# write.csv(dados_sim_2,
#           "dados_sim_2.csv",
#           row.names = FALSE)

# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

table(dados_sim_2$TIPOBITO)

table(dados_sim_2$SEXO)

table(dados_sim_2$RACACOR)

table(dados_sim_2$TPMORTEOCO)

table(dados_sim_2$OBITOGRAV)

table(dados_sim_2$OBITOPUERP)

table(dados_sim_2$CAUSABAS)

table(dados_sim_2$TPOBITOCOR)

table(dados_sim_2$MORTEPARTO)

# Verificando idade
summary(dados_sim_2$IDADE)

unique(dados_sim_2$IDADE)

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA


# Atribuindo NA para categorias ignoradas

dados_sim_2$IDADE[dados_sim_2$IDADE == 999] = NA

dados_sim_2$SEXO[dados_sim_2$SEXO %in% c(0, 9, "0", "9", "I")] = NA

dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] = NA

dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] = NA

dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] = NA

dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] = NA

dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] = NA

dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] = NA

summary(dados_sim_2)

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# Atribuindo legendas às variáveis qualitativas

# Tarefa 6. Atribuir legendas às variáveis qualitativas

dados_sim_2$TIPOBITO = factor(
  dados_sim_2$TIPOBITO,
  levels = c(1, 2),
  labels = c("Fetal", "Não fetal")
)

dados_sim_2$SEXO = factor(
  dados_sim_2$SEXO,
  levels = c(1, 2, "M", "F"),
  labels = c("Masculino", "Feminino", "Masculino", "Feminino")
)

dados_sim_2$RACACOR = factor(
  dados_sim_2$RACACOR,
  levels = c(1,2,3,4,5),
  labels = c("Branca","Preta","Amarela","Parda","Indígena")
)

dados_sim_2$ESC2010 = factor(
  dados_sim_2$ESC2010,
  levels = c(0,1,2,3,4,5),
  labels = c(
    "Sem escolaridade",
    "Fundamental I",
    "Fundamental II",
    "Médio",
    "Superior incompleto",
    "Superior completo"
  )
)

dados_sim_2$TPMORTEOCO = factor(
  dados_sim_2$TPMORTEOCO,
  levels = c(1,2,3,4,5,8),
  labels = c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto",
    "De 43 dias a 1 ano após o término da gestação",
    "Não ocorreu nestes períodos"
  )
)

dados_sim_2$OBITOGRAV = factor(
  dados_sim_2$OBITOGRAV,
  levels = c(1,2),
  labels = c("Sim","Não")
)

dados_sim_2$OBITOPUERP = factor(
  dados_sim_2$OBITOPUERP,
  levels = c(1,2,3),
  labels = c(
    "Sim, até 42 dias após o parto",
    "Sim, de 43 dias a 1 ano",
    "Não"
  )
)

dados_sim_2$MORTEPARTO = factor(
  dados_sim_2$MORTEPARTO,
  levels = c(1,2,3),
  labels = c(
    "Antes",
    "Durante",
    "Após"
  )
)

dados_sim_2$TPOBITOCOR = factor(
  dados_sim_2$TPOBITOCOR,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "Durante a gestação",
    "Durante o abortamento",
    "Após o abortamento",
    "No parto ou até 1 hora após o parto",
    "No puerpério - até 42 dias após o parto",
    "Entre 43 dias e até 1 ano após o parto",
    "A investigação não identificou o momento do óbito",
    "Mais de um ano após o parto",
    "O óbito não ocorreu nas circunstancias anteriores"
  )
)

table(dados_sim_2$TPOBITOCOR)

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

library(tidyverse)
library(dplyr)

# Criação de variaveis auxiliares para a variavel idade:

# Converte IDADE (inteiro) → idade em DIAS (para períodos neonatais / pós-neonatais)
idade_em_dias <- function(idade) {
  # Garante 3 dígitos (preenche zeros à esquerda implícitos)
  unidade <- idade %/% 100        # 1º dígito
  valor   <- idade  %% 100        # 2º-3º dígitos
  
  dplyr::case_when(
    is.na(idade)   ~ NA_real_,
    unidade == 0   ~ 0,                  # minutos  → 0 dias
    unidade == 1   ~ 0,                  # horas    → 0 dias
    unidade == 2   ~ as.numeric(valor),  # dias     → valor dias
    unidade == 3   ~ as.numeric(valor) * 30,  # meses (aprox.)
    unidade == 4   ~ as.numeric(valor) * 365, # anos
    unidade == 5   ~ (100 + as.numeric(valor)) * 365, # >100 anos
    TRUE           ~ NA_real_
  )
}

# Converte IDADE → idade em ANOS (para óbitos maternos / idade fértil)
# Mulher em idade fértil: 415 <= IDADE <= 449  (15 a 49 anos)
idade_em_anos <- function(idade) {
  unidade <- idade %/% 100
  valor   <- idade  %% 100
  dplyr::case_when(
    is.na(idade)   ~ NA_real_,
    unidade == 4   ~ as.numeric(valor),
    unidade == 5   ~ 100 + as.numeric(valor),
    TRUE           ~ 0   # minutos/horas/dias/meses → < 1 ano
  )
}

# ── Pré-processamento: criar colunas auxiliares ───────────────
dados_sim_2 <- dados_sim_2 |>
  mutate(
    # Idade em dias e em anos (vetorizados)
    IDADE_DIAS = idade_em_dias(IDADE),
    IDADE_ANOS = idade_em_anos(IDADE),
    
    # Flag de registros completos
    COMPLETO_87  = complete.cases(dados_sim[match(CONTADOR,
                                                  dados_sim$CONTADOR), ]),
    COMPLETO_14  = complete.cases(dados_sim_2[, c("CONTADOR","TIPOBITO","DTOBITO",
                                                  "DTNASC","IDADE","SEXO","RACACOR",
                                                  "ESC2010","CODMUNRES","TPMORTEOCO",
                                                  "OBITOGRAV","OBITOPUERP","CAUSABAS",
                                                  "TPOBITOCOR","MORTEPARTO")]),
    
    # Causas de óbito (1º caractere de CAUSABAS)
    CAUSABAS_INI = substr(as.character(CAUSABAS), 1, 1),
    
    # Óbito materno precoce:
    # TPMORTEOCO %in% c("Na gravidez","No parto","No abortamento","Até 42 dias após o término do parto")
    OBITO_MAT_PREC = TPMORTEOCO %in% c(
      "Na gravidez",
      "No parto",
      "No abortamento",
      "Até 42 dias após o término do parto"
    ),
    
    # Idade fértil (anos): 15 <= anos <= 49
    IDADE_FERTIL = IDADE_ANOS >= 15 & IDADE_ANOS <= 49,
    
    # Período neonatal: 0 a 27 dias
    # IDADE <= 123 (até 23 horas) OU 200 <= IDADE <= 227
    NEONATAL = (!is.na(IDADE)) & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)),
    
    # Neonatal precoce: 0-6 dias → IDADE <= 123 OU 200 <= IDADE <= 206
    NEONATAL_PREC = (!is.na(IDADE)) & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 206)),
    
    # Neonatal tardio: 7-27 dias → 207 <= IDADE <= 227
    NEONATAL_TARD = (!is.na(IDADE)) & (IDADE >= 207 & IDADE <= 227),
    
    # Pós-neonatal: 28-364 dias → 228 <= IDADE <= 264 (dias) e meses < 12
    # Em dias: 28-364 dias; em código: dias 228-264 não existe (máx 229 dias=29 dias),
    # então precisamos incluir meses: 301-311 (1 a 11 meses) + dias restantes
    # Simplificando: IDADE_DIAS entre 28 e 364
    POS_NEONATAL = (!is.na(IDADE_DIAS)) & (IDADE_DIAS >= 28 & IDADE_DIAS <= 364),
    
    # Óbito fetal
    FETAL = TIPOBITO == "Fetal"
  )

# Função de resumo SIM
resumoVariaveisSIM <- function(df) {
  df |> summarise(
    # Informações Gerais
    TO        = n(),
    TORC      = sum(COMPLETO_87,  na.rm = TRUE),
    TORCR     = sum(COMPLETO_14,  na.rm = TRUE),
    
    TO_NN     = sum(CAUSABAS_INI %in% c("V","W","X","Y"), na.rm = TRUE),
    TO_N      = sum(!CAUSABAS_INI %in% c("V","W","X","Y"), na.rm = TRUE),
    
    TO_CB_I   = sum(CAUSABAS_INI %in% c("A","B"), na.rm = TRUE),
    TO_CB_N   = sum(CAUSABAS_INI %in% c("C","D"), na.rm = TRUE),
    TO_CB_C   = sum(CAUSABAS_INI == "I",           na.rm = TRUE),
    TO_CB_R   = sum(CAUSABAS_INI == "J",           na.rm = TRUE),
    TO_CB_O   = sum(!CAUSABAS_INI %in% c("A","B","C","D","I","J","V","W","X","Y"),
                    na.rm = TRUE),
    
    TO_M      = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F      = sum(SEXO == "Feminino",  na.rm = TRUE),
    TO_F_IF   = sum(SEXO == "Feminino" & IDADE_FERTIL, na.rm = TRUE),
    
    # Fetais e Neonatais 
    TO_FT     = sum(FETAL,          na.rm = TRUE),
    TO_NT     = sum(!FETAL & NEONATAL,      na.rm = TRUE),
    TO_NT_P   = sum(!FETAL & NEONATAL_PREC, na.rm = TRUE),
    TO_NT_T   = sum(!FETAL & NEONATAL_TARD, na.rm = TRUE),
    TO_PNT    = sum(!FETAL & POS_NEONATAL,  na.rm = TRUE),
    
    TO_MT_G   = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),  
    
    TONT_B    = sum(!FETAL & NEONATAL & RACACOR == "Branca",   na.rm = TRUE),
    TONT_PT   = sum(!FETAL & NEONATAL & RACACOR == "Preta",    na.rm = TRUE),
    TONT_A    = sum(!FETAL & NEONATAL & RACACOR == "Amarela",  na.rm = TRUE),
    TONT_PD   = sum(!FETAL & NEONATAL & RACACOR == "Parda",    na.rm = TRUE),
    TONT_I    = sum(!FETAL & NEONATAL & RACACOR == "Indígena", na.rm = TRUE),
    
    # Maternas 
    TO_MT     = sum(!is.na(TPMORTEOCO) & TPMORTEOCO != "Não ocorreu nestes períodos",
                    na.rm = TRUE),
    TO_MT_DG  = sum(TPMORTEOCO == "Na gravidez",                                        na.rm = TRUE),
    TO_MT_PT  = sum(TPMORTEOCO == "No parto",                                           na.rm = TRUE),
    TO_MT_AB  = sum(TPMORTEOCO == "No abortamento",                                     na.rm = TRUE),
    TO_MT_42  = sum(TPMORTEOCO == "Até 42 dias após o término do parto",                na.rm = TRUE),
    TO_MT_43  = sum(TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação",      na.rm = TRUE),
    
    TO_MT_P   = sum(OBITO_MAT_PREC, na.rm = TRUE),
    
    TO_MT_P_I = sum(OBITO_MAT_PREC & IDADE_FERTIL,                        na.rm = TRUE),
    TO_MT_P_ES  = sum(OBITO_MAT_PREC & ESC2010 == "Sem escolaridade",     na.rm = TRUE),
    TO_MT_P_EFI = sum(OBITO_MAT_PREC & ESC2010 == "Fundamental I",        na.rm = TRUE),
    TO_MT_P_EFII= sum(OBITO_MAT_PREC & ESC2010 == "Fundamental II",       na.rm = TRUE),
    TO_MT_P_EM  = sum(OBITO_MAT_PREC & ESC2010 == "Médio",                na.rm = TRUE),
    TO_MT_P_ESI = sum(OBITO_MAT_PREC & ESC2010 == "Superior incompleto",  na.rm = TRUE),
    TO_MT_P_ESC = sum(OBITO_MAT_PREC & ESC2010 == "Superior completo",    na.rm = TRUE)
  )
}

# Agregação Por município
resumo_municipios_sim <- dados_sim_2 |>
  mutate(CODMUNRES = as.character(CODMUNRES)) |>
  group_by(CODMUNRES) |>
  resumoVariaveisSIM() |>
  mutate(ANO = 2015, NIVEL = "MUNICIPIO") |>
  ungroup() |>
  select(ANO, NIVEL, CODMUNRES, everything())

# Agregando Toda a UF
resumo_uf_sim <- dados_sim_2 |>
  mutate(CODMUNRES = "12") |>
  group_by(CODMUNRES) |>
  resumoVariaveisSIM() |>
  mutate(ANO = 2015, NIVEL = "UF") |>
  ungroup() |>
  select(ANO, NIVEL, CODMUNRES, everything())

# Empilhando
SIM_AC <- bind_rows(resumo_uf_sim, resumo_municipios_sim)

nrow(SIM_AC)   # deve ser n_municipios + 1
ncol(SIM_AC)   # deve ser 41 + 3 identificadores = 44... ajuste se necessário


# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

write.csv(SIM_AC, "SIM_AC.csv", row.names = FALSE)

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

library(dplyr)

# Leitura dos 4 arquivos 

# 1. População estimada 2015 (UF + municípios)
tab_6579 <- read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv",
                     sep = ";", header = TRUE,
                     col.names = c("CODMUNRES", "NOME", "POPRE_T"),
                     encoding = "UTF-8")

# 2. População Censo 2010 – total e por sexo (UF + municípios)
tab_1552_sexo <- read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv",
                          sep = ";", header = TRUE,
                          col.names = c("CODMUNRES", "NOME", "POPRC_T", "POPRC_M", "POPRC_F"),
                          encoding = "UTF-8")

# 3. População Censo 2010 – faixa etária e sexo – UF
tab_1552_faixa_uf <- read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv",
                              sep = ";", header = TRUE,
                              col.names = c("CODMUNRES", "ESTADO", "F_IDADE", "POP", "POPM", "POPF"),
                              encoding = "UTF-8")

# 4. População Censo 2010 – faixa etária e sexo – municípios
tab_1552_faixa_mun <- read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv",
                               sep = ";", header = TRUE,
                               col.names = c("CODMUNRES", "F_IDADE", "POP", "POPM", "POPF"),
                               encoding = "UTF-8")

# Filtrando apenas Acre (CODMUNRES começa com "12") 

# Função auxiliar para filtrar AC
filtrar_ac <- function(df) {
  df[substr(as.character(df$CODMUNRES), 1, 2) == "12", ]
}

tab_6579_ac        <- filtrar_ac(tab_6579)
tab_1552_sexo_ac   <- filtrar_ac(tab_1552_sexo)
tab_1552_faixa_uf_ac  <- tab_1552_faixa_uf[tab_1552_faixa_uf$CODMUNRES == 12, ]
tab_1552_faixa_mun_ac <- filtrar_ac(tab_1552_faixa_mun)

# Faixas etárias de interesse 
# POPRC_15  - < 15 anos:  "0 a 4 anos", "5 a 9 anos", "10 a 14 anos"
# POPRC_15_49 - 15 a 49:   "15 a 19 anos" ... "45 a 49 anos"
# POPRC_50    - >= 50 anos: "50 a 54 anos" ... "100 anos ou mais"

faixas_lt15  <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")
faixas_15_49 <- c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                  "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")
faixas_50    <- c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
                  "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
                  "80 a 89 anos", "90 a 99 anos", "100 anos ou mais")

# Função para agregar faixas etárias por CODMUNRES 
agregar_faixas <- function(df_faixa) {
  df_faixa <- df_faixa[!is.na(df_faixa$F_IDADE), ]
  
  df_faixa |>
    group_by(CODMUNRES) |>
    summarise(
      POPRC_15    = sum(POP [F_IDADE %in% faixas_lt15 ], na.rm = TRUE),
      POPRC_15_49 = sum(POP [F_IDADE %in% faixas_15_49], na.rm = TRUE),
      POPRC_50    = sum(POP [F_IDADE %in% faixas_50   ], na.rm = TRUE),
      POPRC_F_15    = sum(POPF[F_IDADE %in% faixas_lt15 ], na.rm = TRUE),
      POPRC_F_15_49 = sum(POPF[F_IDADE %in% faixas_15_49], na.rm = TRUE),
      POPRC_F_50    = sum(POPF[F_IDADE %in% faixas_50   ], na.rm = TRUE),
      .groups = "drop"
    )
}

# Agregando faixas para UF e para municípios
faixas_uf  <- agregar_faixas(tab_1552_faixa_uf_ac)
faixas_mun <- agregar_faixas(tab_1552_faixa_mun_ac)

# Montando linha da UF 
uf_base <- tab_6579_ac[tab_6579_ac$CODMUNRES == 12, c("CODMUNRES", "POPRE_T")]
uf_sexo <- tab_1552_sexo_ac[tab_1552_sexo_ac$CODMUNRES == 12,
                            c("CODMUNRES", "POPRC_T", "POPRC_M", "POPRC_F")]

linha_uf <- uf_base |>
  left_join(uf_sexo,   by = "CODMUNRES") |>
  left_join(faixas_uf, by = "CODMUNRES") |>
  mutate(ANO = 2015, NIVEL = "UF", CODMUNRES = as.character(CODMUNRES)) |>
  select(ANO, NIVEL, CODMUNRES, POPRE_T, POPRC_T, POPRC_M, POPRC_F,
         POPRC_15, POPRC_15_49, POPRC_50,
         POPRC_F_15, POPRC_F_15_49, POPRC_F_50)

# Montando linhas dos municípios 
mun_base <- tab_6579_ac[tab_6579_ac$CODMUNRES != 12, c("CODMUNRES", "POPRE_T")]
mun_sexo <- tab_1552_sexo_ac[tab_1552_sexo_ac$CODMUNRES != 12,
                             c("CODMUNRES", "POPRC_T", "POPRC_M", "POPRC_F")]

linhas_mun <- mun_base |>
  left_join(mun_sexo,   by = "CODMUNRES") |>
  left_join(faixas_mun, by = "CODMUNRES") |>
  mutate(ANO = 2015, NIVEL = "MUNICIPIO", CODMUNRES = as.character(CODMUNRES)) |>
  select(ANO, NIVEL, CODMUNRES, POPRE_T, POPRC_T, POPRC_M, POPRC_F,
         POPRC_15, POPRC_15_49, POPRC_50,
         POPRC_F_15, POPRC_F_15_49, POPRC_F_50)

# Empilhar UF + municípios 
SIDRA_AC <- bind_rows(linha_uf, linhas_mun)

nrow(SIDRA_AC)  
ncol(SIDRA_AC)  

# Exportando o arquivo 
write.csv(SIDRA_AC, "SIDRA_AC.csv", row.names = FALSE)

####################################################################################

# Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"


library(dplyr)

# Lendo o arquivo
sinisa <- read.csv("agua e esgoto - município - 2015.csv",
                   sep = ";", header = TRUE,
                   col.names = c("CODMUNRES_6", "Municipio", "Estado", "Ano", "POPR_RA", "POPR_RE"),
                   encoding = "UTF-8")

# Filtrando Acre 
sinisa_ac <- sinisa[sinisa$Estado == "AC", ]

# tirando os pontos como separador de milhar e convertendo vírgula para ponto nos valores numéricos 
sinisa_ac$POPR_RA <- as.numeric(gsub("\\.", "", gsub(",", ".", sinisa_ac$POPR_RA)))
sinisa_ac$POPR_RE <- as.numeric(gsub("\\.", "", gsub(",", ".", sinisa_ac$POPR_RE)))

# O SINISA usa código de 6 dígitos (sem dígito verificador)
# Convertendo para 7 dígitos adicionando o dígito verificador via tabela de códigos
cod_mun <- read.csv("códigos dos municípios - 2010.csv",
                    sep = ";", header = TRUE,
                    col.names = c("municipio", "CODMUNRES", "extra"),
                    encoding = "UTF-8")

cod_ac <- cod_mun[substr(as.character(cod_mun$CODMUNRES), 1, 2) == "12", 
                  c("municipio", "CODMUNRES")]

# Criar código de 6 dígitos na tabela de códigos para fazer o join
cod_ac$CODMUNRES_6 <- as.integer(substr(as.character(cod_ac$CODMUNRES), 1, 6))

# fazendo merge para obter CODMUNRES de 7 dígitos
sinisa_ac <- sinisa_ac |>
  left_join(cod_ac[, c("CODMUNRES_6", "CODMUNRES")], by = "CODMUNRES_6")

# Montando banco final

# Linha da UF (soma dos municípios)
linha_uf_sin <- sinisa_ac |>
  summarise(
    POPR_RA = sum(POPR_RA, na.rm = TRUE),
    POPR_RE = sum(POPR_RE, na.rm = TRUE)
  ) |>
  mutate(ANO = 2015, NIVEL = "UF", CODMUNRES = "12") |>
  select(ANO, NIVEL, CODMUNRES, POPR_RA, POPR_RE)

# Linhas dos municípios
linhas_mun_sin <- sinisa_ac |>
  mutate(ANO = 2015, NIVEL = "MUNICIPIO", CODMUNRES = as.character(CODMUNRES)) |>
  select(ANO, NIVEL, CODMUNRES, POPR_RA, POPR_RE)

# Empilhar
SINISA_AC <- bind_rows(linha_uf_sin, linhas_mun_sin)

nrow(SINISA_AC)  
ncol(SINISA_AC)  

# ── Exportar ──────────────────────────────────────────────────
write.csv(SINISA_AC, "SINISA_AC.csv", row.names = FALSE)

############################################################################

# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv
# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"



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

