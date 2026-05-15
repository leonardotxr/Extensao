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

dados_sinasc <- read.csv("SINASC_2015.csv", header = TRUE, sep = ";", dec = ",") 
head(dados_sinasc)
str(dados_sinasc)

# Filtrar o banco original inteiro para o AC (código 12 antes de excluir colunas
dados_sinasc_ac <- subset(dados_sinasc, substr(as.character(CODMUNRES), 1, 2) == "12")

# Prepara a verificação para TNRC (Registros completos nas 61 variáveis originais)
completo_61 <- complete.cases(dados_sinasc_ac)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

colunas_selecionadas <- c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)
dados_sinasc_1 <- dados_sinasc_ac[, colunas_selecionadas]

names(dados_sinasc_1) <- c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", 
                           "CODMUNRES", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", 
                           "APGAR5", "RACACOR", "PESO", "IDANOMAL", "ESCMAE2010", 
                           "RACACORMAE", "SEMAGESTAC", "CONSPRENAT", "TPAPRESENT", 
                           "TPROBSON", "PARIDADE", "KOTELCHUCK")

# Prepara a verificação para TNRCR (Registros completos nas 22 variáveis)
completo_22 <- complete.cases(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 11: 27918     12: 16980     13: 80097     14: 11412     15: 143657    16: 15750      17: 25110

# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 


# Exportar o arquivo com o nome dados_sinasc_2.csv



dados_sinasc_2 <- dados_sinasc_1
dados_sinasc_2$COMPLETO_61 <- completo_61
dados_sinasc_2$COMPLETO_22 <- completo_22


# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
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
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA


# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Em variáveis quantitativas como IDADEMAE, CONSPRENAT, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0 | dados_sinasc_2$SEXO == 9] = NA
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


# Leitura da Tabela PIG
tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", sep = ";", dec = ",")
tabela_pig$SEXO <- factor(trimws(tabela_pig$SEXO), levels = c("Masculino", "Feminino"))

# Merge
dados_sinasc_2 <- merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

# Criação de F_PIG respeitando Gravidez Única e lidando com NAs
dados_sinasc_2$F_PIG <- ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                               ifelse(is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90), NA,
                                      ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                             ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG")
                                      )
                               )
)
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

# Função para agregar na base as 103 variáveis 
resumoVariaveis <- function(df) {
  df |> summarise(
    TN = n(),
    TNRC = sum(COMPLETO_61, na.rm = TRUE),
    TNRCR = sum(COMPLETO_22, na.rm = TRUE),
    
    TGI_15 = sum(IDADEMAE < 15, na.rm = TRUE),
    TGI_15_19 = sum(IDADEMAE >= 15 & IDADEMAE <= 19, na.rm = TRUE),
    TGI_20_24 = sum(IDADEMAE >= 20 & IDADEMAE <= 24, na.rm = TRUE),
    TGI_25_29 = sum(IDADEMAE >= 25 & IDADEMAE <= 29, na.rm = TRUE),
    TGI_30_34 = sum(IDADEMAE >= 30 & IDADEMAE <= 34, na.rm = TRUE),
    TGI_35_39 = sum(IDADEMAE >= 35 & IDADEMAE <= 39, na.rm = TRUE),
    TGI_40_44 = sum(IDADEMAE >= 40 & IDADEMAE <= 44, na.rm = TRUE),
    TGI_45_49 = sum(IDADEMAE >= 45 & IDADEMAE <= 49, na.rm = TRUE),
    TGI_50 = sum(IDADEMAE >= 50, na.rm = TRUE),
    TGIF = sum(IDADEMAE >= 15 & IDADEMAE <= 49, na.rm = TRUE),
    
    IM_P25 = quantile(IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(IDADEMAE, 0.50, na.rm = TRUE),
    IM_P75 = quantile(IDADEMAE, 0.75, na.rm = TRUE),
    IM_MD = mean(IDADEMAE, na.rm = TRUE),
    IM_DP = sd(IDADEMAE, na.rm = TRUE),
    
    EM_S = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(ESCMAE2010 == "Fundamental I", na.rm = TRUE),
    EM_FII = sum(ESCMAE2010 == "Fundamental II", na.rm = TRUE),
    EM_M = sum(ESCMAE2010 == "Médio", na.rm = TRUE),
    EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    TGRC_B = sum(RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A = sum(RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I = sum(RACACORMAE == "Indígena", na.rm = TRUE),
    
    TGSC = sum(ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(ESTCIV == "Com companheiro", na.rm = TRUE),
    TGPRI = sum(PARIDADE == "Nenhuma", na.rm = TRUE),
    TGNPRI = sum(PARIDADE == "Uma ou mais", na.rm = TRUE),
    
    TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(GRAVIDEZ %in% c("Dupla", "Tríplice+"), na.rm = TRUE),
    
    TGD_22 = sum(GESTACAO == "Menos de 22 sem", na.rm = TRUE),
    TGD_22_27 = sum(GESTACAO == "22-27 sem", na.rm = TRUE),
    TGD_28_31 = sum(GESTACAO == "28-31 sem", na.rm = TRUE),
    TGD_32_36 = sum(GESTACAO == "32-36 sem", na.rm = TRUE),
    TGD_37_41 = sum(GESTACAO == "37-41 sem", na.rm = TRUE),
    TGD_42 = sum(GESTACAO == "42+ sem", na.rm = TRUE),
    
    TGD_PRT = sum(SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT = sum(SEMAGESTAC >= 37 & SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(SEMAGESTAC >= 42, na.rm = TRUE),
    
    DG_P25 = quantile(SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(SEMAGESTAC, 0.50, na.rm = TRUE),
    DG_P75 = quantile(SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD = mean(SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(SEMAGESTAC, na.rm = TRUE),
    
    TKC_NR = sum(KOTELCHUCK == "Não realizou", na.rm = TRUE),
    TKC_ID = sum(KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
    
    TGPRG_S = sum(PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(PERIG == "Não", na.rm = TRUE),
    
    TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(PARTO == "Cesáreo", na.rm = TRUE),
    
    TRAP_C = sum(TPAPRESENT == "Cefálica", na.rm = TRUE),
    TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE),
    
    TGROB_1 = sum(TPROBSON == 1, na.rm = TRUE),
    TGROB_2 = sum(TPROBSON == 2, na.rm = TRUE),
    TGROB_3 = sum(TPROBSON == 3, na.rm = TRUE),
    TGROB_4 = sum(TPROBSON == 4, na.rm = TRUE),
    TGROB_5 = sum(TPROBSON == 5, na.rm = TRUE),
    TGROB_6 = sum(TPROBSON == 6, na.rm = TRUE),
    TGROB_7 = sum(TPROBSON == 7, na.rm = TRUE),
    TGROB_8 = sum(TPROBSON == 8, na.rm = TRUE),
    TGROB_9 = sum(TPROBSON == 9, na.rm = TRUE),
    TGROB_10 = sum(TPROBSON == 10, na.rm = TRUE),
    
    TNLOC_H = sum(LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(LOCNASC == "Outro estabelecimento", na.rm = TRUE),
    TNLOC_D = sum(LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(LOCNASC == "Aldeia Indígena", na.rm = TRUE),
    
    TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(SEXO == "Feminino", na.rm = TRUE),
    
    TRRC_B = sum(RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
    TRRC_A = sum(RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
    TRRC_I = sum(RACACOR == "Indígena", na.rm = TRUE),
    
    TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M = sum(F_PESO == "Macrossomia", na.rm = TRUE),
    
    PESO_P25 = quantile(PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(PESO, 0.50, na.rm = TRUE),
    PESO_P75 = quantile(PESO, 0.75, na.rm = TRUE),
    PESO_MD = mean(PESO, na.rm = TRUE),
    PESO_DP = sd(PESO, na.rm = TRUE),
    
    TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
    
    TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
    
    APG5_MD = mean(APGAR5, na.rm = TRUE),
    APG5_DP = sd(APGAR5, na.rm = TRUE),
    
    TRAC = sum(IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE)
  )
}

# Agregação por Municípios com a correção de CODMUNRES como caractere (evita erro no bind)
resumo_municipios <- dados_sinasc_2 |>
  mutate(CODMUNRES = as.character(CODMUNRES)) |> 
  group_by(CODMUNRES) |>
  resumoVariaveis() |>
  mutate(ANO = 2015, NIVEL = "MUNICIPIO") |>
  ungroup() |>
  select(ANO, NIVEL, CODMUNRES, everything())

# Agregação da UF (agrupando todos os municípios)
resumo_uf <- dados_sinasc_2 |>
  mutate(CODMUNRES = "12") |> 
  group_by(CODMUNRES) |>
  resumoVariaveis() |>
  mutate(ANO = 2015, NIVEL = "UF") |>
  ungroup() |>
  select(ANO, NIVEL, CODMUNRES, everything())

# Empilhar linhas e Exportar
SINASC_AC <- bind_rows(resumo_uf, resumo_municipios)

nrow(SINASC_AC)
ncol(SINASC_AC)

# Tarefa 11: Exportando o banco de dados com o nome SINASC_AC.csv

write.csv(base_final, "SINASC_AC.csv", row.names = FALSE)


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

