url <- "/home/jerry/Documents/UFAL/Statistics/ENEM_AL_R_2022.csv"

enem <- read.csv(url, header = TRUE, sep = ";")

lapply(enem,class)
# Todas as notas menos a nota de redaçao que è de top "integer" sao 
# de tipo "character". Os valores precisam ser convertidos em numeros e as 
# virgulas precisam serem transformadas em pontos
enem$NU_NOTA_ENEM <- as.numeric(gsub(",", ".", enem$NU_NOTA_ENEM)) # Media do enem
enem$NU_NOTA_MT <- as.numeric(gsub(",", ".", enem$NU_NOTA_MT)) # Matematica
enem$NU_NOTA_LC <- as.numeric(gsub(",", ".", enem$NU_NOTA_LC)) # Linguagens e codigos
enem$NU_NOTA_CH <- as.numeric(gsub(",", ".", enem$NU_NOTA_CH)) # Ciencias humanas
enem$NU_NOTA_CN <- as.numeric(gsub(",", ".", enem$NU_NOTA_CN)) # Ciencias da natureza

# Dataframe que vai ser usado para mostrar todos os box em um unico grafico
notas <- data.frame(ciencias_natureza=enem$NU_NOTA_CN, 
                   ciencias_humanas=enem$NU_NOTA_CH, 
                   linguagens_codigos=enem$NU_NOTA_LC, 
                   matematica=enem$NU_NOTA_MT,
                   redacao=enem$NU_NOTA_REDACAO,
                   nota_emen=enem$NU_NOTA_ENEM)

boxplot(notas, main="Notas de cada prova", xlab="Materias", ylab="Nota", 
        col=c("lemonchiffon", "khaki", "navajowhite2", "lightsalmon", "coral", "indianred2"))

# Limites para as classes das frequencias
limites <- seq(from=0, to=1000, by=100)
intervalos <- c("0 |-- 100", "100 |-- 200", "200 |-- 300", "300 |-- 400",
                "400 |-- 500", "500 |-- 600", "600 |-- 700", "700 |-- 800", 
                "800 |-- 900", "900 |-- 1000")

mathFreq <- table(cut(enem$NU_NOTA_MT, breaks=limites, right=F, labels=intervalos))
redacFreq <- table(cut(enem$NU_NOTA_REDACAO, breaks=limites, right=F, labels=intervalos))
