url <- "/home/jerry/Documents/UFAL/Statistics/Descriptive-Statistics-ENEM-2022/ENEM_AL_R_2022.csv"

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

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Desenha o box plot de uma coluna mostrando os valores de interesse
display_single_box <- function(column, title, color){
  b_plot <- boxplot(column, main=title, col=color)
  values <- c(min(column), b_plot$stats, max(column))
  text(y=values, x=1.25, labels=values)
} 

# Observando boxplots singularmente
display_single_box(enem$NU_NOTA_CN, "Ciencias da Natureza", "lemonchiffon")

display_single_box(enem$NU_NOTA_CH, "Ciencias Humanas", "khaki")

display_single_box(enem$NU_NOTA_LC, "Linguagens e Codigos", "navajowhite2")

display_single_box(enem$NU_NOTA_MT, "Matematica", "lightsalmon")

display_single_box(enem$NU_NOTA_REDACAO, "Ciencias Humanas", "coral")

display_single_box(enem$NU_NOTA_ENEM, "Nota ENEM", "indianred2")

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Limites para as classes das frequencias
limites <- seq(from=0, to=1000, by=100)
intervalos <- c("0 |-- 100", "100 |-- 200", "200 |-- 300", "300 |-- 400",
                "400 |-- 500", "500 |-- 600", "600 |-- 700", "700 |-- 800", 
                "800 |-- 900", "900 |-- 1000")

# Caluclando frequencias absolutas, relativas, e acumuladas
mathFreq <- table(cut(enem$NU_NOTA_MT, breaks=limites, right=F, labels=intervalos))
mathFreq_rel <- round(mathFreq / length(enem$NU_NOTA_MT) * 100, 2)
mathFreq_cum <- cumsum(mathFreq)
mathFreq_rel_cum <- cumsum(mathFreq_rel)

mathTable <- cbind(mathFreq, mathFreq_rel, mathFreq_cum, mathFreq_rel_cum)

redacFreq <- table(cut(enem$NU_NOTA_REDACAO, breaks=limites, right=F, labels=intervalos))
redacFreq_rel <- round(redacFreq / length(enem$NU_NOTA_REDACAO) * 100, 2)
redacFreq_cum <- cumsum(redacFreq)
redacFreq_rel_cum <- cumsum(redacFreq_rel)

redacTable <- cbind(redacFreq, redacFreq_rel, redacFreq_cum, redacFreq_rel_cum)

mathTable
redacTable

barplot(mathFreq_rel, space=F, main="Frequencias das notas de matematica", ylim=c(0, 50), xlab="Range notas", ylab="Frequencia (%)", col="lightsalmon")
barplot(redacFreq_rel, space=F, main="Frequencias das notas de redaçao", ylim=c(0, 30), xlab="Range notas", ylab="Frequencia (%)", col="coral")


