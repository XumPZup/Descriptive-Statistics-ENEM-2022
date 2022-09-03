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

display_single_box(enem$NU_NOTA_REDACAO, "Redaçao", "coral")

display_single_box(enem$NU_NOTA_ENEM, "Nota ENEM", "indianred2")

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Limites para as classes das frequencias
limites <- seq(from=0, to=1000, by=100)
intervalos <- c("0 |-- 100", "100 |-- 200", "200 |-- 300", "300 |-- 400",
                "400 |-- 500", "500 |-- 600", "600 |-- 700", "700 |-- 800", 
                "800 |-- 900", "900 |-- 1000")

# Calucla frequencias absolutas, relativas, e acumuladas, para uma coluna
make_frequency_table <- function(column, limits, intervals, right){ 
  freq <- table(cut(column, breaks=limits, right=right, labels=intervals))
  freq_rel <- round(freq / length(column) * 100, 2)
  freq_cum <- cumsum(freq)
  freq_rel_cum <- cumsum(freq_rel)
  
  return(cbind(freq, freq_rel, freq_cum, freq_rel_cum))
}

mathTable <- make_frequency_table(enem$NU_NOTA_MT, limites, intervalos, F)

redacTable <- make_frequency_table(enem$NU_NOTA_REDACAO, limites, intervalos, F)

mathTable
redacTable

barplot(mathTable[,2], space=F, main="Frequencias das notas de matematica", ylim=c(0, 50), xlab="Range notas", ylab="Frequencia (%)", col="lightsalmon")
barplot(redacTable[,2], space=F, main="Frequencias das notas de redaçao", ylim=c(0, 30), xlab="Range notas", ylab="Frequencia (%)", col="coral")

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Limites para as classes das frequencias
limites <- c(0, 300, 400, 500, 600, 700, 1000)
intervalos <- c("0 |-- 300", "300 |-- 400", "400 |-- 500", "500 |-- 600",
                "600 |-- 700","700 |-- 1000")

# Caluclando frequencias absolutas, relativas, e acumuladas
enemTable <- make_frequency_table(enem$NU_NOTA_ENEM, limites, intervalos, F)

enemTable

barplot(enemTable[,2], space=F, main="Frequencias das notas do ENEM", ylim=c(0, 50), xlab="Range notas", ylab="Frequencia (%)", col="indianred2")

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Analisando relaçao entre o tipo de escola (Publica/Particular) e as notas da prova de redaçao
limites <- seq(from=0, to=1000, by=200)
intervalos <- c("0 |-- 200", "200 |-- 400", "400 |-- 600", "600 |-- 800", "800 |-- 1000")

grade_school <- table(enem$TP_ESCOLA, cut(enem$NU_NOTA_REDACAO, breaks=limites, right=F, labels=intervalos))
grade_school

# Transoftamdo a frequencia absoluta em frequencia relativa
grade_school[1,] <- round(grade_school[1,] / sum(grade_school[1,])*100, 2)
grade_school[2,] <- round(grade_school[2,] / sum(grade_school[2,])*100, 2)

grade_school
barplot(grade_school, space=F, main="Frequencias das notas de redaçao por tipo de escola", 
        ylim=c(0, 100), xlab="Range notas", ylab="Frequencia (%)", 
        legend.text=c("Escola Publica", "Escola Particular"), col=c("indianred2", "lightsalmon"))

pie(grade_school[1,], main="Prova de redaçao escola publica", labels=paste0(intervalos, " = ", grade_school[1,], " %") )
pie(grade_school[2,], main="Prova de redaçao escola particular", labels=paste0(intervalos, " = ", grade_school[2,], " %") )

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################

# Correlaçao entre idade e nota do ENEM
cor(x=enem$NU_IDADE, y=enem$NU_NOTA_ENEM)
cor(x=enem$NU_IDADE, y=enem$NU_NOTA_ENEM, method="kendall")
cor(x=enem$NU_IDADE, y=enem$NU_NOTA_ENEM, method="spearman")

q_idades = quantile(enem$NU_IDADE)
# segundo e terceiro quantil soa iguais
q_idades[4] <- q_idades[4]+1
idade <- cut(enem$NU_IDADE, breaks=q_idades, include.lowest=T)
nota <- cut(enem$NU_NOTA_ENEM, breaks=quantile(enem$NU_NOTA_ENEM), include.lowest=T)

idade_notas <- table(idade, nota)
idade_notas
# Gerando tabela de frequencias relativas
freq_idade_notas <- idade_notas
n <- 4
for(i in 1:n){
  freq_idade_notas[i,] <- round(idade_notas[i,] / sum(idade_notas[i,]) * 100, 2)
}

freq_idade_notas

plot(x=enem$NU_IDADE, y=enem$NU_NOTA_ENEM, main="Relaçao entre as notas do ENEM e a idade dos partecipantes", 
     ylab="Nota do ENEM", xlab="Idade", col="indianred2")

##########################################################################
# ---------------------------------------------------------------------- #
##########################################################################