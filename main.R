install.packages("gstat")
install.packages("sp")
install.packages("writexl")

library(gstat)
library(sp)
library(writexl)

dados <- read.csv(
  "D:/geoeas/data.csv", # usar o caminho absoluto (pegar no explorador de arquivos)
  header = TRUE,
  sep = ","
)

head(dados)

coordinates(dados) <- ~x+y #Alterar os nomes x e y de acordo com o nome de cada coluna no arquivo excel/csv

variograma <- variogram(
  z ~ 1, # quando não existe tendência
  dados,
  cutoff = 12,
  width = 1.16
)

modelo <- vgm(
  psill = 1.8,
  model = "Exp", # Sph, Gau ou Exp
  range = 3, # Caso seja comparado ou utilizado modelos do geoeas, usar aqui o range/3 se exponencial, range/raiz(3) se gaussiano ou range se esférico
  nugget = 1.2
)

plot(
  variograma,
  modelo,
  main = "Semivariograma",
  xlab = "Distância",
  ylab = "Semivariância"
)

resultado <- krige.cv(
  z ~ 1,
  locations = dados,
  model = modelo,
  nfold = nrow(dados), 
  maxdist = 23.1
)

View(as.data.frame(resultado))

Media_Z <- mean(resultado$zscore, na.rm = TRUE)

DP_Z <- sd(resultado$zscore, na.rm = TRUE)

zscore <- data.frame(
  Media_Z = Media_Z,
  DP_Z = DP_Z
)

print(zscore)
