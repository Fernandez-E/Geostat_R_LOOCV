install.packages("gstat")
install.packages("sp")
install.packages("writexl")

library(gstat)
library(sp)
library(writexl)

dados <- read.csv(
  "E:/[MV]MIRELLA_VERAS/ESCRITA_ARTIGOS/GEOESTATISTICA/data.csv", # usar o caminho absoluto (pegar no explorador de arquivos)
  header = TRUE,
  sep = ","
)

head(dados)

coordinates(dados) <- ~x+y #Alterar os nomes x e y de acordo com o nome de cada coluna no arquivo excel/csv

variograma <- variogram(
  z ~ 1, # quando não existe tendência
  dados,
  cutoff = 1700,
  width = 100
)

modelo <- vgm(
  psill = 720,
  model = "Gau", # Sph, Gau ou Exp
  range = 800,
  nugget = 15
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
  nfold = nrow(dados)
)

View(as.data.frame(resultado))

Media_Z <- mean(resultado$zscore, na.rm = TRUE)

DP_Z <- sd(resultado$zscore, na.rm = TRUE)

zscore <- data.frame(
  Media_Z = Media_Z,
  DP_Z = DP_Z
)

print(zscore)

