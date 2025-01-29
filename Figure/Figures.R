### FIgures ###
library("ggplot2")
library("tidyverse")
#Elegir archivo para graficar 
library(readxl)
Dominio_ART_2 <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/Dominio_ART_2.xlsx")
View(Dominio_ART_2)
# realizar la grafica 
ggplot(Dominio_ART_2,aes(x=Samples, y=`Proportion of reads` , fill=Domain))+
  geom_col()+
  scale_fill_manual(breaks = Dominio_ART_2$Domain,
                    values = c("blue", "red", "magenta", "deepskyblue", "orange", "pink", "brown")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.x  = element_text (angle =  90, size = 5 ) )
# realizar la grafica por bloques tipo de muestras AGUA
ggplot(Dominio_ART_2,aes(x=Samples, y=`Proportion of reads` , fill=Domain))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.x  = element_text (angle =  90, size = 4 ) )+
  facet_wrap(~ Water, scales = "free_x",)
# realizar la grafica por bloques tipo de REGION
ggplot(Dominio_ART_2,aes(x=Samples, y=`Proportion of reads` , fill=Domain))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.x  = element_text (angle =  90, size = 4 ) )+
  facet_wrap(~ Continent, scales = "free_x",)

### heatmap ####
#instalar paquetes y activarlos
#install.packages("tidyverse")
#install.packages("pheatmap")
#install.packages("matrixStats")
library(tidyverse)
library(pheatmap)
library(matrixStats)
#Cargar base de datos de los 116 archivos completos de diamond para familia
# se utilizaron datos proporcionales de los valores normalizados
library(readxl)
Family_16_data_raw <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/116 archivos/Family_16_data_raw_diamond.xlsx")
Family_16_data_raw %>%
  select(1:117) %>%
  column_to_rownames("Family") -> heatmap_data
heatmap_data %>%
pheatmap(fontsize_row = 8, fontsize_col = 5)
#Etiqueta para ambos ejes (CORRECTO)
data.frame(row.names = colnames(heatmap_data),
           Continent = c("Europe", "Europe", "Europe", "Europe", "Europe", "Africa", "N.America", "Europe",
                         "Europe", "Europe", "N.America", "Europe", "Europe", "Europe", "Europe", "N.America", "N.America",
                         "N.America", "N.America", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "N.America", "N.America", "N.America", "N.America", "N.America", "N.America",
                         "N.America", "N.America", "N.America", "S.America", "N.America", "N.America", "N.America", "N.America",
                         "Europe", "Europe", "Europe", "Europe", "Europe", "N.America", "N.America", "N.America", "N.America", "N.America",
                         "N.America", "N.America", "N.America", "N.America", "N.America", "Asia", "N.America", "N.America", "N.America",
                         "S.America", "S.America", "S.America", "N.America", "N.America", "Africa", "Africa", "Africa", "Africa", "Africa",
                         "Africa", "Europe", "Europe", "Europe", "Europe", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Europe", "Europe", "Europe",
                         "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe")) -> col_annotations
data.frame(row.names = row.names(heatmap_data),
           Host = c("Bacteria", "Plant", "Bacteria", "Bacteria", "Bacteria", "Bacteria", 
                    "Bacteria", "Plant", "Protist", "Host_range", "Host_range", "Plant", "Vertebrates", 
                    "Plant", "Host_range", "Vertebrates", "Bacteria", "Vertebrates", 
                    "Protist", "Plant")) -> row_annotations
heatmap_data %>%
  pheatmap(annotation_row = row_annotations, annotation_col = col_annotations, fontsize_row = 8, fontsize_col = 5)
           

heatmap_data %>%
  pheatmap(annotation_row = row_annotations, annotation_col = col_annotations, 
           annotation_colors = list(Continent = c(Africa = "#E7298A", Asia = "purple", Europe = "magenta", N.America = "turquoise", S.America = "gray"),
                                    Host = c(Bacteria = "#1B9E77", Host_range = "#E7298A", Plant = "magenta", Protist = "#D95F02", Vertebrates = "#E6AB02")), fontsize_row = 8, fontsize_col = 5)
#######Diversidad de especies
library(readxl)
specie_raw_data_116_diamond <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/116 archivos/specie_raw_data_116_diamond.xlsx")
specie_raw_data_116_diamond %>%
  select(1:117) %>%
  column_to_rownames("Virus") -> heatmap_data
heatmap_data %>%
  pheatmap(fontsize_row = 8, fontsize_col = 5)
#Etiqueta ara ambos ejes (CORRECTO)
data.frame(row.names = colnames(heatmap_data),
           Continent = c("Europe", "Europe", "Europe", "Europe", "Europe", "Africa", "N.America", "Europe",
                         "Europe", "Europe", "N.America", "Europe", "Europe", "Europe", "Europe", "N.America", "N.America",
                         "N.America", "N.America", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "N.America", "N.America", "N.America", "N.America", "N.America", "N.America",
                         "N.America", "N.America", "N.America", "S.America", "N.America", "N.America", "N.America", "N.America",
                         "Europe", "Europe", "Europe", "Europe", "Europe", "N.America", "N.America", "N.America", "N.America", "N.America",
                         "N.America", "N.America", "N.America", "N.America", "N.America", "Asia", "N.America", "N.America", "N.America",
                         "S.America", "S.America", "S.America", "N.America", "N.America", "Africa", "Africa", "Africa", "Africa", "Africa",
                         "Africa", "Europe", "Europe", "Europe", "Europe", "Asia", "Asia", "Asia", "Asia", "Asia",
                         "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Europe", "Europe", "Europe",
                         "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe")) -> col_annotations
data.frame(row.names = row.names(heatmap_data),
           Host = c("Cucurbitaceae", "Solanaceae", "Solanaceae", "Solanaceae", "Solanaceae", "Solanaceae",
                    "Brassicaceae", "Brassicaceae", "Brassicaceae", "Solanaceae", "Solanaceae", "Solanaceae",
                    "Cucurbitaceae", "Solanaceae")) -> row_annotations
heatmap_data %>%
  pheatmap(annotation_row = row_annotations, annotation_col = col_annotations,
           fontsize_row = 7, fontsize_col = 4)
heatmap_data %>%
  pheatmap(annotation_row = row_annotations, annotation_col = col_annotations, 
           annotation_colors = list(Continent = c(Africa = "#E7298A", Asia = "purple", Europe = "magenta", N.America = "turquoise", S.America = "gray"),
                                    Host = c(Brassicaceae = "#1B9E77", Cucurbitaceae = "#E7298A", Solanaceae = "magenta")), fontsize_row = 7, fontsize_col = 4)
#NDMS
library(readxl)
ndms <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/ndms.xlsx")
View(ndms)
str(ndms)
#Para tipo de muestra de agua###############
library(vegan)
matriz <- ndms[,4:ncol(ndms)]
matriz <- as.matrix(matriz)
matriz
analisis <- anosim(matriz,ndms$Sample, distance = "bray", permutations = 9999)
analisis
library(vegan)
library(BiodiversityR)
library(clipr)
#cargar datos diamond 116 archivos
library(readxl)
specie_raw_data_116_diamond <- read_excel("116 archivos/specie_raw_data_116_diamond.xlsx")
View(specie_raw_data_116_diamond)
norm01 <- specie_raw_data_116_diamond[, -1]
boxplot(norm01)
diversity(norm01, index = "shannon")
spectrum(norm01)
#### Grafico Con puntos de los indices de shannon
library(readxl)
Shannon <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/Shannon.xlsx")
View(Shannon)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
#Para combinar graficos
#install.packages("ggpubr")
library(ggpubr)
#Contienente
rm(a)
a <- ggplot(data = Shannon, aes(x = Continent, y = `Shannon index`)) + 
  geom_boxplot(alpha = 0.5, color = c("tomato", "magenta", "orange", "magenta", "lightblue")) +
  geom_jitter(alpha = 0.5, color = "tomato")+
  theme(panel.background = element_rect(fill = NA, color = "black"))
a
#Muestra
rm(c)
c <- ggplot(data = Shannon, aes(x = Sample, y = `Shannon index`)) + 
  geom_boxplot(alpha = 0.5, color = c("magenta", "orange", "red")) +
  geom_jitter(alpha = 0.5, color = c("tomato"))+
  theme(panel.background = element_rect(fill = NA, color = "black"))
c
#Country
rm(e)
e <- ggplot(data = Shannon, aes(x = Country, y = `Shannon index`)) + 
  geom_boxplot(alpha = 0.5, color = c("violet", "magenta", "orange", "purple", "red", "pink", "blue", "violet", "magenta", "orange", "purple", "red", "pink", "blue", "violet", "magenta", "orange", "purple", "red", "pink", "blue", "red" )) +
  geom_jitter(alpha = 0.5, color = "tomato")+
  theme(panel.background = element_rect(fill = NA, color = "black"))+
  theme(axis.text.x = element_text(angle = 90))
e
#Graficos y analisis con datos normalizados por totalidad de virus
library(readxl)
Normalizado_plosOne <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/Normalizado_plosOne.xlsx")
View(Normalizado_plosOne)
norm01 <- Normalizado_plosOne[, -1]
boxplot(norm01)
diversity(norm01, index = "shannon")
spectrum(norm01) 

#Simpson 
diversity(norm01, index = "simpson")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)
Simpson <- read_excel("C:/Users/usdal/OneDrive/Escritorio/Script_Art_2/Simpson.xlsx")
View(Simpson)
Simpson <- read_excel("Simpson.xlsx")
View(Simpson)
#Contienente
rm(b)
b <- ggplot(data = Simpson, aes(x = Continent, y = `Simpson index`)) + 
  geom_boxplot(alpha = 0.5, color = c("tomato", "magenta", "orange", "magenta", "lightblue")) +
  geom_jitter(alpha = 0.5, color = "tomato")+
  theme(panel.background = element_rect(fill = NA, color = "black"))
b
#Muestra
rm(d)
d <- ggplot(data = Simpson, aes(x = Sample, y = `Simpson index`)) + 
  geom_boxplot(alpha = 0.5, color = c("magenta", "orange", "red")) +
  geom_jitter(alpha = 0.5, color = c("tomato"))+
  theme(panel.background = element_rect(fill = NA, color = "black"))
d
#Country
rm(f)
f <- ggplot(data = Simpson, aes(x = Country, y = `Simpson index`)) + 
  geom_boxplot(alpha = 0.5, color = c("violet", "magenta", "orange", "purple", "red", "pink", "blue", "violet", "magenta", "orange", "purple", "red", "pink", "blue", "violet", "magenta", "orange", "purple", "red", "pink", "blue", "red" )) +
  geom_jitter(alpha = 0.5, color = "tomato")+
  theme(panel.background = element_rect(fill = NA, color = "black"))+
  theme(axis.text.x = element_text(angle = 90))
f
# unir graficos
ggpubr::ggarrange(a,b,c,d,e,f, ncol = 2, nrow = 3)
ggpubr::ggarrange(gr2,gr3)

#### NDMS
#install.packages("simEd")
library(simEd)
library(ggplot2)
matriz
set.seed(123)
nmds<-metaMDS(matriz, distance = "bray")
nmds
plot(nmds)
coordenadas <- as.data.frame(scores(nmds)$sites)
coordenadas
coordenadas$Water = ndms$Sample
coordenadas$sample = ndms$SRA
head(coordenadas)
str(coordenadas)  
gr2<-ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+
  geom_point(size = 4, aes(colour = ndms$Sample))+
  geom_text(hjust = 0.5, vjust = 1.5, label="")+
  theme_classic()+
  theme(legend.position = "bottom")
gr2
# Por continente
library(vegan)
matriz2 <- ndms[,4:ncol(ndms)]
matriz2 <- as.matrix(matriz)
matriz2
analisis2 <- anosim(matriz2,ndms$Continent, distance = "bray", permutations = 9999)
analisis2
#NDMS
#install.packages("simEd")
library(simEd)
matriz2
set.seed(123)
nmds2<-metaMDS(matriz2, distance = "bray")
nmds2
plot(nmds2)
coordenadas2 <- as.data.frame(scores(nmds2)$sites)
coordenadas2
coordenadas2$Water = ndms$Sample
coordenadas2$sample = ndms$SRA
head(coordenadas2)
str(coordenadas2)  
gr3<-ggplot(coordenadas2, aes(x = NMDS1, y = NMDS2))+
  geom_point(size = 4, aes(colour = ndms$Continent))+
  geom_text(hjust = 0.5, vjust = 1.5, label="")+
  theme_classic()+
  theme(legend.position = "bottom")
gr3
