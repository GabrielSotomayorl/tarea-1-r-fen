#----------------------------------#
#	 Ejercicios Tarea Intermedia ####
#  Fecha entrega: 13/07/2023 23:59 
#---------------------------------#

#-----------------#
# Instrucciones: 
#---------------#
# 1. Esta evaluación contiene una serie de preguntas cortas relacionados con algunos de los temas
#   estudiados en la primera parte de la capacitación. Esta evaluación tiene como objetivo que
#   usted se evalúe y logre ver sus avances hasta el momento.

# 2. La evaluación tiene 24 puntos distribuidos en tres secciones

# 3. Pondera 30% de la nota final del curso

#------------#
# Ejercicios: 
#-----------#
#Librerias

if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(AER, rlang, ggplot2, WDI, dplyr)

# 1. (2 ptos) Escriba un código que le permita calcular el cuadrado 
#    de los primeros 10 elementos de un vector cualquiera.

cuadradodiez<-function(x) {
  if (!is.vector(x)) {
    stop("El objeto ingresado no corresponde a un vector")
  } else
    if (length(x)<10) {
   print(head(x,10)^2)
    warning("El vector tiene menos de 10 elementos")
  } else {
    head(x,10)^2
  }
}

cuadradodiez(1:500)
# 2. (2 ptos) Cree una matriz de 30 × 30, para cada fila y columna, asigne un valor que
#     sea el producto de la posición en la que se encuentran los elementos de la matriz. Por
#     ejemplo, al elemento en la fila 2 y columna 3 le corresponde el valor 6.

outer(1:30, 1:30)

# 3. (6 ptos) Cree una función que genere histogramas de una variable de interés continua 
#   (argumento de la función a elegir por el usuario) de la base CPS1985 por 
#   género, estado civil y pertenencia a sindicato (variable union)
#   Recuerde añadir titulo del gráfico y ejes. 


data(CPS1985)

histcps <- function(x) {
  x_var <- sym(x)
  ggplot2::ggplot(CPS1985, ggplot2::aes(x = !!x_var)) +  
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..)) +
    ggplot2::facet_grid(
      gender ~ 
        factor(union, c("yes","no"), c("unionized","no unionized")) + 
        factor(married, c("yes","no"), c("married","no married"))) +
    ggplot2::ggtitle(paste("Histograma de",x,"según género, estado civil y pertenencia a sindicato")) + 
    ggplot2::xlab(paste("Valor de", x)) + 
    ggplot2::ylab("Densidad") 
}

histcps("wage")
histcps("education")
histcps("experience")
histcps("age")

# 4. (6 ptos) Utilizando datos provenientes de World Development Indicators para el año 2021:
#   Use un mapa de África para representar la expectativa de vida al nacer utilizando diferentes colores.

# Base de datos World Development Indicators:

wbData <- WDI(indicator=c("SP.DYN.LE00.IN"), country="all", extra = TRUE, start=2021, end=2021)

wbData <- wbData %>% 
  arrange(country, year) %>% 
  rename(lifeExpectancy=SP.DYN.LE00.IN) %>% 
  filter(region!="Aggregates"&region %in% c("Sub-Saharan Africa","Middle East & North Africa"))

africa<- map_data("world",region = unique(wbData$country))

wbData$country

africaj<-merge(africa,wbData,by.x= "region",by.y="country")

# Mapa región separación por color
ggplot(africaj, aes(x = long, y = lat, group = group, fill = lifeExpectancy)) +
  geom_polygon(colour = "black") + coord_map("mercator")

# 5. (2 ptos) Cree un objeto de diseño de muestreo con la base CASEN 2020
temp <- tempfile() #Creamos un archivo temporal
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip",temp) #descargamos los datos
casen20<- haven::read_sav(unz(temp, "Casen_en_Pandemia_2020_revisada202209.sav")) #cargamos los datos
unlink(temp); remove(temp) #eliminamos el archivo temporal

casen20w<-svydesign(ids = ~varunit,
                    strata = ~varstrat,
                    weights = ~expr,
                    data=casen20)
# 6. (6 ptos) Cree un gráfico (a su elección) representativo a nivel nacional de una variable de su interés.
#  El gráfico debe contener título y nombre de los ejes en caso que corresponda 

# Crear tabla de frecuencias y convertirla en dataframe
tabla <- svytable(~haven::as_factor(pobreza), casen20w) %>% 
  prop.table() * 100 
df <- as.data.frame(tabla)

# Renombrar las columnas
colnames(df) <- c("Pobreza", "Porcentaje")

# Crear el gráfico
ggplot2::ggplot(df, aes(x = Pobreza, y = Porcentaje)) +
  geom_bar(stat = "identity",fill ="steelblue") +
  geom_text(aes(label = round(Porcentaje, 1)), vjust = -0.4) +
  ggtitle("Porcentaje de pobreza por tipo") +
  xlab("Situación de pobreza") +
  ylab("Porcentaje") 
