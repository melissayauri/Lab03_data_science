# Importando librerías
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)
library(stringr)
library(purrr)
library(vioplot)
library(ggrepel)
# Importando el archivo
server_data <- read_table("D:\\MAESTRIA-CIBERSEGURIDAD\\SEMINARIO_DATA_SCIENCE\\LABS\\Lab03_lengR\\epa-http.csv", col_names = FALSE)
# Renonbrando los encabezados de las columnas
colnames(server_data) <- c("Ip_address", "Date", "Method", "Resource", "Protocol", "Response_code", "Bytes")

# PREGUNTA 1
# Conversión de datos

# Formateando la columna Method de "\"GET" a "GET"
server_data$Method <- str_sub(server_data$Method, 2)
# Convirtiendo a datos categóricos de la columna Method
server_data$Method <- as.factor(server_data$Method)

# Formateando la columna Protocol de "HTTP/1.0\"" a "HTTP/1.0"
server_data$Protocol <- str_sub(server_data$Protocol, end = -2)
# Convirtiendo a datos categóricos
server_data$Protocol <- as.factor(server_data$Protocol)

# Convirtiendo a datos catégoricos la columna Response_code
server_data$Response_code <- as.factor(server_data$Response_code)

# Formateando la columna Bytes,reemplazando "-" por 0
server_data$Bytes <- str_replace(server_data$Bytes, "-", "0")
# Convirtiendo a datos númericos
server_data$Bytes <- as.numeric(server_data$Bytes)

# Pregunta 2

# Creando la tabla con las columnas Ip_address y Response_code
address_table<- data.frame(Ip_address = server_data$Ip_address, Response_code = server_data$Response_code) 
# Tabla de Frecuencias de la columna Ip_address con Response_code
frequency <- as.data.frame(table(address_table))
# Obteniendo los datos existentes 
address_data <- filter(frequency, Freq > 0) 
# Ordenando de forma ascendente
address_data <- arrange(address_data, Response_code)
# Obteniendo los valores únicos de la columna Response_code
# Los valores son 200, 302, 304, 400, 403, 404, 500, 501
value_codes <- unique(address_data$Response_code)
# Creando nuevas tablas según el código de respuesta
code_data<- map(value_codes, ~address_data[address_data$Response_code == .x, ])
# Cada tabla según el código de respuesta es guardada en cada variable como data_code_200
names(code_data) <- paste0("data_code_", value_codes)
# Hallando el n° de usuarios según el código de respuesta
code200_users <- nrow(data_code_200)
code200_users
# Hay 2296 usuarios con el código de respuesta 200
code302_users <- nrow(data_code_302)
code302_users
# Hay 970 usuarios con el código de respuesta 302
code304_users <- nrow(data_code_304)
code304_users
# Hay 505 usuarios con el código de respuesta 304 
code400_users <- nrow(data_code_400)
code400_users
# Hay 1 usuario con el código de respuesta 400
code403_users <- nrow(data_code_403)
code403_users
# Hay 5 usuarios con el código de respuesta 403
code404_users <- nrow(data_code_404)
code404_users
# Hay 152 usuarios con el código de respuesta 404
code500_users <- nrow(data_code_500)
code500_users
# Hay 29 usuarios con el código de respuesta 500
code501_users <- nrow(data_code_501)
code501_users
# Hay 11 usuarios con el código de respuesta 501

# Pregunta 3
# Hallando el n° de repeticiones según el método GET, POST, HEAD (columna Method)
method_frequency <- table(server_data$Method)
# Mostrando la tabla de frecuencia según el método
method_data <- data.frame(method = names(method_frequency), Frequency = as.vector(method_frequency))
# Muestra de los resultados
print(method_data)
# Método GET (46020 repeticiones)
# Método HEAD (106 repeticiones)
# Método POST (1622 repeticiones)

# Hallando el n° de repeticiones descartando los recursos tipo imagen
# Filtrando los recursos que no contengan extensiones tipo imagen, discriminando mayúsculas y minusculas de las extensiones
noimages_data <- filter(server_data, !grepl("(?i)\\.(gif|jpg|jpeg|png|bmp)$", Resource))
nrow(noimages_data)
nrow(server_data)
# Hallando el n° de repeticiones del filtro anterior
method_frequency2 <- table(noimages_data$Method)
# Mostrando la tabla
method2_data <- data.frame(Method = names(method_frequency2), Frequency = as.vector(method_frequency2))
# Muestra de los resultados
print(method2_data)
#    GET     23841
#   HEAD        50
#   POST      1416

# Pregunta 4

# Agrupamos los datos de la columna Response_code y hallamos la frequencia.
response_code_data <- summarize(group_by(server_data, Response_code), Freq = n())

# Gráfico de barras
ggplot(response_code_data, aes(x = Response_code, y = Freq, fill = Response_code)) +
  # Creación del gráfico de barras
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#00686c", "#32c2b9", "#edecb3", "#fad928", "#ff9915", "#9e1e4c","#04394e","#703d6f"),name = "Códigos de respuesta") +
  # Etiquetas del gráfico
  labs(x = "Códigos de respuesta", y = "n° de peticiones", 
       title = "Códigos de respuesta de las peticiones") +
  geom_text(aes(label=Freq), vjust= -0.5) +
  # Escala del eje y 
  scale_y_continuous(expand = c(0,0), limits = c(-5, max(response_code_data$Freq)+ 3000)) +
  theme_gray()
 
# Gráfico de barras apiladas
ggplot(response_code_data, aes(x = 1, y = Freq, fill = Response_code)) + 
  # Creación del gráfico de barras
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#00686c", "#32c2b9", "#edecb3", "#fad928", "#ff9915", "#9e1e4c","#04394e","#703d6f"),name = "Códigos de respuesta") +
  # Etiquetas del gráfico
  xlab("") +
  ylab("n° de peticiones") +
  theme_gray()

# Gráfico circular
ggplot(response_code_data, aes(x = "", y = Freq, fill = Response_code)) +
  # Creación del gráfico chart
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values =  c("#00686c", "#32c2b9", "#dc6378", "#fad928", "#ff9915", "#9e1e4c","#04394e","#c90a02"),name = "Códigos de respuesta") +
  labs(title = "n° de peticiones según el código de respuesta") +
  # Incorporando los valores con la libreria ggrepel
  geom_label_repel(aes(label = Freq, x = 2.2, y = cumsum(Freq) - 0.5 * Freq),
                   show.legend = FALSE, color = "white", size = 4) 



# Pregunta 5
# Creando nueva columna con el número de carácteres de la columna Resource
server_data$length_Resource <-  str_length(server_data$Resource)
# Creando una nueva tabla con las columnas tipo factor
endpoints_data <- server_data[, c("Method", "Response_code", "Protocol")]
# Convirtiendo datos tipo factor a datos binarios
one_hot_data <- one_hot(as.data.table(endpoints_data), sparsifyNAs = TRUE)

# Obteniendo el kmeans
set.seed(50) # Fijando los centroides iniciales
# Agrupamiento de 7
value7_kmeans <- kmeans(one_hot_data, centers = 7)
#set.seed(150) # Fijando los centroides iniciales
# Agrupamiento de 4
value4_kmeans <- kmeans(one_hot_data, centers = 4)
#set.seed(100)# Fijando los centroides iniciales
# Agrupamiento de 3
value9_kmeans <- kmeans(one_hot_data, centers = 9)

#pregunta 6
# Agrupamiento de 7, se convierte en factor para la separación de cada cluster
server_data$clusters_7 <- as.factor(value7_kmeans$cluster)

# Gráfica en base a la columna bytes y length_Resource
ggplot(server_data, aes(x = Bytes, y = length_Resource, color = clusters_7)) +
  # Creación de la gráfica scatter
  geom_point() +
  # Convirtiendo la escala x en valores númericos
  scale_x_continuous(labels = function(x) as.integer(floor(x))) +
  labs(color = "Clusters") +
  xlab("Bytes") +
  ylab("N° de carácteres del Recurso") +
  scale_color_manual(values = c("#ffb300", "#d43f5d", "#f2a772", "#e8d890","#211c33","#d83018","#17a7a8"),
                     # especificando el orden de los colores
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
  ) +
  theme(legend.text=element_text(size=12))


# Agrupamiento de 4
server_data$clusters_4 <- as.factor(value4_kmeans$cluster)

# Gráfica en base a la columna bytes y length_Resource
  ggplot(server_data, aes(x = Bytes, y = length_Resource, color = clusters_4)) +
    # Creación de la gráfica scatter
    geom_point() +
    # Convirtiendo la escala x en valores númericos
    scale_x_continuous(labels = function(x) as.integer(floor(x))) +
    labs(color = "Clusters") +
    xlab("Bytes") +
    ylab("N° de carácteres del Recurso") +
    # Definiendo los colores
    scale_color_manual(values = c("#005bc5", "#790614", "#028f76", "#520647"),
                       breaks = c(1, 2, 3, 4),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
    theme(legend.text=element_text(size=12))

# Agrupamiento de 9
server_data$clusters_9 <- as.factor(value9_kmeans$cluster)
# Generando una nueva grafica en base al n° de slashes
server_data$slashes_number <- str_count(server_data$Resource, "/")

# Gráfica del nº de slashes del Recurso con el n° de carácteres del Recurso
ggplot(server_data, aes(x = slashes_number, y = length_Resource, color = clusters_9)) +
  # Creación de la gráfica scatter
  geom_point() +
  # Convirtiendo la escala x en valores númericos
  scale_x_continuous(labels = function(x) as.integer(floor(x))) +
  labs(color = "Clusters") +
  xlab("N° de slashes del Recurso") +
  ylab("N° de carácteres del Recurso") +
  # Definiendo los colores
  scale_color_manual(values = c("#2b818c", "#ffc6a5", "#fa6900", "#028f76", "#6d0839","#790614","#e4d829", "#fe59c2","#8a8780"),
                     breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                     labels = c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4", "Cluster 5", "Cluster 6","Cluster 7", "Cluster 8", "Cluster 9"))



View(server_data)
