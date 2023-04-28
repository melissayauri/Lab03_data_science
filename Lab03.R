# Importando librerías
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)
library(stringr)
library(purrr)

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
# Hallando el n° de repeticiones del filtro anterior
method_frequency2 <- table(noimages_data$Method)
# Mostrando la tabla
method2_data <- data.frame(Method = names(method_frequency2), Frequency = as.vector(method_frequency2))
# Muestra de los resultados
print(method2_data)
# GET     46020
# HEAD       106
# POST      1622

# Pregunta 4
# Data
data_ta <- data.frame(Method = server_data$Method, Response_code = server_data$Response_code)
# Frequencia según la columna Method y Response_code
requests_freq <- table(server_data$Method, server_data$Response_code)
# Conversión a data.frame
requests_data <- as.data.frame(requests_freq)
# Añadiendo nombres a la tabla
colnames(requests_data) <- c("Method", "Response_code", "Frequency")
# Nueva data para validarlo con la gráfica
new_data <- spread(requests_data, Response_code, Frequency)
#requests_data$Frequency <- as.numeric(requests_data$Frequency)

# Creando los gráficos

# Gráfica 1
ggplot(requests_data, aes(x = Method, y = Frequency, fill = Method)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("#a40b54", "#11766d", "#027fe9"), 
                    labels=c("GET", "HEAD", "POST")) +
  labs(title = "Código de respuesta con el tipo de método", 
       x = "Método de solicitud", y = "Frecuencia", 
       fill = "Método") +
  facet_wrap(~Response_code, ncol = 3, scales = "free_y") +
   geom_text(aes(label = ..y..), stat="identity", position=position_dodge(width=1), vjust=-0.5, size=3) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1), add = c(0, 10)))
 
# Gráfica 2
ggplot(requests_data, aes(x = Response_code, y = Frequency, fill = Method)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#a40b54", "#11766d", "#027fe9"), 
                    labels=c("GET", "HEAD", "POST")) +
  labs(title = "Método según el código de respuesta", 
       x = "Código de respuesta", y = "Frecuencia", 
       fill = "Método") +
  facet_wrap(~Method, ncol = 1, scales = "free_y") +
  geom_text(aes(label = ..y..), stat="identity", position=position_stack(vjust=0.5), size=3)

# Gráfica 3
ggplot(requests_data, aes(x = Method, y = Frequency, color = Response_code)) +
  geom_point() +
  geom_text(aes(label = Frequency), vjust = -1) +
  labs(title = "Método de solicitud según código de respuesta", x = "Método de solicitud", y = "Frecuencia", color = "Código de respuesta") +
  facet_grid(Response_code ~ .) +
  ylim(0, 60000) +
  scale_color_manual(values=c("#f70a71", "#c90a02", "#ffb145", "#74ab90", "#3a89c9","#1b325f","#f26c4f","#63203d"), 
                     labels=c("200", "302","304","400","403","404", "500", "501"))

# Pregunta 5
# Creando nueva columna con el número de carácteres de la columna Resource
server_data$length_Resource <-  str_length(server_data$Resource)
# Creando una nueva tabla con las columnas tipo factor
endpoints_data <- server_data[, c("Method", "Response_code", "Protocol")]
# Convirtiendo datos tipo factor a datos binarios
one_hot_data <- one_hot(as.data.table(endpoints_data), sparsifyNAs = TRUE)
# Obteniendo el kmeans
value7_kmeans <- kmeans(one_hot_data, centers = 7)
value4_kmeans <- kmeans(one_hot_data, centers = 4)
#pregunta 6
# Agrupamiento de 7
server_data$clusters_7 <- as.factor(value7_kmeans$cluster)

# Gráfica en base a la columna bytes y length_Resource
ggplot(server_data, aes(x = Bytes, y = length_Resource, color = clusters_7)) +
  geom_point() +
  scale_x_continuous(labels = function(x) as.integer(floor(x)))

# Agrupamiento de 4
server_data$clusters_4 <- as.factor(value4_kmeans$cluster)
# Gráfica en base a la columna bytes y length_Resource
ggplot(server_data, aes(x = Bytes, y = length_Resource, color = clusters_4)) +
  geom_point() +
  scale_x_continuous(labels = function(x) as.integer(floor(x)))

# generando una nueva grafica en base al n° de slashes
server_data$slashes_number <- str_count(server_data$Resource, "/")
View(server_data)

ggplot(server_data, aes(x = slashes_number, y = length_Resource, color = clusters_4)) +
  geom_point() +
  scale_x_continuous(labels = function(x) as.integer(floor(x)))