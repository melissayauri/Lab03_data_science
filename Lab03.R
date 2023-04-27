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
method_data <- data.frame(method = names(method_frequency), method_frequency = as.vector(method_frequency))
# Muestra de los resultados
print(method_data)
# Método GET (46020 repeticiones)
# Método HEAD (106 repeticiones)
# Método POST (1622 repeticiones)



View(server_data)