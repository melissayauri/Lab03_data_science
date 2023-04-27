# Importando librerías
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)
library(stringr)

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


View(server_data)