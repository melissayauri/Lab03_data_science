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
