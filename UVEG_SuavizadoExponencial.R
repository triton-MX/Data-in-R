# Autor: Triton Perea
# Importar archivo csv
bancoDatos <- read.csv('C:/.../bancoDatos.csv', header=TRUE, row.names=1)

# Determinamos el número de columnas que tienen datos. 
n= ncol(bancoDatos)

# Deteminar coeficiente de amortiguamiento (en este caso n ya tiene +1)
alpha= 2/(n)

# Crear data frame con el mismo tamaño y tipo
pronostico<- data.frame(bancoDatos)

# Ciclo for para recorrer cada tupla (fila) de la tabla
for (i in 1:nrow(bancoDatos)){
	
	# Necesito un primer valor para marcar tendencia:
	# beta= Valor_2016 / Valor_2015
	beta<- bancoDatos[i,2]/bancoDatos[i,1]

	#El primer pronostico depende de una tendencia de los valores reales
	pronostico[i,1]<- bancoDatos[i,1]*beta
	
	# Ciclo for para recorrer una por una las celdas de la fila
	for(j in 2:ncol(bancoDatos)) {
		# Pronostico_actual = Pronostico_anterior + alpha(ValorReal_anteior-Pronostico_anterior)
		pronostico[i,j]<- (pronostico[i,j-1]+(alpha*(bancoDatos[i,j-1]-pronostico[i,j-1])))
	}
	
	# Abrir el dispositivo PNG
	nombreArchivo= paste("C:/Users/iapcl/Documents/UVEG/Análisis de datos/Imagen", as.character(i),".png", sep = "")
	png(file = nombreArchivo, width = 800, height = 600)  # Especifica el nombre del archivo y dimensiones
	
	# Crear gráfico con plot() para la fila actual
	plot(2015:2020, bancoDatos[i, ], type = 'l', col = 'blue', lwd = 2, xlab = 'Año', ylab = 'Proporción', main = 'Gráfico de datos')
      lines(2015:2020, pronostico[i, ], col = 'red', lwd = 2)

	# Agregar leyenda
	legend("topright", legend = c("Datos reales", "Pronóstico"), col = c("blue", "red"), lty = 1, lwd = 2)

	# Cerrar el dispositivo PNG
	dev.off()
     
}
# Imprimir en la consola los dos dataframe
print(bancoDatos)
print(pronostico)




