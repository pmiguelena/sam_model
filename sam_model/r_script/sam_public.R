### SAM MODEL
## PROF GABRIEL MICHELENA VERSION ABRIL 2022

rm(list = ls()) # limpiar la memoria

# opciones iniciales
options(warn = 1)
options(scipen = 999)
options(max.print = 1000000)
options(java.parameters = "-Xmx2000m")
options(digits = 3)

# funcion concatenar
"%+%" <- function(x,y) paste(x, y, sep = "")

# Establecer directorio de trabajo

# Directorios principales
main     <- getwd() %+% '/' 
main     <- gsub('r_script/', '', main)
data_in  <- main %+% 'data_in/'
data_out <- main %+% 'data_out/'
script   <- main %+% 'r_script/'


# cargo las librerias que necesito para trabajar
source(script  %+%  'librerias.R')

#-----------------------------------------------------------------------------------------------#
# CARGA DE DATOS
#-----------------------------------------------------------------------------------------------#

# Establecer directorio de trabajo
tbl_act <- read_xls(data_in %+% "sam_data.xls", sheet = "desc_act")
q_act <- nrow(tbl_act)

str(tbl_act)
class(tbl_act)
mode(tbl_act)
tbl_act[1:2,]

tbl_com <- read_xls(data_in %+% "sam_data.xls", sheet = "desc_com")
q_com <- nrow(tbl_com)
tbl_com[1:2,]

# SAM 2018 millones de pesos corrientes
tbl_sam <- read_xls(data_in %+% "sam_data.xls", sheet = "sam_2018")
n_cell <- nrow(tbl_sam) -1  # nro de filas menos el total
tbl_sam <- as.data.frame(tbl_sam[,-1])   # le elimino la primer columna
rownames(tbl_sam) <- colnames(tbl_sam)
tbl_sam[1:2,1:3]

dim(tbl_sam)
class(tbl_sam) # data.frame

# la convierto a matriz
sam <- as.matrix(tbl_sam[1:(n_cell), 1:(n_cell)]) # convertir el data.frame en una matriz
sam[1:10,1:3]

#-----------------------------------------------------------------------------------------------#
# ALGUNAS VARIABLES DE INTERES 
#-----------------------------------------------------------------------------------------------#
VBP <- colSums(sam[, 1:q_act])
VA <- sam[c("vab"), 1:q_act]
VAsobreVBP <- VA / VBP
IM <- sam['row', (q_act+1):(q_act+q_com)]
OA <- colSums(sam[,(q_act + 1):(q_act+q_com)])
IMsobreOA <- IM/OA
IMsobreOA

#-----------------------------------------------------------------------------------------------#
# INVIRTIENDO LA MATRIZ DE COEF 
#-----------------------------------------------------------------------------------------------#
# vector de totales
tot <- colSums(sam[, 1:n_cell])
tot <- as.matrix(tot)  

#rownames(tot) <- names
colnames(tot) <- c('tot')

# armo una matriz diagonal
iden <- diag(n_cell)

# matriz inversa de totales
inv_tot <- 1/tot

#pregunta a R que elementos del vector con infinito y en esos casos pone cero
inv_tot[is.infinite(inv_tot)] <- 0  

# matrices 
# endogenizo el consumo
# componentes de la demanda final
hhd      <- sam[1:n_cell,c("hhd")] 
gov      <- sam[1:n_cell,c("gov")] 
inv      <- sam[1:n_cell,c("s.i")] 
row      <- sam[1:n_cell,c("row")] 
dstk     <- sam[1:n_cell,c("dstk")] 


# armo la SAM que voy a terminar invirtiendo
sam_mod <- sam[1:n_cell,1:121] # matriz original hasta los componentes de DF
zero <- matrix(data=0, nrow = n_cell, ncol= 4) # matriz columna de zeros
sam_mod <- cbind(sam_mod, zero) 
  
# le pongo nombres a las columnas
colnames(sam_mod) <- colnames(sam) 

# armo la matriz diagonal
diag_tot <- diag(c(inv_tot)) 

# matriz de coeficientes
coef_sam <- sam_mod %*% diag_tot  # ahora si hago la multiplicación
coef_sam[is.nan(coef_sam)] <- 0
colnames(coef_sam) <- colnames(sam)

# 3: finalmente invierto la matriz
inter <- iden - coef_sam
inv_sam <- solve(iden - coef_sam) # finalmente obtengo la inversa sam

#-----------------------------------------------------------------------------------------------#
# SIMULACIONES ===> DEMANDA
#-----------------------------------------------------------------------------------------------#
# recordatorio => la sam y la cou estan expresadas en miles de pesos (2018)
# cargamos el vector del shock, en donde la construccion aumenta en
# 28,100,000 miles de pesos =>  => aprox USD 1.000 millones  
#-----------------------------------------
x.chg <- matrix(0,nrow=n_cell, ncol = 1)
x.chg[78,1] <- 28100000 
  

# multiplico por la inversa SAM
sam.chg <- inv_sam %*% x.chg

# calculo el vector de cambio en el emplo
#-----------------------------------------
# primero levanto el vector => miles de puestos
labor <- read_xls(data_in %+% "sam_data.xls", sheet = "labor_input" )
labor <- as.matrix(labor[,-1]) 
labor

# estimo el coeficiente labor-output
# puestos por miles de $ de producto
LsobreVBP <- labor/VBP
LsobreVBP

# estimo el cambio en el empleo
vbp.chg <- sam.chg[1:40,1]
lab.chg <- diag(c(LsobreVBP)) %*% vbp.chg
lab.chg

# calculo el vector de cambio en el valor agregado
#-----------------------------------------
va.chg <- diag(c(VAsobreVBP)) %*% sam.chg[1:40,1]
va.chg

#-----------------------------------------------------------------------------------------------#
# TABLAS RESULTADOS
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------
# TABLA SECTORIAL
#-----------------------------------------------------------
# creo una tabla final y exporto los resultados 
sec_res <- cbind(vbp.chg, va.chg, lab.chg)

# agrego totales
# le agrego una  una fila con los totales 
sec_res <- rbind(sec_res, colSums(sec_res))
# corrijo el nombre
rownames(sec_res)[NROW(sec_res)] <- 'TOTAL'
colnames(sec_res) <- c('chg.vbp', 'chg.vab', 'chg.lab')

# imprimo la tabla
sec_res


# finalmente exporto los resultados
#-------------------------------------
write.csv(as.data.frame(sec_res), data_out %+% "resultados.csv")

