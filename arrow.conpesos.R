func_calcula_matrizclasificacion2 <- function(Mvalor, pesos.criterios) {
  #Mvalor
  w <- matrix(pesos.criterios, ncol = 1)
  num.alter=nrow(Mvalor)
  num.crit=ncol(Mvalor)
  Mclasificacion = matrix(NA,nrow=num.alter,ncol=num.alter)
  rownames(Mclasificacion) = colnames(Mclasificacion) = rownames(Mvalor)
  for (i in 1:nrow(Mvalor)) {
    for (j in 1:nrow(Mvalor)) {
      if (i!=j) {
        filai = Mvalor[i,]
        filaj = Mvalor[j,]
        mayoriquej = matrix(filai>filaj, nrow = 1) %*% w
        igualiquej = matrix(filai==filaj, nrow = 1) %*% w
        Mclasificacion[i,j] = mayoriquej + 0.5 * igualiquej
      }
    }
  }
  return(Mclasificacion)
}


multicriterio.metodoaxiomatico.ArrowRaymond.conpesos <- function(Mvalor, pesos.criterios) {
  
  num.alter = nrow(Mvalor)
  salida = vector("list",num.alter-1)
  salidatemp = list()
  Mvalor_red = Mvalor
  alternativas.salen = c()
  #i=1
  for (i in 1:(num.alter-1)) {
    Mclasificacion_red = func_calcula_matrizclasificacion2(Mvalor_red, pesos.criterios)
    nombres.alternativas = rownames(Mclasificacion_red)
    max.filas = apply(Mclasificacion_red,1,max,na.rm=T)
    inds.orden = order(max.filas)
    salidatemp$Mclasificacion = Mclasificacion_red
    salidatemp$max.filas = max.filas
    salidatemp$indices.ordenados = inds.orden
    salidatemp$alternativa.sale = nombres.alternativas[inds.orden[1]]
    if (i<num.alter) {
      Mvalor_red = Mvalor_red[-inds.orden[1],]
    }
    salida[[i]] = salidatemp
    alternativas.salen = c(alternativas.salen,nombres.alternativas[inds.orden[1]])
  }
  alternativas.salen = c(alternativas.salen,nombres.alternativas[inds.orden[2]])
  salidas = list()
  salidas$pasos = salida
  salidas$alternativasordenadas = alternativas.salen[length(alternativas.salen):1]
  return(salidas)
  
}
