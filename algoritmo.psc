Funcion frecuencia_cupon_dias <- retorna_frecuencia_cupon( frecuencia_cupon )
	Segun frecuencia_cupon Hacer
		"Mensual":
			frecuencia_cupon_dias <- 30
		"Bimestral":
			frecuencia_cupon_dias <- 60
		"Trimestral":
			frecuencia_cupon_dias <- 90
		"Cuatrimestral":
			frecuencia_cupon_dias <- 120
		"Semestral":
			frecuencia_cupon_dias <- 180
		"Anual":
			frecuencia_cupon_dias <- 360
		De Otro Modo:
			frecuencia_cupon_dias <- -1
	Fin Segun
FinFuncion

Funcion dias_capitalizacion <- retorna_dias_capitalizacion( capitalizacion )
	Segun capitalizacion Hacer
		"Diaria":
			dias_capitalizacion <- 1
		"Quincenal":
			dias_capitalizacion <- 15
		"Mensual":
			dias_capitalizacion <- 30
		"Bimestral":
			dias_capitalizacion <- 60
		"Trimestral":
			dias_capitalizacion <- 90
		"Cuatrimestral":
			dias_capitalizacion <- 120
		"Semestral":
			dias_capitalizacion <- 180
		"Anual":
			dias_capitalizacion <- 360
		De Otro Modo:
			frecuencia_cupon_dias <- -1
	Fin Segun
FinFuncion

Funcion periodos_por_anio <- retorna_periodos_por_anio( dias_por_anio, frecuencia_cupon )
	periodos_por_anio <- dias_por_anio / frecuencia_cupon
FinFuncion

Funcion nro_periodos <- retorna_nro_periodos( periodos_por_anio, nro_anios )
	nro_periodos <- periodos_por_anio * nro_anios
FinFuncion

Funcion tasa_efectiva_anual <- retorna_tasa_efectiva_anual ( tipo_tasa_interes, tasa_interes, dias_por_anio, dias_capitalizacion )
	Si tipo_tasa_interes = "Efectiva" Entonces
		tasa_efectiva_anual <- tasa_interes * 100
	Sino
		aux = dias_por_anio / dias_capitalizacion
		tasa_efectiva_anual <- ((1 + tasa_interes / aux)^aux) - 1
		tasa_efectiva_anual <- tasa_efectiva_anual * 100
	FinSi
FinFuncion

Funcion tasa_efectiva_frecuencia_cupon <- retorna_tasa_efectiva_frecuencia_cupon ( tasa_efectiva_anual, dias_por_anio, frecuencia_cupon )
	tasa_efectiva_frecuencia_cupon <- ((1 + tasa_efectiva_anual)^(frecuencia_cupon/dias_por_anio)) - 1
	tasa_efectiva_frecuencia_cupon <- tasa_efectiva_frecuencia_cupon * 100 
FinFuncion

Funcion cok <- retorna_cok( tasa_anual_descuento, frecuencia_cupon, dias_por_anio )
	cok <- ((1+tasa_anual_descuento)^(frecuencia_cupon / dias_por_anio)) - 1
	cok <- cok * 100
FinFuncion

Funcion costes_iniciales_emisor <- retorna_costes_iniciales_emisor( estructuracion, colocacion, flotacion, cavali, valor_comercial )
	costes_iniciales_emisor <- (estructuracion + colocacion + flotacion + cavali) * valor_comercial
FinFuncion

Funcion costes_iniciales_bonista <- retorna_costes_iniciales_bonista(  flotacion, cavali, valor_comercial )
		costes_iniciales_bonista <- (flotacion + cavali) * valor_comercial	
FinFuncion

Funcion cuota <- calcula_cuota( bono_indexado, periodos_restantes, tasa_efectiva_frecuencia_cupon )
	aux = (1 + tasa_efectiva_frecuencia_cupon)^periodos_restantes
	cuota <- - bono_indexado * ((tasa_efectiva_frecuencia_cupon * aux)/(aux - 1))
FinFuncion

Funcion calculo_cronograma_pagos( valor_nominal, nro_periodos, tasa_efectiva_frecuencia_cupon, prima )
	bono_inicial <- valor_nominal
	Dimension bonos[nro_periodos]
	Dimension bonos_indexados[nro_periodos]
	Dimension cupones_interes[nro_periodos]
	Dimension cuotas[nro_periodos]
	Dimension amortizaciones[nro_periodos]	
	Dimension primas[nro_periodos]
	
	Para i desde 1 Hasta nro_periodos Hacer
		Si i = 1 Entonces
			bonos[i] <- bono_inicial
		FinSi
		
		bonos_indexados[i] <- bonos[i]
		cupones_interes[i] <- - bonos_indexados[i] * tasa_efectiva_frecuencia_cupon
		cuotas[i] <- calcula_cuota( bonos_indexados[i], nro_periodos - i + 1, tasa_efectiva_frecuencia_cupon )
		amortizaciones[i] <- cuotas[i] - cupones_interes[i]
		
		Si i = nro_periodos Entonces
			primas[i] <- bono_indexado * prima
		SiNo
			primas[i] <- 0
		FinSi
	Fin Para
FinFuncion

Algoritmo Frances
	//calculo_cronograma_pagos(1000, 10)
FinAlgoritmo
	