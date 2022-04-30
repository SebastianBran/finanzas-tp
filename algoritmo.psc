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

Funcion tasa_efectiva_frecuencia_cupon <- retorna_tasa_efectiva_frecuencia_cupon ( tasa_efectiva_anual, dias_por_anio, frecuencia_cupon_dias )
	tasa_efectiva_frecuencia_cupon <- ((1 + tasa_efectiva_anual)^(frecuencia_cupon_dias/dias_por_anio)) - 1
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

Funcion calculo_cronograma_pagos( valor_nominal, nro_periodos, tasa_efectiva_anual, prima, impuesto_renta, frecuencia_cupon, dias_por_anio )
	bono_inicial <- valor_nominal
	frecuencia_cupon_dias <- retorna_frecuencia_cupon( frecuencia_cupon )
	tasa_efectiva_frecuencia_cupon <- retorna_tasa_efectiva_frecuencia_cupon( tasa_efectiva_anual, dias_por_anio, frecuencia_cupon_dias )
	
	Dimension bonos[nro_periodos]
	Dimension bonos_indexados[nro_periodos]
	Dimension cupones_interes[nro_periodos]
	Dimension cuotas[nro_periodos]
	Dimension amortizaciones[nro_periodos]	
	Dimension primas[nro_periodos]
	Dimension escudos[nro_periodos]
	Dimension flujos_emisor[nro_periodos]
	Dimension flujos_emisor_escudo[nro_periodos]
	Dimension flujos_bonistas[nro_periodos]
	Dimension flujos_actuales[nro_periodos]
	Dimension flujos_actuales_x_plazo[nro_periodos]
	Dimension factor_p_convexidad[nro_periodos]
	
	Para i desde 1 Hasta nro_periodos Hacer
		Si i = 1 Entonces
			bonos[i] <- bono_inicial
		SiNo
			bonos[i] <- bonos_indexados[i - 1] + amortizaciones[i - 1]
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
		
		escudos[i] <- - cupones_interes[i] * impuesto_renta
		flujos_emisor[i] <- cuotas[i] + primas[i]
		flujos_emisor_escudo[i] <- flujos_emisor[i] + escudos[i]
		flujos_bonistas[i] <- - flujos_emisor[i]
		flujos_actuales[i] <- (flujos_bonistas[i])/((1 + cok)^i)
		flujos_actuales_x_plazo[i] <- (flujos_actuales[i] * i) * (frecuencia_cupon_dias / dias_por_anio)
		factor_p_convexidad[i] <- flujos_actuales[i] * i * (i + 1)
	Fin Para
	
	
	//mostrar resultados
	Para i desde 1 Hasta nro_periodos Hacer
		Escribir "Periodo" i 
	FinPara
FinFuncion
Funcion  MostrarRestultados(dato,frecuencia_del_cupon_dias ,dias_de_capitalizacion ,periodos_por_anio,nro_total_de_periodos,tasa_efectiva_anual,tasa_efectiva,cok,costes_iniciales_emisor,costes_iniciales_bonista)
	Segun dato Hacer
		1:
			Mostrar  frecuencia_del_cupon_dias
		2:
			Mostrar dias_de_capitalizacion
		3:
			Mostrar periodos_por_anio
		4: 
			Mostrar nro_total_de_periodos
		5:
			
		6: 
			Mostrar tasa_efectiva_anual
		7:		
			Mostrar tasa_efectiva
		8:
			Mostrar cok
			
		9:	Mostrar costes_iniciales_emisor
			
		10:	Mostrar costes_iniciales_bonista
	Fin Segun
FinFuncion

Algoritmo Frances
	//Datos
	//Del Bono
	Definir valor_nominal como real
	Definir valor_comecial Como Real
	Definir n_de_anios Como Entero
    Definir frecuencia_del_cupon Como Cadena
	Definir dias_por_anio Como Entero
	Definir tipo_tasa_interes Como cadena
	Definir capitalizacion como cadena
	Definir tasa_interes Como Real
	Definir tasa_anual_descuento como real

	Definir imp_renta Como Real
	Definir fecha_de_emision como cadena
	//De los Costes/Gastos Iniciales
	Definir prima Como Real
	Definir estructuracion Como Real
	Definir colocacion Como Real
	Definir flotacion Como Real
	definir Cavali Como Real
	//Resultados
	//Dela estructuracion del bono
	Definir frecuencia_del_cupon_dias Como Entero
	Definir dias_capitalizacion como entero
	Definir periodos_por_anio  Como Entero
	Definir nro_total_de_periodos Como Entero
	Definir tasa_efectiva_anual como  real
	Definir tasa_efectiva Como Real
	Definir cok Como Real
	Definir costes_iniciales_emisor Como Real
	Definir costes_iniciales_bonista Como Real
	
	//Calculo Resultados
	frecuencia_del_cupon_dias     <-          retorna_frecuencia_cupon( frecuencia_del_cupon )
	dias_de_capitalizacion        <-          retorna_dias_capitalizacion( capitalizacion )
	periodos_por_anio             <-          retorna_periodos_por_anio( dias_por_anio, frecuencia_del_cupon )
	nro_total_de_periodos         <-          retorna_nro_periodos( periodos_por_anio, nro_anios )
	tasa_efectiva_anual           <-          retorna_tasa_efectiva_anual ( tipo_tasa_interes, tasa_interes, dias_por_anio, dias_capitalizacion )
	tasa_efectiva                 <-          retorna_tasa_efectiva_frecuencia_cupon ( tasa_efectiva_anual, dias_por_anio, frecuencia_cupon_dias )
	cok                           <-          retorna_cok( tasa_anual_descuento, frecuencia_del_cupon_dias, dias_por_anio )
	costes_iniciales_emisor       <-          retorna_costes_iniciales_emisor( estructuracion, colocacion, flotacion, cavali, valor_comercial )                         
    costes_iniciales_bonista      <-          retorna_costes_iniciales_bonista(  flotacion, cavali, valor_comercial )
	
FinProceso

	
	