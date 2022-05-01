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

Funcion nro_periodos <- retorna_nro_periodos( periodos_por_anio, n_de_anios )
	nro_periodos <- periodos_por_anio * n_de_anios
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

Funcion tasa_efectiva <- retorna_tasa_efectiva ( tasa_efectiva_anual, dias_por_anio, frecuencia_cupon_dias )
	tasa_efectiva <- ((1 + tasa_efectiva_anual)^(frecuencia_cupon_dias / dias_por_anio)) - 1
	tasa_efectiva <- tasa_efectiva * 100 
FinFuncion

Funcion cok <- retorna_cok( tasa_anual_descuento, frecuencia_cupon, dias_por_anio )
	cok <- ((1 + tasa_anual_descuento)^(frecuencia_cupon / dias_por_anio)) - 1
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

Funcion tasa <- calcula_tasa_indicador_rentabilidad( tir, dias_por_anio, frecuencia_cupon)
	tasa <- ((tir + 1)^(dias_por_anio / frecuencia_cupon) - 1) * 100
FinFuncion

Funcion sumatoria_flujo <- retornar_sumatoria_flujo(flujo, tasa , nro_periodos)
    sumatoria_flujo <- 0
    Para i<-1 Hasta nro_periodos Hacer
        sumatoria_flujo <- sumatoria_flujo + (ABS(flujo[i]) / (1 + tasa)^i) 
    Fin Para
FinFuncion

Funcion tir <- calcula_tir( inversion, flujo, nro_periodos )
	l <- 0
	r <- 1
	precision <- 0.000001
	
	Mientras l < r - precision Hacer
		mit <- TRUNC(((l + r) / 2) * 1000000) / 1000000
		
		Si retornar_sumatoria_flujo( flujo, mit, nro_periodos) < ABS(inversion) Entonces
			r <- mit - precision
		SiNo
			l <- mit
		FinSi
	Fin Mientras
		
	tir <- l * 100
FinFuncion

Funcion calculo_cronograma_pagos( valor_nominal, valor_comercial, frecuencia_cupon_dias, dias_capitalizacion, periodos_por_anio, nro_periodos, tasa_efectiva_anual, tasa_efectiva, cok, costes_iniciales_emisor, costes_iniciales_bonista, prima, impuesto_renta, dias_por_anio, plazo_gracia, tipo_plazo )
	bono_inicial <- valor_nominal
	
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
	Dimension plazos_de_gracia_bono[nro_periodos]
	
	Si plazo_gracia > 0 Entonces
		Para i <- 1 Hasta plazo_gracia Hacer
			plazos_de_gracia_bono[i] <- tipo_plazo
		FinPara
		
		Para i <- plazo_gracia + 1 Hasta nro_periodos Hacer
			plazos_de_gracia_bono[i] <- "S"
		FinPara
	Sino
		Para i <- 1 Hasta nro_periodos Hacer
			plazos_de_gracia_bono[i] <- "S"
		FinPara
	FinSi
		
	Para i desde 1 Hasta nro_periodos Hacer
		Si i = 1 Entonces
			bonos[i] <- bono_inicial
		SiNo
			Si plazos_de_gracia_bono[i - 1] = "T" Entonces
				bonos[i] <- bonos_indexados[i - 1] - cupones_interes[i - 1]
			SiNo
				bonos[i] <- bonos_indexados[i - 1] + amortizaciones[i - 1]
			FinSi
			
		FinSi
		
		bonos_indexados[i] <- bonos[i]
		cupones_interes[i] <- - bonos_indexados[i] * tasa_efectiva
		
		Si plazos_de_gracia_bono[i] = "T" Entonces
			cuotas[i] <- 0
		SiNo
			Si plazos_de_gracia_bono[i] = "P" Entonces
				cuotas[i] <- cupones_interes[i]
			SiNo
				cuotas[i] <- calcula_cuota( bonos_indexados[i], nro_periodos - i + 1, tasa_efectiva )
			FinSi
		FinSi
		
		Si plazos_de_gracia_bono[i] = "T" o  plazos_de_gracia_bono[i] = "P" Entonces
			amortizaciones[i] <- 0
		SiNo
			amortizaciones[i] <- cuotas[i] - cupones_interes[i]
		FinSi
		
		Si i = nro_periodos Entonces
			primas[i] <- -bonos_indexados[i] * prima
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
	
	flujo_emisor_inicial <- costes_iniciales_emisor - valor_comercial
	flujo_emisor_c_escudo_inicial <- flujo_emisor_inicial
	flujo_bonista_inicial <- - valor_comercial - costes_iniciales_bonista
	
	//VA
	
	
	//Indicadores de rentabilidad
	tir_tcea_emisor <- calcula_tir(flujo_emisor_inicial, flujos_emisor, nro_periodos)
	tir_tcea_emisor_c_escudo <- calcula_tir(flujo_emisor_c_escudo_inicial, flujos_emisor_escudo, nro_periodos)
	tir_trea_bonista <- calcula_tir(flujo_bonista_inicial, flujos_bonistas, nro_periodos)
	
	tcea_emisor <- calcula_tasa_indicador_rentabilidad(tir_tcea_emisor * 0.01, dias_por_anio, frecuencia_cupon_dias)
	tcea_emisor_c_escudo <- calcula_tasa_indicador_rentabilidad(tir_tcea_emisor_c_escudo * 0.01, dias_por_anio, frecuencia_cupon_dias)
	trea_bonista <- calcula_tasa_indicador_rentabilidad(tir_trea_bonista * 0.01, dias_por_anio, frecuencia_cupon_dias)
	
	//mostrar resultados
	mostrar_cronograma_pagos(nro_periodos, bonos, bonos_indexados, cupones_interes, cuotas, amortizaciones, primas, escudos, flujos_emisor, flujos_emisor_escudo, flujos_bonistas, flujos_actuales, flujos_actuales, flujos_actuales_x_plazo, factor_p_convexidad)	
	mostrar_resultados_estructuracion( frecuencia_cupon_dias, dias_capitalizacion, periodos_por_anio, nro_periodos, tasa_efectiva_anual * 100, tasa_efectiva * 100, cok * 100, costes_iniciales_emisor, costes_iniciales_bonista, tcea_emisor, tcea_emisor_c_escudo, trea_bonista )
FinFuncion

Funcion mostrar_resultados_estructuracion( frecuencia_cupon_dias, dias_capitalizacion, periodos_por_anio, nro_periodos, tasa_efectiva_anual, tasa_efectiva, cok, costes_iniciales_emisor, costes_iniciales_bonista, tcea_emisor, tcea_emisor_c_escudo, trea_bonista )
	Escribir ""
	Escribir "Frecuencia del cupon en dias: " frecuencia_cupon_dias
	Escribir "Dias de capitalizacion: " dias_capitalizacion
	Escribir "Periodos por anio: " periodos_por_anio
	Escribir "Numero total de periodos: " nro_periodos
	Escribir "Tasa efectiva anual: " tasa_efectiva_anual "%"
	Escribir "Tasa efectiva: " tasa_efectiva "%"
	Escribir "Cok: " cok "%"
	Escribir "Costes iniciales emisor: " costes_iniciales_emisor
	Escribir "Costes iniciales bonista: " costes_iniciales_bonista
	Escribir "TCEA Emisor: " tcea_emisor "%"
	Escribir "TCEA Emisor c/Escudo: " tcea_emisor_c_escudo "%"
	Escribir "TREA Bonista: " trea_bonista "%"
FinFuncion

Funcion mostrar_cronograma_pagos(nro_periodos, bonos, bonos_indexados, cupones_interes, cuotas, amortizaciones, primas, escudos, flujos_emisor, flujos_emisor_escudo, flujos_bonistas, flujos_actuales, flujos_actuales, flujos_actuales_x_plazo, factor_p_convexidad)
	Escribir "/////CRONOGRAMA DE PAGOS/////"
	Para i desde 1 Hasta nro_periodos Hacer
		Escribir "| Periodo: " i " | Bono: " bonos[i] " | Bono indexado: " bonos_indexados[i] " | Cupon: " cupones_interes[i] " | Cuota: " cuotas[i] " | Amortizacion: " amortizaciones[i] " | Prima: " primas[i] " | Escudo: " escudos[i] " | Flujo emisor: " flujos_emisor[i] " | Flujos emisor c/escudo: " flujos_emisor_escudo[i] " | Flujo bonista: " flujos_bonistas[i] " | Flujo actual: " flujos_actuales[i] " | Flujo actual x Plazo: "  flujos_actuales_x_plazo[i] " | Factor p / convexidad: " factor_p_convexidad[i]
	FinPara
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
	Definir impuesto_renta Como Real
	Definir fecha_de_emision como cadena
	
	//De los Costes/Gastos Iniciales
	Definir prima Como Real
	Definir estructuracion Como Real
	Definir colocacion Como Real
	Definir flotacion Como Real
	Definir cavali Como Real
	Definir plazo_gracia Como Entero
	Definir tipo_plazo Como Caracter
	//Ini Lectura de datos
	Escribir "Valor nominal: "
	Leer valor_nominal
	
	Escribir "Valor comercial: "
	Leer valor_comercial
	
	Escribir "Numero de anios: "
	Leer n_de_anios
	
	Escribir "Frecuencia del cupon: "
	Leer frecuencia_del_cupon
	
	Escribir "Dias por anio: "
	Leer dias_por_anio
	
	Escribir "Tipo de tasa de interes: "
	Leer tipo_tasa_interes
	
	Escribir "Capitalizacion: "
	Leer capitalizacion
	
	Escribir "Tasa de interes: "
	Leer tasa_interes
	
	Escribir "Tasa anual de descuento: "
	Leer tasa_anual_descuento
	
	Escribir "Impuesto a la renta: "
	Leer impuesto_renta
	
	Escribir "Fecha de emision: "
	Leer fecha_de_emision
	
	Escribir "Prima: "
	Leer prima
	
	Escribir "Estructuracion: "
	Leer estructuracion
	
	Escribir "Colocacion: "
	Leer colocacion
	
	Escribir "Flotacion: "
	Leer flotacion
	
	Escribir "Cavali: "
	Leer cavali
	
	Escribir "Plazo de gracia: "
	Leer plazo_gracia
	
	Escribir "Tipo de plazo de gracia: "
	Leer tipo_plazo
	//FIn lectura de datos
	
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
	
	//Calculo Resultados de la estructuracion del bono
	frecuencia_cupon_dias     	  <-          retorna_frecuencia_cupon( frecuencia_del_cupon )
	dias_capitalizacion           <-          retorna_dias_capitalizacion( capitalizacion )
	periodos_por_anio             <-          retorna_periodos_por_anio( dias_por_anio, frecuencia_cupon_dias )
	nro_periodos                  <-          retorna_nro_periodos( periodos_por_anio, n_de_anios )
	tasa_efectiva_anual           <-          retorna_tasa_efectiva_anual ( tipo_tasa_interes, tasa_interes, dias_por_anio, dias_capitalizacion )
	tasa_efectiva                 <-          retorna_tasa_efectiva ( tasa_efectiva_anual * 0.01, dias_por_anio, frecuencia_cupon_dias )
	cok                           <-          retorna_cok( tasa_anual_descuento, frecuencia_cupon_dias, dias_por_anio )
	costes_iniciales_emisor       <-          retorna_costes_iniciales_emisor( estructuracion, colocacion, flotacion, cavali, valor_comercial )                         
    costes_iniciales_bonista      <-          retorna_costes_iniciales_bonista(  flotacion, cavali, valor_comercial )

	calculo_cronograma_pagos( valor_nominal, valor_comercial, frecuencia_cupon_dias, dias_capitalizacion, periodos_por_anio, nro_periodos, tasa_efectiva_anual * 0.01, tasa_efectiva * 0.01, cok * 0.01, costes_iniciales_emisor, costes_iniciales_bonista, prima, impuesto_renta, dias_por_anio, plazo_gracia, tipo_plazo )
FinProceso