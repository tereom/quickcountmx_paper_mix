# Script creates:
# 100 complete samples
# 100 samples with missing polling stations
# 100 samples with missing strata

# Details:
## Sample frame: gto_2012 (included in package quickcountmx)
# quickcountmx (devotos::github_install("tereom/quickcountmx")
## Sample selection
# We create 3 scenarios with different missing data patterns:
# 1) Complete sample: 100 samples are selected, stratified random samples where
# the stratifying variable is federal district.
# 2) Muestra faltantes_casilla: se seleccionaron 100 muestras censurando las 
#   muestras completas del inciso anterior, donde la probabilidad de que una 
#   casilla sea censurada considera los patrones observados en las remesas 2012 
#   con corte a las 22:00 horas.
# 3) Muestra faltantes_estrato: se seleccionaron 100 muestras censurando las 
#   muestras completas del inciso 1), en este caso se eliminan 3 estratos por 
#   muestra. Los estratos a censurar se seleccionan con probabilidad 
#   proporcional a los votos recibidos por el PAN en el estrato.

library(quickcountmx)
library(tidyverse)
library(lubridate)

# 1. 100 muestras completas
gto_2012

marco <- gto_2012 %>% 
    mutate(
        distrito_rural = str_c(distrito_fed_12, rural, sep = "-"), 
        distrito_rural = ifelse(distrito_rural == "5-1", "5-0", distrito_rural)
    )

seleccionar_muestras_completas <- function(i){
    muestra <- select_sample_prop(marco, stratum = distrito_loc_17, 
        frac = 0.075)
    write_csv(select(muestra, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/complete/complete_", 
        i, ".csv"))
    return(muestra)
}
set.seed(895428)
completas <- map(1:100, seleccionar_muestras_completas)

# 2. 100 faltantes a nivel casilla
# tomaremos submuestras de la muestra para simular las remesas
# la simulación de las remesas se hará de acuerdo a los datos de la elección 
# presidencial 2012

remesas_2012 <- readxl::read_excel("data_input/quickcount_presidential_2012.xlsx")

# veamos los cortes cada media hora entre 20:30 y 23:30
remesas_gto_2012 <- remesas_2012 %>% 
    filter(base_pres_id_ESTADO == "11") %>% 
    mutate(
        distrito_fed_12 = base_pres_ID_DISTRITO, 
        remesas,
        rural = (tsecc_DEOE == "Rural") * 1,
        hora_transmision = strftime(hora_transmision, format="%H:%M:%S"),
        dia_hora = FECHA_HORA
    ) %>% 
    mutate(
        remesa_2030 = dia_hora < ymd_hms("2012-07-01 20:30:00 UTC"), 
        remesa_2100 = dia_hora < ymd_hms("2012-07-01 21:00:00 UTC"),
        remesa_2130 = dia_hora < ymd_hms("2012-07-01 21:30:00 UTC"),
        remesa_2200 = dia_hora < ymd_hms("2012-07-01 22:00:00 UTC"), 
        remesa_2230 = dia_hora < ymd_hms("2012-07-01 22:30:00 UTC"),
        remesa_2300 = dia_hora < ymd_hms("2012-07-01 23:00:00 UTC"),
        remesa_2330 = dia_hora < ymd_hms("2012-07-01 23:30:00 UTC")
    ) %>% 
    mutate_at(vars(remesa_2030:remesa_2330), ~ifelse(is.na(.), 
        FALSE, .))

glimpse(remesas_gto_2012)

# usaremos las proporciones de datos recibidas por distrto federal y rural/urbano
llegada_distrito <- remesas_gto_2012 %>% 
    group_by(distrito_fed_12, rural) %>% 
    summarise_at(vars(remesa_2030:remesa_2330), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(distrito_rural = str_c(distrito_fed_12, rural, sep = "-")) %>% 
    select(distrito_rural, rural, remesa_2030:remesa_2330)

# La gráfica muestra como llegaban las remesas y que a partir de las 22:30 se
# tenía todo
llegada_distrito %>% 
    gather(horario, p_llegada, remesa_2030:remesa_2330) %>% 
    ggplot(aes(x = reorder(horario, p_llegada), y = p_llegada,
        color = factor(rural))) +
    geom_boxplot() 

# usamos corte de las 22:00
seleccionar_muestras_2200 <- function(i, muestra){
    muestra_f <- select_sample_str(sampling_frame = muestra, 
        allocation = select(llegada_distrito, distrito_rural, remesa_2200), 
        sample_size = remesa_2200, stratum = distrito_rural, is_frac = TRUE)
    write_csv(select(muestra_f, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/missing_polls_2012_trends/missing_polls_2012_trends_", 
            i, ".csv"))
    return(muestra_f)
}
set.seed(98713)
muestras_2200 <- map2(1:100, completas, ~seleccionar_muestras_2200(.x, .y))

# usamos corte de las 20:30
seleccionar_muestras_2030 <- function(i, muestra){
    muestra_f <- select_sample_str(sampling_frame = muestra, 
        allocation = select(llegada_distrito, distrito_rural, remesa_2030), 
        sample_size = remesa_2030, stratum = distrito_rural, is_frac = TRUE)
    write_csv(select(muestra_f, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/missing_polls_2012_2030/missing_polls_2012_2030_", 
            i, ".csv"))
    return(muestra_f)
}
set.seed(98713)
muestras_2030 <- map2(1:100, completas, ~seleccionar_muestras_2030(.x, .y))

# 3. 100 muestras faltantes 3 estratos
seleccionar_muestras_sesgo <- function(i, muestra){
    estratos_eliminar <- sample(unique(estrato_pan_na$distrito_loc_17), size = 3, 
        prob = estrato_pan_na$p)
    muestra_s <- filter(muestra, !(distrito_loc_17 %in% estratos_eliminar))
    write_csv(select(muestra_s, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/missing_strata_pan/missing_strata_pan_", 
            i, ".csv"))
    return(muestra_s)
}
estrato_pan_na <- marco %>% 
    group_by(distrito_loc_17) %>% 
    summarise(p = sum(pan_na) / sum(total))

set.seed(4654678)
muestras_sesgo <- map2(1:100, completas, ~seleccionar_muestras_sesgo(.x, .y))

# 4. 100 muestras faltantes casillas proporcional votos pan
seleccionar_muestras_sesgo_casilla <- function(i, muestra){
    muestra_s <- muestra %>% sample_frac(size = 0.85, weight = pan_na / ln_total)
    write_csv(select(muestra_s, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/missing_polls_pan/missing_polls_pan_", 
            i, ".csv"))
    return(muestra_s)
}

set.seed(8931828)
muestras_sesgo_casilla <- map2(1:100, completas, 
    ~seleccionar_muestras_sesgo_casilla(.x, .y))

# seleccionar con distinta prob de acuerdo a tipo_seccion
seleccionar_muestras_sesgo_tipo <- function(i, muestra){
    tipo_seccion_prob <- tibble(tipo_seccion = c("M", "R", "U"), 
        prob_miss = c(2, 1, 3))
    muestra_s <- muestra %>% 
        left_join(tipo_seccion_prob, by = "tipo_seccion") %>% 
        mutate(prob_miss = ifelse(is.na(prob_miss), 2, prob_miss)) %>% 
        sample_frac(size = 0.80, weight = prob_miss)
    write_csv(select(muestra_s, -distrito_rural), 
        path = stringr::str_c("data_output/calibration_samples/missing_polls_type/missing_polls_type_", 
            i, ".csv"))
    return(muestra_s)
}

set.seed(180983)
muestras_sesgo_tipo <- map2(1:100, completas, 
    ~seleccionar_muestras_sesgo_tipo(.x, .y))

map_dbl(completas, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_2200, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_sesgo, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_sesgo_casilla, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_sesgo_tipo, ~n_distinct(.$distrito_loc_17)) %>% table()


map_dbl(completas, nrow) %>% mean()
map_dbl(muestras_2200, nrow) %>% mean()
map_dbl(muestras_sesgo, nrow) %>% mean()
map_dbl(muestras_sesgo_casilla, nrow) %>% mean()
map_dbl(muestras_sesgo_tipo, nrow) %>% mean()

compara <- tibble(
    completa = map_dbl(completas, ~sum(.$pan_na) / sum(.$total)), 
    casilla_2200 = map_dbl(muestras_2200, ~sum(.$pan_na) / sum(.$total)), 
    sesgo_estrato = map_dbl(muestras_sesgo, ~sum(.$pan_na) / sum(.$total)), 
    sesgo_casilla = map_dbl(muestras_sesgo, ~sum(.$pan_na) / sum(.$total)))

ggplot(compara) +
    geom_point(aes(x = completa, y = casilla_2200, color = "Casilla")) +
    geom_abline() +
    coord_equal() +
    geom_point(aes(x = completa, y = sesgo_estrato, color = "Sesgo")) +
    geom_point(aes(x = sum(marco$pan_na) / sum(marco$total), 
        y = sum(marco$pan_na) / sum(marco$total)), color = "red", size = 2)
