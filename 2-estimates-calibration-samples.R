library(quickcountmx)
library(tidyverse)

# samples
paths_complete <- list.files("data_output/calibration_samples/complete", 
    full.names = TRUE)
paths_missing_polls_2012 <- list.files("data_output/calibration_samples/missing_polls_2012_trends", 
    full.names = TRUE)
paths_missing_strata_pan <- list.files("data_output/calibration_samples/missing_strata_pan", 
    full.names = TRUE)
paths_missing_polls_pan <- list.files("data_output/calibration_samples/missing_polls_pan", 
    full.names = TRUE)

####### NNP (normal no-pooling)
gto_strata <- gto_2012 %>% 
    group_by(distrito_loc_17) %>% 
    summarise(n = sum(ln_total))

process_sample_nnp <- function(path) {
    sample <- read_csv(path) 
    estimation_nnp <- nnp_estimation(sample, pri_pvem:otros, ln = ln_total,
        stratum = distrito_loc_17, data_stratum = gto_strata, n_stratum = n)
    estimation_nnp$lambdas_summary %>% 
        mutate(n_sample = parse_number(basename(path))) %>% 
        select(n_sample, party, est = mean_post, LI = q_low, LS = q_sup)
}

complete_nnp <- map_df(paths_complete, process_sample_nnp)
write_csv(complete_nnp, "data_output/calibration_estimates/complete_nnp.csv")

missing_2012_nnp <- map_df(paths_missing_polls_2012, process_sample_nnp)
write_csv(missing_2012_nnp, "data_output/calibration_estimates/missing_polls_2012_trends_nnp.csv")

missing_strata_pan_nnp <- map_df(paths_missing_strata_pan, process_sample_nnp)
write_csv(missing_strata_pan_nnp, "data_output/calibration_estimates/missing_strata_pan_nnp.csv")

missing_polls_pan_nnp <- map_df(paths_missing_polls_pan, process_sample_nnp)
write_csv(missing_polls_pan_nnp, "data_output/calibration_estimates/missing_polls_pan_nnp.csv")


######## Ratio
process_sample_ratio <- function(path) {
    sample <- read_csv(path) 
    strata <- unique(gto_strata$distrito_loc_17)
    present <- unique(sample$distrito_loc_17)
    missing <- setdiff(strata, present)
    data_strata <- gto_strata %>% 
        rowwise() %>% 
        mutate(estrato_coll = ifelse(distrito_loc_17 %in% missing, 
            sample(present, 1), distrito_loc_17)
        ) %>% 
        ungroup()
    data_strata_coll <- data_strata %>%
        group_by(estrato_coll) %>%
        summarise(n_coll = sum(n)) 
    sample_coll <- sample %>%
        left_join(data_strata, by = c("distrito_loc_17"))
    ratio_estimation(sample_coll, stratum = estrato_coll, 
        data_stratum = data_strata_coll,
        n_stratum = n_coll, ... =  pri_pvem:otros, B = 500) %>% 
        mutate(
            n_sample = parse_number(basename(path)), 
            LI = r - 1.96 * std_error, 
            LS = r + 1.96 * std_error) %>% 
        select(n_sample, party, est = r, LI, LS)
}

complete_ratio <- map_df(paths_complete, process_sample_ratio)
write_csv(complete_ratio, "data_output/calibration_estimates/complete_ratio.csv")

missing_2012_ratio <- map_df(paths_missing_polls_2012, process_sample_ratio)
write_csv(missing_2012_ratio, "data_output/calibration_estimates/missing_polls_2012_trends_ratio.csv")

missing_strata_pan_ratio <- map_df(paths_missing_strata_pan, process_sample_ratio)
write_csv(missing_strata_pan_ratio, "data_output/calibration_estimates/missing_strata_pan_ratio.csv")

missing_polls_pan_ratio <- map_df(paths_missing_polls_pan, process_sample_ratio)
write_csv(missing_polls_pan_ratio, "data_output/calibration_estimates/missing_polls_pan_ratio.csv")

#### Heavy MM

process_sample_hmm <- function(path) {
    sample <- read_csv(path) 
    estimation_hmm <- mrp_estimation(sample,  ... = pri_pvem:otros,
        stratum = distrito_loc_17, 
        n_iter = 3000, n_burnin = 1500, n_chains = 1,
        parallel = TRUE)
    estimation_hmm$post_summary %>% 
        mutate(n_sample = parse_number(basename(path))) %>% 
        select(n_sample, party, est = mean_post, LI = int_l, LS = int_r)
}

complete_hmm <- map_df(paths_complete, process_sample_hmm)
write_csv(complete_hmm, "data_output/calibration_estimates/complete_hmm.csv")

missing_2012_hmm <- map_df(paths_missing_polls_2012, process_sample_hmm)
write_csv(missing_2012_hmm, "data_output/calibration_estimates/missing_polls_2012_trends_hmm.csv")

missing_strata_pan_hmm <- map_df(paths_missing_strata_pan, process_sample_hmm)
write_csv(missing_strata_pan_hmm, "data_output/calibhration_estimates/missing_strata_pan_hmm.csv")

missing_polls_pan_hmm <- map_df(paths_missing_polls_pan, process_sample_hmm)
write_csv(missing_polls_pan_hmm, "data_output/calibration_estimates/missing_polls_pan_hmm.csv")


### gráficas calibración
library(fs)
resultados_gto <- path("data/calib_gto")
csvs_le <- dir_ls(resultados_gto, regexp = "_le")
gto_le <- csvs_le %>% 
    map_df(read_csv, .id = "tipo") %>% 
    mutate(
        tipo = str_extract(basename(tipo), "^.*(?=(_gto))"), 
        metodo = "NNP"
    ) 

csvs_ma <- dir_ls(resultados_gto, regexp = "anzarut")
gto_ma <- map_df(csvs_ma, read_csv, .id = "tipo") %>% 
    mutate(
        tipo = str_extract(basename(tipo), "^.*(?=(_gto))"), 
        metodo = "Heavy-MM"
    ) 

rds_razon <- dir_ls(resultados_gto, regexp = "razon")
gto_razon <- map_df(rds_razon, read_rds, .id = "tipo") %>% 
    mutate(
        tipo = str_extract(basename(tipo), "^.*(?=(_gto))"), 
        tipo = str_replace(tipo, "razon_", ""),
        tipo = ifelse(tipo == "completos", "completas", tipo),
        metodo = "Ratio", 
        n_muestra = parse_number(path_file(nom_sim)), 
        LI = r - 2 * std_error, 
        LS = r + 2 * std_error
    ) %>% 
    select(tipo, n_muestra, partido = party, est = r, LI, LS, tipo, metodo)

gto_calib_ma_le_razon <- bind_rows(gto_ma, gto_le, gto_razon)

actual <- gto_2012 %>% 
    gather(partido, n_votes, pri_pvem:otros) %>% 
    group_by(partido) %>% 
    summarise(n_votes = sum(n_votes)) %>% 
    mutate(rank = rank(-n_votes)) %>% 
    ungroup() %>% 
    mutate(prop_votes = 100 * n_votes / sum(n_votes))

gto_calib <- gto_calib_ma_le_razon %>% 
    left_join(actual, by = "partido") %>% 
    mutate(
        precision = (LS - LI) / 2,
        cubre = LI < prop_votes & LS > prop_votes, 
        n_muestra_factor = reorder(factor(n_muestra), precision)
    )

tab_coberturas <- gto_calib %>% 
    group_by(metodo, partido, tipo) %>% 
    summarise(
        cobertura = round(100 * mean(cubre)) 
    ) 

tab_precisiones <- gto_calib %>% 
    group_by(metodo, partido, tipo) %>% 
    summarise(
        precision = round(mean(precision), 2)
    ) 

tab_cob_pres <- tab_coberturas %>% 
    left_join(tab_precisiones, by = c("partido", "tipo", "metodo")) %>% 
    filter(partido != "Otros") %>% 
    ungroup() %>% 
    mutate(
        tipo = case_when(
            tipo == "completas" ~ "complete",
            tipo == "faltantes_casilla" ~ "missing polls",
            tipo == "faltantes_estrato" ~ "missing strata"
        ), 
        tipo = factor(tipo, c("complete", "missing polls", 
            "missing strata")), 
        partido = case_when(
            partido == "mc" ~ "MC",
            partido == "otros" ~ "Other",
            partido == "pan_na" ~ "PAN",
            partido == "prd" ~ "PRD",
            partido == "pri_pvem" ~ "PRI",
            partido == "pt" ~ "PT"
        )) 

party_colors <- c(PAN = "#3399FF", PRI = "#00CD66", PRD = "#FFCC00", PT = "red",
    MC = "#80DEEA", Other = "blue")

calib_gto <- ggplot(filter(tab_cob_pres, partido != "Other"), aes(x = metodo, 
    y = precision, 
    fill = reorder(partido, precision))) +
    geom_col(position = "dodge", show.legend = FALSE) +
    facet_wrap(~tipo, ncol = 1) + 
    geom_text(aes(label = cobertura), position = position_dodge(width = 1), 
        vjust = 0, size = 3, color = "gray20", hjust = 0.5) +
    scale_fill_manual(values = party_colors) +
    # theme_minimal() +
    labs(fill = "", x = "", y = "precision") +
    theme(text = element_text(size = 12)) +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.25))

# ggplot(filter(tab_cob_pres, partido != "Other"), aes(x = cobertura, 
#     y = precision, color = partido)) +
#     geom_point() +
#     facet_grid(metodo ~ tipo)

ggsave(calib_gto, filename = "img/calib_gto.eps", device = "eps", width = 5, 
    height = 4.5, units = "in")


library(scales)
tab_cob_pres <- tab_cob_pres %>% 
    filter(partido != "Other") %>% 
    mutate(metodo = factor(metodo, levels = c("Ratio", "NNP", "Heavy-MM")))
ggplot(tab_cob_pres, aes(x = metodo, y = cobertura, 
    fill = reorder(partido, cobertura))) +
    geom_hline(yintercept = 95, color = "red", size = 0.3) +
    geom_col(position = "dodge", show.legend = FALSE, alpha = 0.8) +
    facet_wrap(~tipo, ncol = 1) + 
    geom_text(aes(label = precision), position = position_dodge(width = 1), 
        vjust = 0, size = 3, color = "gray20", hjust = 0.5) +
    scale_fill_manual(values = party_colors) +
    # theme_minimal() +
    labs(fill = "", x = "", y = "coverage") +
    theme(text = element_text(size = 12)) +
    scale_y_continuous(limits = c(60, 110), breaks = seq(60, 100, 10),
        oob = rescale_none)

ggsave(filename = "img/calib_coverage_gto.pdf", device = "pdf", 
    width = 5, height = 4.5, units = "in")

