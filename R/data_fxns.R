#' Get rodent data
#'
#' Core function for getting rodent data.
#'
#' Gives data from February 1988 until either March 2015 or May 2019, depending on use_pre_switch.
#'
#' @param use_christensen_plots Early in development I was working from the plots used in Christensen (2019 ProcB). Defaults F
#' @param return_plot Return plot level energy use or return treatment level. If TRUE, returns plot level totals. If F, returns mean per treatment per period.
#' @param use_pre_switch Use data up to the treatment switch in 2015? If yes, allows for more plots of each treatment type.
#' @param currency "energy" (default) or "abundance"
#' @param clean passed to portalr, whether to use only qa data or not
#'
#' @return data
#' @export
#'
#' @importFrom portalr energy
#' @importFrom dplyr mutate rename left_join mutate_at group_by ungroup
#' @importFrom here here
#' @importFrom stringr str_replace
get_rodent_data <- function(use_christensen_plots = F, return_plot = F, use_pre_switch = F, currency = "energy", clean =F) {

  if(currency == "energy") {
  plot_level <- portalr::energy(clean = clean,
                                level = "Plot",
                                type = "Granivores", # this removes NA, OL, OT, ...cotton rats, perhaps?
                                plots = "all",
                                unknowns = F,
                                shape = "crosstab",
                                time = "all",
                                na_drop = T,
                                zero_drop = F,
                                min_traps = 45, # allow partially trapped plots - 45 or 47, of 49, plots. Necessary bc apparently plot 24 was often trapped to 47 for the 2010s.
                                min_plots = 24,
                                effort = T
  ) %>%
    add_eras() %>%
    add_plot_types()
  } else if(currency == "abundance") {
    plot_level <- portalr::abundance(clean = T,
                                  level = "Plot",
                                  type = "Granivores", # this removes NA, OL, OT, ...cotton rats, perhaps?
                                  plots = "all",
                                  unknowns = F,
                                  shape = "crosstab",
                                  time = "all",
                                  na_drop = T,
                                  zero_drop = F,
                                  min_traps = 45, # allow partially trapped plots - 45 or 47, of 49, plots. Necessary bc apparently plot 24 was often trapped to 47 for the 2010s.
                                  min_plots = 24,
                                  effort = T
    ) %>%
      add_eras() %>%
      add_plot_types()
  }


  if(use_pre_switch) {

    plot_level <- plot_level %>%
      dplyr::filter(Use_first,
                    period > 118,
                    period < 436) %>%
      dplyr::mutate(plot_type =
                      first_trt)

  } else {
    if(use_christensen_plots) {
      plot_level <- plot_level %>%
        dplyr::filter(combined_trt %in% c("RC", "EC", "CC"),
                      period > 118) %>%
        dplyr::mutate(plot_type = combined_trt) # control
    } else {
      plot_level <- plot_level %>%
        dplyr::filter(Use_second,
                      period > 118) %>%
        dplyr::mutate(plot_type = combined_trt)  # control
    }
  }


  rodent_names <- c('BA','DM','DO','DS','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO')
  dipo_names <- c('DM', 'DO', 'DS')
  smgran_names <- c('BA','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO')
  tinygran_names <- c('BA','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO')

  plot_level_totals <- plot_level %>%
    dplyr::mutate(total_e = rowSums(.[rodent_names]),
                  dipo_e = rowSums(.[dipo_names]),
                  smgran_e = rowSums(.[smgran_names]),
                  tinygran_e = rowSums(.[tinygran_names]),
                  pb_e = PB,
                  pp_e = PP) %>%
    dplyr::select(period, censusdate, era, oera, plot, plot_type, total_e, dipo_e, smgran_e, pb_e, pp_e, tinygran_e) %>%
    dplyr::mutate(censusdate = as.Date(censusdate),
                  oplottype = ordered(plot_type),
                  fplottype = as.factor(plot_type),
                  fplot = as.factor(plot)
    ) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate_at(c("total_e", "dipo_e", "smgran_e", "pb_e", "pp_e", "tinygran_e"), .funs = list(ma = maopts)) %>%
    dplyr::ungroup()

  treatment_means <- plots_to_treatment_means(plot_level_totals, currency = currency)

  if(currency == "abundance") {

    plotcols <- colnames(plot_level_totals)

    plotcols_to_change <- plotcols[ which(grepl("_e", plotcols))]

    new_plotcols <- stringr::str_replace(plotcols_to_change, "_e", "_n")

    colnames(plot_level_totals)[ which(grepl("_e", plotcols))] <- new_plotcols


  }

  if(return_plot) {
    return(plot_level_totals)
  }

  return(treatment_means)
}

#' Go from plot level totals to treatment means
#'
#' @param plot_level_totals plots
#' @param currency "energy" or "abundance"
#'
#' @return treatment means
#' @export
#'
#' @importFrom dplyr group_by summarize ungroup mutate_at
#' @importFrom stringr str_replace
plots_to_treatment_means <- function(plot_level_totals, currency) {


  treatment_means <- plot_level_totals %>%
    dplyr::group_by(period, censusdate, era, oera, plot_type, oplottype) %>%
    dplyr::summarize(total_e = mean(total_e),
                     dipo_e = mean(dipo_e),
                     smgran_e = mean(smgran_e),
                     tinygran_e = mean(tinygran_e),
                     pb_e = mean(pb_e),
                     pp_e = mean(pp_e),
                     nplots = dplyr::n()) %>%
    dplyr::ungroup()%>%
    dplyr::group_by(plot_type, oplottype) %>%
    dplyr::mutate_at(c("total_e", "dipo_e", "smgran_e", "pb_e", "pp_e", "tinygran_e"), .funs = list(ma = maopts)) %>%
    dplyr::ungroup()

  if(currency == "abundance") {
    treatcols <- colnames(treatment_means)

  treatcols_to_change <- treatcols[ which(grepl("_e", treatcols))]

  new_treatcols <- stringr::str_replace(treatcols_to_change, "_e", "_n")

  colnames(treatment_means)[ which(grepl("_e", treatcols))] <- new_treatcols
  }

  return(treatment_means)

}

#' List which plots are which treatments
#'
#' For checking
#'
#' @param use_pre_switch use treatments to 2015?
#'
#' @return dataframe of plots & treatments
#' @export
#'
#' @importFrom dplyr select distinct
list_plot_types <- function(use_pre_switch = F) {

  plots <- get_rodent_data(return_plot = T, use_pre_switch = use_pre_switch)

  plots %>%
    dplyr::select(plot, plot_type) %>%
    dplyr::distinct()

}

#' Get plot totals
#'
#' Quick wrapper for get_rodent_data.
#'
#' @param use_pre_switch use pre switch T/F
#' @param currency "energy" or "abundance"
#' @param clean passed to portalr, whether to use only qa data or not
#'
#' @return data
#' @export
#'
get_plot_totals <- function(use_pre_switch = F, currency = "energy", clean = F) {

  get_rodent_data(return_plot = T, use_pre_switch = use_pre_switch, currency = currency, clean = clean)

}

#' Get treatment means
#'
#' Quick wrapper for get_rodent_data.
#'
#' @param use_pre_switch use pre switch T/F
#' @param currency "energy" or "abundance"
#' @param clean passed to portalr, whether to use only qa data or not
#' @return data
#' @export
#'
get_treatment_means <- function(use_pre_switch = F, currency = "energy", clean = F) {

  get_rodent_data(return_plot = F, use_pre_switch = use_pre_switch, currency = currency, clean = clean)

}



#' Moving average with default options
#'
#' @param x the thing to average
#' @param n window length, default 6
#' @param type type, default "s"
#'
#' @return movavg
#' @export
#'
#' @importFrom pracma movavg
maopts <- function(x, n = 6, type = "s") {
  pracma::movavg(x, n = n, type = type)
}

#' Add plot types
#'
#' @param dat A dataset with column Plot
#'
#' @return dat with plot types added
#' @export
#'
#' @importFrom dplyr rename left_join
add_plot_types <- function(dat) {

  plot_treatments <- plot_treatments %>%
    dplyr::rename(plot = Plot)

  dat <- dat %>%
    dplyr::left_join(plot_treatments)

  return(dat)
}

#' Add "eras"
#'
#' Breaking the time series into 4 chunks: prior to 1996, 1996-ca 2010, 2010-2015, post plot switch
#'
#' @param dat Dataset with column "period"
#'
#' @return dat with column "era" added
#' @export
#'
#' @importFrom dplyr mutate
add_eras <- function(dat) {
  dat <- dat %>%
    dplyr::mutate(era = NA) %>%
    dplyr::mutate(era = ifelse(period <= 216, "a_pre_ba",
                               ifelse(period <= 380, "b_pre_cpt",
                                      ifelse(period <= 436, "c_pre_switch", "d_post-switch")))) %>%
    dplyr::mutate(oera = as.ordered(era))

  return(dat)

}

#' Add temporary treatments
#'
#' @param dat dataframe iwth plot_type, period
#'
#' @return dat with column temp_plot_type for what treatment a plot had at each timestep
#' @export
#'
#' @importFrom dplyr group_by_all mutate ungroup left_join
add_temp_treatments <- function(dat) {

  temp_treatments <- expand.grid(plot_type = unique(dat$plot_type),
                                 period = unique(dat$period)) %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(temp_plot_type = ifelse(period <= 436, substr(as.character(plot_type), 1, 1), substr(as.character(plot_type), 2, 2))) %>%
    dplyr::ungroup()

  dat <- dplyr::left_join(dat, temp_treatments)

  return(dat)
}

#' Get Portal plants ready for ldats
#'
#' Returns list of abundance (wide abundance matrix, no 0 years) and covariates.
#'
#' @param census_season "winter" or summmer
#' @param plot_type "CC", "CE", "EE", "EC"
#'
#' @return ready for ldats
#' @export
#'
#' @importFrom dplyr filter select rename
get_plants_annual_ldats <- function(census_season = "winter", plot_type = "CC") {

  plant_dat <- get_treatment_abundance_matrix(census_season = census_season) %>%
    dplyr::filter(year > 1988)

  this_dat <- plant_dat %>%
    dplyr::filter(combined_trt == plot_type,
                  total_abundance > 0)

  abundance <- dplyr::select(this_dat, -c(year, season, combined_trt, total_plots, total_empty_plots, total_abundance))

  covariates <- dplyr::select(this_dat, year, season, combined_trt, total_plots) %>%
    dplyr::rename(plot_type = combined_trt)

  abund_dat <- list(abundance = abundance,
                    covariates = covariates)

  return(abund_dat)
}

#' Get plot-level plant data
#'
#' For winter and summer censuses for CC, EE plots from 1988-2019, all plots are censused and all plots have 16 quadrats censused. There are however plots that had no individuals. This function adds those in as empties (0 abundance)
#'
#' @param census_season "winter" or "summer"
#'
#' @return wide dataframe of species abundance per plot, including records for empty plots.
#' @export
#'
#' @importFrom portalr plant_abundance load_plant_data
#' @importFrom dplyr filter group_by summarize ungroup select mutate group_by_all left_join
#' @importFrom tidyr pivot_wider
get_plants <- function(census_season = "winter") {


  na_to_zero <- function(val) {
    if(is.na(val)) {
      return(0)
    }
    return(val)
  }

  if(census_season == "winter") {
    quadrats <- portalr::plant_abundance(level = "Plot", type = "Winter Annuals", plots = "all")
  } else {
    quadrats <- portalr::plant_abundance(level = "Plot", type = "Summer Annuals", plots = "all")

  }

  plant_tables <- portalr::load_plant_data()

  plant_cenuses <- plant_tables$census_table

  all_plots_censused <- plant_cenuses %>%
    dplyr::filter(season == census_season,
                  censused == 1,
                  year >= 1988,
                  year < 2021) %>%
    dplyr::group_by(year, season, plot) %>%
    dplyr::summarize(quadrats_censused = sum(censused)) %>%
    dplyr::ungroup() %>%
    add_plot_types() %>%
    dplyr::select(year, season, plot, quadrats_censused, combined_trt) %>%
    dplyr::filter(combined_trt %in% c("CC", "EE"))

  all_plots_quaddat <- all_plots_censused %>%
    dplyr::left_join(quadrats) %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(empty_plot = is.na(species),
                  abundance = na_to_zero(abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, plot, season) %>%
    dplyr::mutate(plot_total_abundance = sum(abundance))

  quadrats_wide <- all_plots_quaddat %>%
    tidyr::pivot_wider(id_cols = c(year, plot, season, combined_trt, empty_plot, quadrats_censused, plot_total_abundance), names_from = species, values_from = abundance, values_fill = 0) %>%
    dplyr::select(-`NA`)

  return(quadrats_wide)
}

#' Get Erodium data (plot level)
#'
#' Includes rows for empty plots.
#'
#' @param census_season "winter"
#'
#' @return df
#' @export
#'
#' @importFrom dplyr select distinct group_by summarize ungroup rename mutate
get_erodium_plot <- function(census_season = "winter") {

  plant_dat <- get_plants(census_season = census_season)

  plots_per_treatment <- plant_dat %>%
    dplyr::select(year, season, combined_trt, plot, empty_plot) %>%
    dplyr::distinct() %>%
    dplyr::group_by(year, season, combined_trt) %>%
    dplyr::summarize(
      total_plots = length(unique(plot)),
      empty_plots = sum(empty_plot)
    ) %>%
    dplyr::ungroup()

  plot_totals <- plant_dat %>%
    dplyr::select(year, season, plot, combined_trt, empty_plot, plot_total_abundance, `erod cicu`) %>%
    dplyr::rename(erod_plot_abundance = `erod cicu`) %>%
    dplyr::mutate(erod_plot_prop_abundance = erod_plot_abundance / plot_total_abundance)

  return(plot_totals)
}

#' Get Erodium data (treatment level)
#'
#' To use these data, note that there are 5 EE and 4 CC plots. The prop data is fine but you want to standardize the totals.
#'
#' @param census_season "winter"
#'
#' @return df
#' @export
#'
#' @importFrom dplyr group_by summarize ungroup mutate
get_erodium_treatment <- function(census_season = "winter") {

  erodium_plots <- get_erodium_plot(census_season = census_season)

  erodium_treatments <- erodium_plots %>%
    dplyr::group_by(year, season, combined_trt) %>%
    dplyr::summarize(
      erod_total_abundance = sum(erod_plot_abundance),
      total_total_abundance = sum(plot_total_abundance),
      total_plots = length(unique(plot)),
      total_empty_plots = sum(empty_plot)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(erod_treatment_prop_abundance = erod_total_abundance / total_total_abundance)


  return(erodium_treatments)
}

#' Get plot-level plant abundance matrix
#'
#' Includes rows for empty plots.
#'
#' @param census_season "winter" or "summer"
#'
#' @return df
#' @export
#'
get_plot_abundance_matrix <- function(census_season = "winter") {

  plant_dat <- get_plants(census_season = census_season)

  return(plant_dat)
}

#' Get treatment-level plant abundance matrix
#'
#' Includes rows for empty treatments, columns for number of plots, total abundance, and number of empty plots.
#'
#' Note that there are 4 CC and 5 EE plots. Before using, you want to standardize.
#'
#' @param census_season "winter" or "summer"
#'
#' @return df
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup summarize_at all_of
get_treatment_abundance_matrix <- function(census_season = "winter") {
  plant_dat <- get_plants(census_season = census_season)

  plantcols <- colnames(plant_dat)[8:ncol(plant_dat)]

  treatment_dat <- plant_dat %>%
    dplyr::group_by(year, season, combined_trt) %>%
    dplyr::mutate(total_plots = length(unique(plot)),
                  total_empty_plots = sum(empty_plot),
                  total_abundance = sum(plot_total_abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, season, combined_trt, total_plots, total_empty_plots, total_abundance) %>%
    dplyr::summarize_at(dplyr::all_of(plantcols), .funs = sum) %>%
    dplyr::ungroup()

  return(treatment_dat)

}

#' Remove switch
#'
#' @param a_df df with cols plot_type, era
#'
#' @return df filterd to longterm plots and with the final era changed to c_post_cpt
#' @export
#'
#' @importFrom dplyr filter group_by_all mutate ungroup
remove_switch <- function(a_df) {

  a_df %>%
    dplyr::filter(plot_type %in% c("CC", "EE")) %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(era = ifelse(era %in% c("c_pre_switch", "d_post-switch"), "c_post_cpt", era)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(oera = as.ordered(era),
                  oplottype = as.ordered(plot_type))

}
