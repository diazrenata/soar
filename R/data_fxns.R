#' Get rodent data
#'
#' Core function for getting rodent data.
#'
#' Gives data from February 1988 until January 2020.
#'
#' @param return_plot Return plot level energy use or return treatment level. If TRUE, returns plot level totals. If F, returns mean per treatment per period
#' @param clean passed to portalr, whether to use only qa data or not
#' @param currency "energy" or "biomass"
#'
#' @return data
#' @export
#'
#' @importFrom portalr energy biomass
#' @importFrom dplyr mutate rename left_join mutate_at group_by ungroup
#' @importFrom here here
#' @importFrom stringr str_replace
get_rodent_data <- function(return_plot = F,clean =F, currency = "energy") {


  if(currency == "energy") {

    plot_level <- portalr::energy(clean = clean,
                                  level = "Plot",
                                  type = "Granivores",
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

  } else if(currency == "biomass") {

    plot_level <- portalr::biomass(clean = clean,
                                  level = "Plot",
                                  type = "Granivores",
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


  plot_level <- plot_level %>%
    dplyr::filter(period > 118,
                  period < 495) %>%
    dplyr::mutate(plot_type = combined_trt)  %>%  # control
    dplyr::filter(plot_type %in% c("CC", "EE"))


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

  treatment_means <- plots_to_treatment_means(plot_level_totals)

  if(return_plot) {
    return(plot_level_totals)
  }

  return(treatment_means)
}

#' Go from plot level totals to treatment means
#'
#' @param plot_level_totals plots
#'
#' @return treatment means
#' @export
#'
#' @importFrom dplyr group_by summarize ungroup mutate_at
#' @importFrom stringr str_replace
plots_to_treatment_means <- function(plot_level_totals) {


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

  return(treatment_means)

}

#' List which plots are which treatments
#'
#' For checking
#'
#'
#' @return dataframe of plots & treatments
#' @export
#'
#' @importFrom dplyr select distinct
list_plot_types <- function() {

  plots <- get_rodent_data(return_plot = T)

  plots %>%
    dplyr::select(plot, plot_type) %>%
    dplyr::distinct()

}

#' Get plot totals
#'
#' Quick wrapper for get_rodent_data.
#'
#' @param clean passed to portalr, whether to use only qa data or not
#' #' @param currency "energy" or "biomass"
#' @return data
#' @export
#'
get_plot_totals <- function(clean = F, currency = "energy") {

  get_rodent_data(return_plot = T,  clean = clean, currency = currency)

}

#' Get treatment means
#'
#' Quick wrapper for get_rodent_data.
#'
#' @param clean passed to portalr, whether to use only qa data or not
#' @param currency "energy" or "biomass"
#' @return data
#' @export
#'
get_treatment_means <- function(clean = F, currency = "energy") {

  get_rodent_data(return_plot = F, clean = clean, currency = currency)

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
#' Breaking the time series into 3 chunks: Feb 1988-June 1997, July 1997-January 2010, January 2010-January 2020
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
    dplyr::mutate(era = ifelse(period <= 232, "a_pre_pb",
                               ifelse(period <= 380, "b_pre_reorg", "c_post_reorg"))) %>%
    dplyr::mutate(oera = as.ordered(era))

  return(dat)

}

#' Make dataframe of time period transitions
#'
#' For plots.
#'
#' @return df
#' @export
#'
#' @importFrom dplyr left_join select rename distinct
make_era_df <- function() {

  rats <- get_treatment_means()

  era_df <- data.frame(event_name = c("C. baileyi establishment", "Reorganization event"),
                     event_period = c(233, 381),
                     no_name = c("", "")) %>%
  dplyr::left_join(dplyr::distinct(dplyr::select(rats, period, censusdate)), by = c("event_period" = "period")) %>%
  dplyr::rename(event_date = censusdate)

  return(era_df)
}
