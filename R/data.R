#' Get rodent data
#'
#' Core function for getting rodent data.
#'
#' @param use_christensen_plots Early in development I was working from the plots used in Christensen (2019 ProcB). Defaults F
#' @param return_plot Return plot level energy use or return treatment level. If TRUE, returns plot level totals. If F, returns mean per treatment per period.
#' @param use_pre_switch Use data up to the treatment switch in 2015? If yes, allows for more plots of each treatment type.
#'
#' @return data
#' @export
#'
#' @importFrom portalr energy
#' @importFrom dplyr mutate rename left_join
#' @importFrom here here
get_rodent_data <- function(use_christensen_plots = F, return_plot = F, use_pre_switch = F) {

  plot_level <- portalr::energy(clean = T,
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

    dplyr::mutate(era = NA) %>%
    dplyr::mutate(era = ifelse(period <= 216, "a_pre_ba",
                               ifelse(period <= 380, "b_pre_cpt",
                                      ifelse(period <= 434, "c_pre_switch", "d_post-switch"))))


  plot_treatments <- read.csv(here::here("supporting_files", "plot_treatments.csv")) %>%
    dplyr::rename(plot = Plot)

  plot_level <- plot_level %>%
    dplyr::left_join(plot_treatments)

  if(use_pre_switch) {

    plot_level <- plot_level %>%
      dplyr::filter(Use_first,
                    period > 118,
                    period < 434) %>%
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
    dplyr::select(period, censusdate, era, plot, plot_type, total_e, dipo_e, smgran_e, pb_e, pp_e, tinygran_e) %>%
    dplyr::mutate(censusdate = as.Date(censusdate),
                  oplottype = ordered(plot_type)
    )


  treatment_means <- plot_level_totals %>%
    dplyr::group_by(period, censusdate, era, plot_type) %>%
    dplyr::summarize(total_e = mean(total_e),
                     dipo_e = mean(dipo_e),
                     smgran_e = mean(smgran_e),
                     tinygran_e = mean(tinygran_e),
                     pb_e = mean(pb_e),
                     pp_e = mean(pp_e),
                     nplots = dplyr::n()) %>%
    dplyr::ungroup()

  if(return_plot) {
    return(plot_level_totals)
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
