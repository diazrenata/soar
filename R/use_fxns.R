#' Get PB data for analysis
#'
#' Calculate the proportion of total energy use accounted for by C. baileyi (species code PB) in each time step for each treatment type.
#'
#' @param treatl result of `get_treatment_means`
#'
#' @return `treatl` with added columns pb_prop, the proportion of total energy accounted for by C. baileyi, and pb_prop_ma, the 6-month moving average of pb_prop.
#' @export
#'
#' @importFrom dplyr mutate group_by ungroup filter select
get_pb <- function(treatl = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means()
  }

  pb <- treatl %>%
    dplyr::mutate(pb_prop = pb_e / total_e) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate(pb_prop_ma = maopts(pb_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, pb_prop, pb_prop_ma)

  return(pb)
}

#' Get Dipo data for analysis.
#'
#' Get kangaroo rat proportional energy use on control plots in each time step for each treatment type.
#'
#' @param treatl result of `get_treatment_means`
#'
#' @return `treatl` with added columns dipo_prop, the proportion of total energy accounted for by all Dipodomys, and dipo_prop_ma, the 6-month moving average of dipo_prop_ma.
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate group_by ungroup filter select
get_dipo_c <- function(treatl  = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means()
  }

  dipo_dat <- treatl %>%
    dplyr::mutate(dipo_prop = dipo_e / total_e) %>%
    dplyr::group_by(oplottype) %>%
    dplyr::mutate(dipo_prop_ma = maopts(dipo_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, dipo_prop, dipo_prop_ma)

  dipo_c_dat <- dipo_dat %>%
    dplyr::filter(oplottype == "CC")

  return(dipo_c_dat)
}

#' Get EE energy use as fraction of CC
#'
#' For each timestep, calculate ratio of total granivore energy use on exclosure plots to control plots.
#'
#' @param treatl result of `get_treatment_means`
#'
#' @return dataframe with column `total_e_rat`, the ratio of `treatl$total_e` on exclosures relative to controls in each timestep, and `total_e_rat_ma`, the moving average.
#' @export
#'
#' @importFrom dplyr filter select rename left_join mutate group_by ungroup
get_e_ratio <- function(treatl = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means()
  }


  controls <- treatl %>%
    dplyr::filter(plot_type == "CC") %>%
    dplyr::select(period, total_e, smgran_e, tinygran_e) %>%
    dplyr::rename(total_e_c = total_e, smgran_e_c = smgran_e, tinygran_e_c = tinygran_e)

  ee <-  treatl %>%
    dplyr::filter(plot_type != "CC") %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, total_e, smgran_e, tinygran_e) %>%
    dplyr::left_join(controls) %>%
    dplyr::mutate(total_e_rat = total_e / total_e_c) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate(total_e_rat_ma = maopts(total_e_rat)) %>%
    dplyr::ungroup() %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, total_e_rat, total_e_rat_ma)

  return(ee)
}

#' Get compensation
#'
#' Calculate degree to which small granivores on exclosure plots compensate for kangaroo rat removal. Compensation calculated as (SmgranEnergy_Exclosures - SmgranEnergy_Controls) / DipoEnergy_Controls.
#'
#' @param treatl result of `get_treatment_means`
#'
#' @return df with columns `smgran_comp`, compensation as defined above, and `smgran_comp_ma`, the 6-month moving average.
#' @export
#'
#' @importFrom dplyr filter select rename left_join mutate group_by ungroup
get_compensation <- function(treatl = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means()
  }

  controls <- treatl %>%
    dplyr::filter(plot_type == "CC") %>%
    dplyr::select(period, dipo_e, smgran_e) %>%
    dplyr::rename(dipo_e_c = dipo_e, smgran_e_c = smgran_e)

  compensation <- treatl %>%
    dplyr::filter(plot_type != "CC") %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, smgran_e) %>%
    dplyr::left_join(controls) %>%
    dplyr::mutate(smgran_increase = smgran_e - smgran_e_c) %>%
    dplyr::mutate(smgran_comp = smgran_increase / dipo_e_c)%>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate(smgran_comp_ma = maopts(smgran_comp)) %>%
    dplyr::ungroup() %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, smgran_comp, smgran_comp_ma)

  return(compensation)
}
