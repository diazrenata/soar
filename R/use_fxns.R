#' Get PB data for analysis
#'
#' @param treatl df
#'
#' @return PB edf
#' @export
#'
#' @importFrom dplyr mutate group_by ungroup filter
get_pb <- function(treatl = NULL) {
  if(is.null(treatl)) {
  treatl <- get_treatment_means() %>%
    remove_switch()
  }

  pb <- treatl %>%
    dplyr::mutate(pb_prop = pb_e / total_e) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate(pb_prop_ma = maopts(pb_prop)) %>%
    dplyr::ungroup()

  return(pb)
}

#' Get EE energy use as fraction of CC
#'
#' @param treatl df
#'
#' @return df
#' @export
#'
#' @importFrom dplyr filter select rename left_join mutate group_by ungroup
get_e_ratio <- function(treatl = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means() %>%
      remove_switch()
  }


  controls <- treatl %>%
    dplyr::filter(plot_type == "CC") %>%
    dplyr::select(period, total_e, smgran_e, tinygran_e) %>%
    dplyr::rename(total_e_c = total_e, smgran_e_c = smgran_e, tinygran_e_c = tinygran_e)

  ee <-  treatl %>%
    dplyr::filter(plot_type != "CC") %>%
    dplyr::select(period, censusdate, era, oera, plot_type, oplottype, total_e, smgran_e, tinygran_e) %>%
    dplyr::left_join(controls) %>%
    dplyr::mutate(total_e_rat = total_e / total_e_c,
             smgran_e_rat = smgran_e / smgran_e_c,
             tinygran_e_rat = tinygran_e / tinygran_e_c) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate(total_e_rat_ma = maopts(total_e_rat),
             smgran_e_rat_ma = maopts(smgran_e_rat),
             tinygran_e_rat_ma = maopts(tinygran_e_rat)) %>%
    dplyr::ungroup()

  return(ee)
}

#' Get compensation
#'
#' @param treatl df
#'
#' @return df
#' @export
#'
#' @importFrom dplyr filter select rename left_join mutate group_by ungroup
get_compensation <- function(treatl = NULL) {
  if(is.null(treatl)) {
    treatl <- get_treatment_means() %>%
     remove_switch()
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
    dplyr::ungroup()

  return(compensation)
}
