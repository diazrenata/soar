#' Add control values
#'
#' Adds columns for control values to a treatment mean df
#'
#' @param treatment_mean_df result of get_treatment_means
#'
#' @return treatment_mean_df with columns for control values at every timestep
#' @export
#'
#' @importFrom dplyr filter select rename select_at rename_at left_join
add_control_values <- function(treatment_mean_df) {

  if("CC" %in% treatment_mean_df$plot_type) {
    control_treatment <- "CC"
  } else {
    control_treatment <- "C"
  }

  add_ctrl <- function(x) {
    paste0(x, "_control")
  }

  only_control <- treatment_mean_df %>%
    dplyr::filter(plot_type == control_treatment) %>%
    dplyr::select_at(dplyr::vars(-contains("_ma"))) %>%
    dplyr::rename_at(dplyr::vars(contains("_e")), add_ctrl) %>%
    dplyr::rename(nplots_control = nplots) %>%
    dplyr::select(-plot_type)


  w_control <- dplyr::left_join(treatment_mean_df, only_control) %>%
    dplyr::select_at(dplyr::vars(-contains("_ma"))) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::mutate_at(dplyr::vars(contains("_e")), .funs = list(ma = maopts)) %>%
    dplyr::ungroup()

  w_control
}
