#' Get (invlink) estimates from GLM
#'
#' @param glm_mod model
#' @param glm_dat newdat
#'
#' @return estimates
#' @export
#'
#' @importFrom dplyr mutate right_join select
est_glm_ilink <- function(glm_mod, glm_dat) {

  ilink <- glm_mod$family$linkinv

  glm_est <- predict(glm_mod, type = "link", se.fit = T, newdata = glm_dat) %>%
    as.data.frame() %>%
    dplyr::mutate(est = ilink(fit),
           lower = ilink(fit - 2*se.fit),
           upper = ilink(fit + 2*se.fit),
           period = filter(glm_dat)$period,
           oplottype = filter(glm_dat)$oplottype)


  glm_est <- glm_est %>%
    dplyr::right_join(dplyr::select(glm_dat, oera, oplottype, period, censusdate, pb_prop_ma))

  return(glm_est)

}
