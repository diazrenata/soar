#' Get new pps
#'
#' @return dataframe of number of new pps captured per plot per period
#' @export
#'
#' @importFrom portalr summarize_individual_rodents
#' @importFrom dplyr filter bind_rows group_by summarize ungroup
get_new_pps <- function() {

  individuals <- portalr::summarise_individual_rodents(type = "Granivores", unknowns = FALSE, time = "all", fillweight = FALSE, min_plots = 24, min_traps = 45)

  pps <- dplyr::filter(individuals, species == "PP", !is.na(tag), tag != "0") # removing tag = 0 removes 1 individual with a ltag but a 0 right tag. I don't have it in me to write the exception for that one at the moment. Will want to later.

  pp_tags <- unique(pps$tag)

  first_sightings <- list()

  for(i in 1:length(pp_tags)) {

    this_rat <- dplyr::filter(pps, tag == pp_tags[i])

    ltag_match <- dplyr::filter(pps, tag == pp_tags[i]) %>%
      dplyr::filter(ltag != "0")

    if(nrow(ltag_match) > 0) {

      nltags <- length(unique(ltag_match$ltag))

      if(nltags > 1) {
        break("Lots of ltags")
      }

      this_ltag <- ltag_match$ltag[1]
      this_rat_ltags <- dplyr::filter(pps, ltag == this_ltag)

      this_rat <- dplyr::bind_rows(this_rat, this_rat_ltags) %>%
        dplyr::distinct()
    }

    this_rat_first_sighting <- min(this_rat$period)

    this_rat <- dplyr::filter(this_rat, period == this_rat_first_sighting)

    first_sightings[[i]] <- this_rat
  }


  new_pps <- dplyr::bind_rows(first_sightings)

  new_pps_by_plot <- new_pps %>%
    dplyr::group_by(newmoonnumber, period, censusdate, treatment, plot) %>%
    dplyr::summarize(PP = dplyr::n()) %>%
    dplyr::ungroup()

  return(new_pps_by_plot)

}


#' Add new PPs
#'
#' @param plotdat plot level data
#'
#' @return returns new PPs per plot
#' @export
#'
#' @importFrom dplyr left_join group_by_all mutate ungroup rename group_by filter
add_new_pps <- function(plotdat) {

  new_pps_use <- new_pps %>%
    dplyr::filter((!period %in% c(406:412))) # remove a section where there were no tags

  plotdat <- dplyr::left_join(plotdat, new_pps_use) %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(PP = ifelse(is.na(PP), 0, PP)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(new_pp = PP) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(new_pp_ma = maopts(new_pp)) %>%
    dplyr::ungroup()

  return(plotdat)
}
