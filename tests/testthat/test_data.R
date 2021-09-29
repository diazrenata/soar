test_that("correct numbers of plots per treatment", {

  trt <- get_rodent_data()

  trt_nplots <- trt %>%
    dplyr::select(plot_type, nplots) %>%
    dplyr::distinct()

  expect_true(nrow(trt_nplots) == 2)
  expect_true(all(trt_nplots$plot_type == c("CC", "EE")))
  expect_true(all(trt_nplots$nplots == c(4, 5)))

  })

test_that("plots have correct treatments", {

  # This table of treatments taken from https://github.com/emchristensen/PlotSwitch/blob/master/Data/data_functions.R line 76
  treatment = data.frame(before_after = c('CX','CE','EE','CC',
                                          'XC','EC','XC','CE',
                                          'CX','XX','CC','CX',
                                          'EC','CC','EE','XX',
                                          'CC','EC','EE','EE',
                                          'EE','CE','XX','XC'),plot=seq(1,24))

  full_trts <- list_plot_types() %>%
    dplyr::left_join(treatment)

  expect_true(all(full_trts$plot_type == full_trts$before_after))



}
)

test_that("treatl", {

  treatl <- get_treatment_means()
  expect_true(min(treatl$period) == 119)
  expect_true(max(treatl$period) == 494)

  plotl <- get_rodent_data(return_plot = T)

  manual_plotl_1 <- plotl %>%
    dplyr::filter(period == 300) %>%
    dplyr::group_by(plot_type) %>%
    dplyr::summarize(e = mean(total_e),
                     dipo_e = mean(dipo_e),
                     pb_e = mean(pb_e),
                     nplots = dplyr::n())

  treatl_300 <- dplyr::filter(treatl, period == 300)

  expect_true(all(treatl_300$total_e == manual_plotl_1$e))
  expect_true(all(treatl_300$dipo_e == manual_plotl_1$dipo_e))
  expect_true(all(treatl_300$pb_e == manual_plotl_1$pb_e))

})


test_that("get pb", {

  pb <- get_pb()

  treatl <- get_treatment_means()

  expect_true(pb$pb_prop[1] == 0)
  expect_true(all.equal(pb$pb_prop,
                      treatl$pb_e / treatl$total_e))

})


test_that("get e ratio", {

  er <- get_e_ratio()

  treatl <- get_treatment_means()

  tl_300 <- dplyr::filter(treatl, period == 300)

  er_300 <- dplyr::filter(er, period == 300)

  expect_true(er_300$total_e_rat == tl_300$total_e[2] / tl_300$total_e[1])
  expect_true(all(unique(treatl$period) == unique(er$period)))

  expect_true(nrow(treatl) == 2 * nrow(er))

  })
