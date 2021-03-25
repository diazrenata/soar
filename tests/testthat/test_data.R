test_that("correct numbers of plots per treatment", {

  trt <- get_rodent_data()

  trt_nplots <- trt %>%
    dplyr::select(plot_type, nplots) %>%
    dplyr::distinct()

  expect_true(nrow(trt_nplots) == 4)
  expect_true(all(trt_nplots$plot_type == c("CC", "CE", "EC", "EE")))
  expect_true(all(trt_nplots$nplots == c(4, 3, 3, 5)))

  trt_preswitch <- get_rodent_data(use_pre_switch = T)

  trt_preswitch_nplots <- trt_preswitch %>%
    dplyr::select(plot_type, nplots) %>%
    dplyr::distinct()

  expect_true(nrow(trt_preswitch_nplots) == 2)
  expect_true(all(trt_preswitch_nplots$plot_type == c("C", "E")))
  expect_true(all(trt_preswitch_nplots$nplots == c(8,8)))

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


  ps_trts <- list_plot_types(use_pre_switch = T) %>%
    dplyr::left_join(treatment) %>%
    dplyr::mutate(first_trt = substr(before_after, 1, 1))

  expect_true(all(ps_trts$plot_type == ps_trts$first_trt))


}
)
