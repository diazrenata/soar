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
