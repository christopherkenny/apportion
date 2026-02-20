if (FALSE) {
  bench::mark(
    app_adams(size = 435, pop = state_2020$pop),
    app_dean(size = 435, pop = state_2020$pop),
    app_dhondt(size = 435, pop = state_2020$pop),
    app_hamilton_vinton(size = 435, pop = state_2020$pop),
    app_huntington_hill(size = 435, pop = state_2020$pop),
    app_webster(size = 435, pop = state_2020$pop),
    check = FALSE,
    min_iterations = 20,
    max_iterations = 200,
  )

  big = matrix(rep(state_2020$pop, 100), nrow=50)
  bench::mark(app_adams(435, big))
}
