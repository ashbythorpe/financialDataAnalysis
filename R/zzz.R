
.onLoad <- function(libname, pkgname) {
  score_column <<- memoise::memoise(score_column)

  predict_iteration_daily <<- memoise::memoise(predict_iteration_daily)

  predict_iteration_monthly <<- memoise::memoise(predict_iteration_monthly)
}
