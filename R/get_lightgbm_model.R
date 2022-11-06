get_lightgbm_model <- function(x) {
  if(x == "daily") {
    wf <- readRDS(system.file(
      "data/daily_lightgbm_model.rds",
      package = "financialDataAnalysis"
    ))
    
    model <- lightgbm::readRDS.lgb.Booster(system.file(
      "data/daily_lightgbm_inner_model.rds",
      package = "financialDataAnalysis"
    ))
    
    wf$fit$fit$fit <- model
    
    wf
  } else {
    wf <- readRDS(system.file(
      "data/monthly_lightgbm_model.rds",
      package = "financialDataAnalysis"
    ))
    
    model <- lightgbm::readRDS.lgb.Booster(system.file(
      "data/monthly_lightgbm_inner_model.rds",
      package = "financialDataAnalysis"
    ))
    
    wf$fit$fit$fit <- model
    
    wf
  }
}
