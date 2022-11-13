parent_ns <- function(id){
  id %>%
    stringr::str_remove(paste0("\\Q", ns.sep, "\\E[:alpha:]+_[:digit:]+$")) %>%
    NS()
}

# https://github.com/rstudio/shiny/issues/1484#issuecomment-262812760
dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}

makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
