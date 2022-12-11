hostess_obj <- R6::R6Class(
  "hostess_obj",
  public = list(
    initialize = function(h1, h2) {
      private$h1 <- h1
      private$h2 <- h2
    },
    set_active = function(x) {
      private$active <- x
    },
    set = function(x) {
      if (private$active == 1) {
        private$h1$set(x)
      } else {
        private$h2$set(x)
      }
    },
    inc = function(x) {
      if (private$active == 1) {
        private$h1$inc(x)
      } else {
        private$h2$inc(x)
      }
    },
    start = function() {
      if (private$active == 1) {
        private$h1$start()
      } else {
        private$h2$start()
      }
    },
    print = function() {
      cat(paste("Hostess", private$active, "is currently active:\n"))
      if (private$active == 1) {
        print(private$h1)
      } else {
        print(private$h2)
      }
    }
  ),
  private = list(
    active = 1,
    h1 = NULL,
    h2 = NULL
  )
)
