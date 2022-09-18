is_continuous <- function(x){
  is.numeric(x)
}

is_categorical <- function(x){
  !is_continuous(x)
}
