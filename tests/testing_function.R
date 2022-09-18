library(tidyverse)
library(rlang)
library(testthat)

test <- function(x, .f){
  with_n <- x %>%
    mutate(n = seq_len(nrow(.env$x)))
  pmap(with_n, test_function, fun = .f)
}

test_function <- function(expected_result, n, fun, ...){

  args_table <- tibble(map(list(...), list))
  contained_function <- quietly(safely(fun))
  function_result <- contained_function(...)
  actual_result <- function_result$result$result
  error <- function_result$result$error
  warnings <- function_result$warnings
  messages <- function_result$messages
  failed <- 0
  if(is_formula(expected_result)) expected_result <- rlang::as_function(expected_result)
  if(length(messages == 1)){
    print(glue("Messages for test {n}:"))
    map(messages, print)
  }
  if(length(warnings == 1)){
    print(glue("Warnings for test {n}:"))
    map(messages, print)
  }
  if(!is.null(error)){
    msg <- glue::glue("Test {n} returned an error: \n
                      {error}")
    expect(F, failure_message = msg)
  } else if(is.function(expected_result)){
    safe_function <- quietly(possibly(expected_result, NULL))
    return_value <- safe_function(actual_result)$result
    string_function <- expected_result %>%
      fn_body() %>%
      as.character() %>%
      {rlang::inject(paste0(!!!.))}
    msg <- glue::glue("Test {n}'s return value did not return `TRUE` for function `expected_result`.\n
                      Value: {actual_result}.
                      Function: {string_function}.
                      Return value: {return_value}.")
    expect(isTRUE(return_value), failure_message = msg)
  } else{
    expect_equal(actual_result, expected_result, label = glue::glue("Test {n}'s result"))
  }
  actual_result
}
