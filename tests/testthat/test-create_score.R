test_that("validate_score_type works", {
  validate_score_type(NULL) %>%
    expect_null()
  validate_score_type("a") %>%
    expect_null()
  validate_score_type("Linear") %>%
    expect_equal("Linear")
  validate_score_type("Custom coordinates") %>%
    expect_equal("Custom coordinates")
})

test_that("validate_universal_score works", {
  validate_universal_score(colname = "", score_name = "", weight = 1) %>%
    expect_null()
  validate_universal_score(colname = NULL, score_name = "", weight = 1) %>%
    expect_null()
  validate_universal_score(colname = "x", score_name = NULL, weight = 1) %>%
    expect_null()
  validate_universal_score(colname = "x", score_name = "", weight = NULL) %>%
    expect_null()
  validate_universal_score(colname = "x", score_name = "aaa", weight = 1) %>%
    expect_equal(tibble::tibble(colname = "x", score_name = "aaa", weight = 1))
  validate_universal_score(colname = "x", score_name = "", weight = 1) %>%
    expect_equal(tibble::tibble(colname = "x", score_name = NA_character_, weight = 1))
  validate_universal_score(colname = "x", score_name = "Default", weight = 1) %>%
    expect_equal(tibble::tibble(colname = "x", score_name = NA_character_, weight = 1))
})

test_that("validate_exponential_transformation", {
  validate_exponential_transformation(
    exponential = NULL, logarithmic = F, magnitude = 1
  ) %>%
    expect_equal(NULL)
  validate_exponential_transformation(
    exponential = F, logarithmic = F, magnitude = 1
  ) %>%
    expect_equal(tibble::tibble(exponential = F))
  validate_exponential_transformation(
    exponential = F, logarithmic = NULL, magnitude = 1
  ) %>%
    expect_equal(tibble::tibble(exponential = F))
  validate_exponential_transformation(
    exponential = T, logarithmic = F, magnitude = NULL
  ) %>%
    expect_equal(NULL)
  validate_exponential_transformation(
    exponential = T, logarithmic = F, magnitude = 1
  ) %>%
    expect_equal(tibble::tibble(exponential = T, logarithmic = F, magnitude = 1))
})

test_that("validate_linear_score", {
  validate_linear_score(lb = NULL, ub = 1) %>%
    expect_equal(NULL)
  validate_linear_score(lb = 1, ub = NULL) %>%
    expect_equal(NULL)
  validate_linear_score(lb = 1, ub = 1) %>%
    expect_equal(tibble::tibble(lb = 1, ub = 1))
})

test_that("validate_peak_score works", {
  validate_peak_score(NULL, ub = 1, centre = 1, inverse = TRUE) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = NULL, centre = 1, inverse = TRUE) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = 1, centre = NULL, inverse = TRUE) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = 1, centre = 1, inverse = NULL) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = 2, centre = 3, inverse = TRUE) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = 1, centre = 3, inverse = TRUE) %>%
    expect_equal(NULL)
  validate_peak_score(1, ub = 3, centre = 2, inverse = T) %>%
    expect_equal(tibble::tibble(lb = 1, ub = 3, centre = 2, inverse = T))
  validate_peak_score(3, ub = 1, centre = 2, inverse = T) %>%
    expect_equal(tibble::tibble(lb = 1, ub = 3, centre = 2, inverse = T))
})

test_that("validate_custom_score works", {
  validate_custom_score(NULL) %>%
    expect_equal(NULL)
  validate_custom_score(tibble::tibble(x = 1, z = 1)) %>%
    expect_equal(NULL)
  validate_custom_score(tibble::tibble(x = c(NA, 1), y = c(2, NA))) %>%
    expect_equal(NULL)
  validate_custom_score(tibble::tibble(x = c(3, 1, 1), y = c(1, 2, 2))) %>%
    expect_equal(tibble::tibble(custom_args = tibble::tibble(x = c(1, 3), y = c(2, 1))))
  validate_custom_score(tibble::tibble(x = c(1, 2), y = c(2, 1))) %>%
    expect_equal(tibble::tibble(custom_args = tibble::tibble(x = c(1, 2), y = c(2, 1))))
  validate_custom_score(tibble::tibble(x = c(1, NA), y = c(2,3))) %>%
    expect_null()
})

test_that("bind_validated_columns works", {
  bind_validated_columns(score_type = "Linear",
                         NULL, tibble::tibble(x = 1, y = 2)) %>%
    expect_equal(NULL)
  bind_validated_columns(score_type = "Linear",
                         tibble::tibble(x = 1, y = 2), NULL) %>%
    expect_equal(NULL)
  bind_validated_columns(score_type = "Linear",
                         tibble::tibble(x = 1, y = 2),
                         tibble::tibble(z = 3)) %>%
    expect_equal(tibble::tibble(score_type = "Linear", x = 1, y = 2, z = 3))
  bind_validated_columns(score_type = NULL,
                         tibble::tibble(x = 1, y = 2),
                         tibble::tibble(z = 3)) %>%
    expect_equal(tibble::tibble(x = 1, y = 2, z = 3))
})

test_that("validate_score works", {
  validate_score("aaa", colname = "x", score_name = "aaa", weight = 1, lb = NULL,
                 ub = NULL, centre = NULL, inverse = NULL, exponential = NULL,
                 logarithmic = NULL, magnitude = NULL, custom_args = NULL) %>%
    expect_equal(NULL)
  validate_score("Linear", colname = "", score_name = "aaa", weight = 1,
                 lb = NULL, ub = NULL, centre = NULL, inverse = NULL,
                 exponential = NULL, logarithmic = NULL, magnitude = NULL,
                 custom_args = NULL) %>%
    expect_equal(NULL)
  validate_score("Linear", colname = "x", score_name = "aaa", weight = 1, lb = NULL,
                 ub = NULL, centre = NULL, inverse = NULL, exponential = TRUE,
                 logarithmic = "a", magnitude = 1, custom_args = NULL) %>%
    expect_equal(NULL)
  validate_score("Linear", colname = "x", score_name = "aaa", weight = 1, lb = 4,
                 ub = "a", centre = NULL, inverse = NULL, exponential = FALSE,
                 logarithmic = NULL, magnitude = NULL, custom_args = NULL) %>%
    expect_equal(NULL)
  validate_score("Peak", colname = "x", score_name = "aaa", weight = 1, lb = 5,
                 ub = 4, centre = 6, inverse = TRUE, exponential = FALSE,
                 logarithmic = NULL, magnitude = NULL, custom_args = NULL) %>%
    expect_equal(NULL)
  validate_score("Custom coordinates", colname = "x", score_name = "aaa",
                 weight = 1, lb = NULL, ub = NULL, centre = NULL, inverse = NULL,
                 exponential = F, logarithmic = NULL, magnitude = NULL,
                 custom_args = tibble::tibble(x = 1, z = 2)) %>%
    expect_equal(NULL)
  validate_score("Linear", colname = "x", score_name = "aaa", weight = 1, lb = 1,
                 ub = 2, centre = NULL, inverse = NULL, exponential = FALSE,
                 logarithmic = NULL, magnitude = NULL, custom_args = NULL) %>%
    expect_equal(tibble::tibble_row(score_type = "Linear", colname = "x",
                            score_name = "aaa", weight = 1,
                            lb = 1, ub = 2, exponential = F))
  validate_score("Linear", colname = "x", score_name = "aaa", weight = 1, lb = 1,
                 ub = 2, centre = 1.5, inverse = F, exponential = FALSE,
                 logarithmic = F, magnitude = 4,
                 custom_args = tibble::tibble(x = 1, y = 2)) %>%
    expect_equal(tibble::tibble_row(score_type = "Linear", colname = "x",
                            score_name = "aaa", weight = 1,
                            lb = 1, ub = 2, exponential = F))
})

test_that("edit_row works", {
  edit_row(tibble::tibble(x = 1:5, y = 2:6, z = 3:7),
           row = tibble::tibble_row(x = 4, y = 3), editing = 3) %>%
    expect_equal(tibble::tibble(x = c(1,2,4,4,5), y = c(2,3,3,5,6), z = c(3,4,NA,6,7)))
  edit_row(tibble::tibble(x = 1:5, y = 2:6, z = 3:7),
           row = tibble::tibble_row(x = 4, y = 3), editing = 6) %>%
    expect_equal(tibble::tibble(x = 1:5, y = 2:6, z = 3:7))
})

test_that("get_score_function works", {
  get_score_function(NA) %>%
    is.function() %>%
    expect_true()
  get_score_function(3) %>%
    is.function() %>%
    expect_true()
})

test_that("replace_score_names works", {
  replace_score_names(tibble::tibble(colname = c("a", "b", "c"), score_name = c("a", NA, NA))) %>%
    expect_equal(tibble::tibble(colname = c("a", "b", "c"),
                                score_name = c("a", "Score 2: b", "Score 3: c")))
  replace_score_names(tibble::tibble(colname = "1", score_name = "a")) %>%
    expect_equal(tibble::tibble(colname = "1", score_name = "a"))
})


test_that("create_score works", {
  scores_with_scores <-
    scores %>%
    tibble::add_row(score_type = "Linear", colname = "x",
            score_name = "a", weight = 1, lb = 1, ub = 2,
            exponential = F) %>%
    tibble::add_row(score_type = "Linear", colname = "x",
            score_name = "a", weight = 1, lb = 1, ub = 3,
            exponential = F)
  create_score(scores_with_scores, editing = NA, score_type = "aaaa") %>%
    expect_equal(scores_with_scores)
  create_score(scores_with_scores, editing = NA, score_type = "Linear", colname = "x",
               score_name = "a", weight = 2, lb = 1, ub = 2,
               exponential = FALSE) %>%
    expect_equal(tibble::add_row(scores_with_scores, score_type = "Linear", colname = "x",
                                 score_name = "a", weight = 2, lb = 1, ub = 2,
                                 exponential = F))
  create_score(scores_with_scores, editing = 2, score_type = "Peak", colname = "x",
               score_name = "a", weight = 2, lb = 1, ub = 3, centre = 2,
               inverse = FALSE, exponential = FALSE) %>%
    expect_equal(scores %>%
                   tibble::add_row(score_type = "Linear", colname = "x",
                                   score_name = "a", weight = 1, lb = 1, ub = 2,
                                   exponential = F) %>%
                   tibble::add_row(score_type = "Peak", colname = "x",
                                   score_name = "a", weight = 2, lb = 1, ub = 3,
                                   centre = 2, inverse = F,
                                   exponential = F))
  #editing > nrow(scores)
  create_score(scores_with_scores, editing = 3, score_type = "Linear", colname = "x",
               score_name = "a", weight = 2, lb = 1, ub = 2, exponential = FALSE) %>%
    expect_equal(scores_with_scores)
  create_score(scores_with_scores, editing = NA, score_type = "Linear", colname = "x",
               score_name = "Default", weight = 2, lb = 1, ub = 2,
               exponential = FALSE) %>%
    expect_equal(tibble::add_row(scores_with_scores, score_type = "Linear", colname = "x",
                                 score_name = "Score 3: x", weight = 2, lb = 1, ub = 2,
                                 exponential = F))
  #check that replace_score_names is not run on empty data.
  create_score(scores, editing = NA) %>%
    expect_equal(scores)
})
