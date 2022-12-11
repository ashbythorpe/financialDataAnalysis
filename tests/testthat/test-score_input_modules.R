test_that("score_type_server works", {
  testServer(score_type_server, args = list(
    reset = reactive(NULL), editing_row = reactive(NULL)
  ), {
    expect_equal(score_type(), NULL)

    session$setInputs(score_type = "a")

    expect_equal(score_type(), NULL)

    session$setInputs(score_type = "Linear")

    expect_equal(score_type(), "Linear")
  })
})

test_that("linear_score_server works", {
  testServer(linear_score_server, args = list(
    column = reactive(1:100), reset = reactive(NULL),
    editing_row = reactive(NULL)
  ), {
    expect_equal(linear_row(), NULL)

    session$setInputs(lb = 1)

    expect_equal(linear_row(), NULL)

    session$setInputs(ub = 2)

    expect_equal(linear_row(), tibble::tibble_row(
      lb = 1, ub = 2
    ))
  })
})

test_that("peak_score_server works", {
  testServer(peak_score_server, args = list(
    column = reactive(1:100), reset = reactive(NULL),
    editing_row = reactive(NULL)
  ), {
    expect_equal(peak_row(), NULL)

    session$setInputs(lb = 1)

    expect_equal(peak_row(), NULL)

    session$setInputs(ub = 3, centre = 2, inverse = FALSE)

    expect_equal(peak_row(), tibble::tibble_row(
      lb = 1, ub = 3, centre = 2, inverse = FALSE
    ))
  })
})

test_that("exponential_score_server works", {
  testServer(exponential_score_server, args = list(
    reset = reactive(NULL), editing_row = reactive(NULL)
  ), {
    expect_equal(exponential_row(), NULL)

    session$setInputs(exponential = TRUE)

    expect_equal(exponential_row(), NULL)

    session$setInputs(exponential = FALSE)

    expect_equal(exponential_row(), tibble::tibble_row(
      exponential = FALSE
    ))
  })
})
