#' Plot the spread of a set of scores
#' 
#' Use a boxplot and jitter plot to display the spread and arrangement of a set
#' of scores. Can often be used to view non-useful scores, where the majority
#' of the scores are the same.
#' 
#' @param scores A data frame containing a set of numerical scores. If your data
#'   frame has been scored using [apply_scores()], use [get_scores()] to extract
#'   the actual scores.
#'   
#' @details
#' Scores must be between 0 and 1.
#' 
#' @returns 
#' A [ggplot2::ggplot()] object.
#' 
#' @seealso [score_performance()]
#' 
#' @examples 
#' data <- tibble::tibble(
#'   x = 1:10
#' )
#' 
#' scores <- create_score(
#'   scores_init, score_type = "Linear", colname = "x", score_name = "Default",
#'   weight = 1, lb = 1, ub = 6, exponential = FALSE
#' )
#' scores <- create_score(
#'   scores, score_type = "Peak", colname = "x", score_name = "Peak score",
#'   weight = 2, lb = 2, ub = 8, centre = 5, inverse = FALSE,
#'   exponential = FALSE
#' )
#' 
#' scored <- apply_scores(data, scores)
#' score_distributions(get_scores(scored, scores, final_score = FALSE))
#' 
#' @export
score_distributions <- function(scores){
  if(is.null(scores)){
    return(NULL)
  }
  
  tidyr::pivot_longer(scores, tidyselect::everything()) %>%
    ggplot2::ggplot(ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    ggplot2::facet_grid(.~name) +
    ggplot2::scale_x_discrete(breaks = NULL, position = "top") +
    ggplot2::labs(x = "Score name", y = "Score") +
    ggplot2::ylim(0, 1) +
    ggthemes::theme_clean()
}

#' Compare a set of scores to a column
#' 
#' Plot a line graph of a set of scores against a single column in a data frame.
#' Can be used to evaluate whether a score is useful in determining some other
#' factor.
#' 
#' @param df A data frame to be plotted.
#' @param colname The name of the column to plot.
#' @param scores A data frame containing a set of numerical scores. If your data
#'   frame has been scored using [apply_scores()], use [get_scores()] to extract
#'   the actual scores.
#'
#' @seealso [score_distributions()]
#' 
#' @examples 
#' data <- tibble::tibble(
#'   x = 1:10
#' )
#' 
#' scores <- create_score(
#'   scores_init, score_type = "Linear", colname = "x", score_name = "Default",
#'   weight = 1, lb = 1, ub = 6, exponential = FALSE
#' )
#' scores <- create_score(
#'   scores, score_type = "Peak", colname = "x", score_name = "Peak score",
#'   weight = 2, lb = 2, ub = 8, centre = 5, inverse = FALSE,
#'   exponential = FALSE
#' )
#' 
#' scored <- apply_scores(data, scores)
#' score_performance(
#'   scored, 
#'   colname = "x",
#'   scores = get_scores(scored, scores, final_score = FALSE)
#' )
#' 
#' @export
score_performance <- function(df, colname, scores){
  if(is.null(colname) || !colname %in% colnames(df) || 
     colname %in% colnames(scores) || all(is.na(df[[colname]]))){
    return(NULL)
  }
  
  scores %>%
    dplyr::bind_cols(!!colname := df[[colname]]) %>%
    tidyr::pivot_longer(-tidyselect::any_of(colname)) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, y = .data[[colname]], 
                                 color = name)) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 1) +
    ggthemes::theme_clean()
}

#' See the measure of correlation between each column in a data frame
#' 
#' Create a heatmap of the correlation matrix of a data frame, created using
#' [cor()].
#' 
#' @param data The data to plot
#' @param show_text Whether to show the correlation number over each panel in
#'   the heatmap.
#' 
#' @returns A [ggplot2::ggplot()] object.
#' 
#' @examples 
#' correlation_plot(mtcars)
#' correlation_plot(mtcars, show_text = TRUE)
#' 
#' @export
correlation_plot <- function(data, show_text = FALSE) {
  if(is.null(data) || is.null(show_text)) {
    return(NULL)
  }
  
  x <- cor(data[,purrr::map_lgl(data, is.numeric)], use = "complete.obs") %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-rowname)
  
  if(show_text) {
    ggplot2::ggplot(x, ggplot2::aes(x = rowname, y = name, fill = value)) +
      ggplot2::geom_bin_2d() +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), size = 10/20 * 4) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
      ggplot2::scale_fill_viridis_c(guide = "none") +
      ggplot2::labs(x = NULL, y = NULL)
  } else {
    ggplot2::ggplot(x, ggplot2::aes(x = rowname, y = name, fill = value)) +
      ggplot2::geom_bin_2d() +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = NULL, y = NULL)
  }
}
