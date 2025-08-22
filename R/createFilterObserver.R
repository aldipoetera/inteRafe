#' Create a Cross-Filtering Shiny Observer for ggiraph Plots
#'
#' This function sets up a Shiny observer that listens for selection events
#' from a ggiraph plot. When a selection is made, it filters a dataset to
#' find the corresponding unique identifiers and updates a reactiveVal by
#' finding the intersection with the currently selected identifiers.
#'
#' @param input The `input` object from the Shiny server function.
#' @param plot_id A character string: the `outputId` of the `girafeOutput`
#'   that this observer should listen to.
#' @param main_data The full, unfiltered dataframe to use for lookups.
#' @param id_col <[`data-masking`][rlang::args_data_masking]> The unquoted column name in `main_data` that
#'   contains the unique identifier for each row (e.g., car_name).
#' @param reactive_data The `reactiveVal` object that holds the state of the
#'   selected `id_col` values. This is the reactive that will be updated by
#'   the observer.
#' @param filter_column A character vector specifying the column(s) used for
#'   filtering in the plot.
#'   \itemize{
#'     \item \code{NULL} (default): Assumes the plot's `data_id` directly
#'       provides the values from `id_col` (e.g., a scatter plot).
#'     \item \code{character(1)}: A single column name. Used for simple bar
#'       charts where the selection is a category in this column.
#'     \item \code{character(n)}: Multiple column names. Used for complex
#'       plots (e.g., stacked bars) where a composite ID column must be
#'       looked up. The name of this composite ID column in `main_data` is
#'       assumed to be the `filter_column` names pasted together with an
#'       underscore (e.g., "cyl_gear").
#'   }
#'
#' @return This function does not return a value. It is called for its side
#'   effect of creating a Shiny observer.
#'
#' @importFrom shiny observeEvent
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Inside your Shiny server function:
#'
#' # 1. Define your reactive state
#' selected_cars <- reactiveVal(mtcars_data$car_name)
#'
#' # 2. Call the function to create the observer for a plot
#' createFilterObserver(
#'   input = input,
#'   plot_id = "gear_plot",
#'   main_data = mtcars_data,
#'   id_col = car_name,
#'   reactive_data = selected_cars,
#'   filter_column = "gear"
#' )
#' }
createFilterObserver <- function(input,
                                 plot_id,
                                 main_data,
                                 id_col,
                                 reactive_data,
                                 filter_column = NULL) {

  observe_input_id <- paste0(plot_id, "_selected")

  shiny::observeEvent(input[[observe_input_id]], {
    selection <- input[[observe_input_id]]
    if (length(selection) == 0) return()

    cars_to_intersect <- NULL

    if (is.null(filter_column)) {
      cars_to_intersect <- selection
    } else if (length(filter_column) == 1) {
      cars_to_intersect <- main_data %>%
        dplyr::filter(.data[[filter_column]] %in% selection) %>%
        dplyr::pull({{ id_col }})
    } else {
      id_col_name <- paste(filter_column, collapse = "_")

      cars_to_intersect <- main_data %>%
        dplyr::filter(.data[[id_col_name]] %in% selection) %>%
        dplyr::pull({{ id_col }})
    }

    current_selection <- reactive_data()
    new_selection <- intersect(current_selection, cars_to_intersect)
    reactive_data(new_selection)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
