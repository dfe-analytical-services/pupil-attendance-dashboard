#' GOV.UK styled CSV download button for govReactable tables
#'
#' @param table_id ID of the govReactable table.
#' @param filename Filename used for CSV download.
#'
#' @export
govReactableDownloadButton <- function(
  table_id,
  filename = "data.csv"
) {
  stopifnot(
    is.character(table_id),
    length(table_id) == 1,
    nzchar(table_id)
  )

  shiny::tags$div(
    style = "
      display:flex;
      gap:8px;
      margin-bottom:15px;
    ",
    shiny::tags$button(
      type = "button",
      class = "govuk-button govuk-button--secondary",
      onclick = sprintf(
        "
        Reactable.downloadDataCSV(
          '%s',
          '%s'
        );
        ",
        table_id,
        filename
      ),
      "Download CSV"
    )
  )
}
