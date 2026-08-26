#' Generate a standard HTML report for a4a results
#' 
#' @param stk An FLStock object
#' @param idx An FLIndices object
#' @param fit An a4aFit object
#' @param comp_stk Optional FLStock object to compare against. Defaults to NA.
#' @param output_dir Directory to save the HTML report (defaults to current working directory)
#' @param file_name Name of the output HTML file
#' @return The file path to the generated HTML report
#' @importFrom utils browseURL
#' @export
renderReporta4a <- function(stk, idx, fit, comp_stk = NA, output_dir = getwd(), file_name = "a4a_report.html") {
  
  if (missing(stk) || missing(idx) || missing(fit)) {
    stop("Arguments 'stk', 'idx', and 'fit' must all be provided.")
  }
  
  template_path <- system.file("rmarkdown", "report.Rmd", package = "wmsc")
  
  if (template_path == "") {
    stop("Could not find report.Rmd. Ensure the package is installed correctly.")
  }
  
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  out_file <- file.path(output_dir, file_name)
  
  message(sprintf("Building HTML report in %s...", out_file))
  
  rmarkdown::render(
    input = template_path,
    output_file = file_name,
    output_dir = output_dir,
    params = list(
      stk = stk,
      idx = idx,
      fit = fit,
      comp_stk = comp_stk
    ),
    envir = new.env(parent = globalenv()), 
    quiet = TRUE
  )
  
  message("Report successfully created!")
  
  if (interactive()) {
    utils::browseURL(out_file)
  }
  
  return(invisible(out_file))
}
