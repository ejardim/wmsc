#' Easter egg animate a stickman walking in ASCII
#'
#' Displays an ASCII stickman walking animation in the R console.
#'
#' @param steps Number of animation steps (default: 20)
#' @param width Width of the "stage" in characters (default: 40)
#' @param delay Delay between frames in seconds (default: 0.15)
#' @return Invisible NULL. Called for side effects (animation).
#' @export
#' @examples
#' walk_stickman()
#' walk_stickman(steps = 30, width = 60, delay = 0.1)
easteregg <- function(steps = 200, width = 40, delay = 0.15) {
  # 4 walking frames: legs alternate
  frames <- list(
    c("  O  ", " /|\\ ", " / \\ "),  # both feet out
    c("  O  ", " /|\\ ", " /|  "),  # right foot up
    c("  O  ", " /|\\ ", "  |\\ "),  # left foot up
    c("  O  ", " /|\\ ", " / | ")   # stride transition
  )

  n_frames <- length(frames)
  frame_height <- length(frames[[1]])
  stage_width  <- max(width, 20L)

  for (step in seq_len(steps)) {
    frame <- frames[[((step - 1L) %% n_frames) + 1L]]

    # Compute horizontal position: ping-pong across stage
    pos <- ((step - 1L) %% (2L * (stage_width - 5L)))
    if (pos >= stage_width - 5L) pos <- 2L * (stage_width - 5L) - pos
    pad <- pos  # number of leading spaces

    # Build each line of the frame with horizontal offset
    lines <- vapply(frame, function(row) {
      paste0(strrep(" ", pad), row)
    }, character(1L))

    # Clear screen and print
    cat("\033[2J\033[H")  # ANSI: clear screen + move cursor home
    cat("Walking stickman  [step", step, "of", steps, "]\n")
    cat(strrep("-", stage_width), "\n", sep = "")
    cat(paste(lines, collapse = "\n"), "\n")
    cat(strrep("-", stage_width), "\n", sep = "")

    Sys.sleep(delay)
  }

  cat("\nDone!\n")
  invisible(NULL)
}
