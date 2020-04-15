#' Construct the data used for a scrawl plot
#'
#' @param seed Integer valued seed to pass to set.seed()
#' @param n_paths Number of paths to trace
#' @param n_steps Number of steps along each path
#' @param sz_step Size of each step in the x and y dimensions
#' @param sz_slip Size of each step in the z dimension
#'
#' @return A tibble
#' @export
scrawl_build <- function(seed = 1, n_paths = 1000, n_steps = 50,
                         sz_step = 50, sz_slip = 5) {

  options <- list(
    seed = seed,        # RNG seed
    n_paths = n_paths,  # number of paths
    n_steps = n_steps,  # number of steps
    sz_step = sz_step,  # step size
    sz_slip = sz_step   # slip size
  )

  scrawl <- scrawl_new(options = options)
  state <- scrawl

  # yes this loop could be vectorised: I've left it as is
  # to better match the original art tutorial
  for(step in 1:options$n_steps) {
    state <- scrawl_modify(state = state, options = options)
    scrawl <- dplyr::bind_rows(scrawl, state)
  }

  return(scrawl)
}
