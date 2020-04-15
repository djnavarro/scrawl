scrawl_new <- function(options) {

  set.seed(options$seed)

  # To start drawing our scrawl we need to specify the "state" of the picture
  # after the first "step". Each of the paths starts at a random location on
  # the canvas (varibles: x and y), and has a hidden z co-ordinate that is
  # initially set to 0 for every path.
  state <- tibble::tibble(
    x = stats::runif(options$n_paths, min = 0, max = 2), # uniform between 0 and 2
    y = stats::runif(options$n_paths, min = 0, max = 2), # uniform between 0 and 2
    z = 0
  )

  # It is good practice to assign each path a specific identifier number (path_id)
  # and similarly we should assign each step its own id (this is the first step so
  # step_id = 1 for all paths)
  state <- dplyr::mutate(
    state,
    path_id = 1:options$n_paths,
    step_id = 1
  )

  # return the initial state to the user
  return(state)
}



scrawl_modify <- function(state, options) {

  # Use the curl_noise function from the ambient package to select
  # directions/distances to "step" each of the brushes in 3D space
  step <- ambient::curl_noise(
    generator = ambient::gen_simplex,
    x = state$x,
    y = state$y,
    z = state$z,
    seed = c(1, 1, 1) * options$seed
  )

  # Use the "step" data to mutate/modify the current state
  state <- dplyr::mutate(
      state,
      x = x + step$x * options$sz_step / 10000, # step along x
      y = y + step$y * options$sz_step / 10000, # step along y
      z = z + step$z * options$sz_slip / 10000, # step along z
      step_id = step_id + 1                     # increment the step number!
    )

  # Return the modified state to the user
  return(state)
}
