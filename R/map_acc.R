#' Apply a function to each element of an `acc` vector
#' 
#' @description 
#' This function provides a general framework to apply an arbitrary function
#' to each element of an `acc` vector while providing access to each element's
#' burst, frequency, and start time metadata.
#' 
#' Note that some [common operations][explore-functions] have already been implemented as stand-alone
#' functions.
#' 
#' @details
#' This function behaves similarly to the [purrr::map()] family of functions.
#' However, `map_acc()` only matches arguments by name, not position. Thus,
#' the input to `.f` must use the specified terminology (`.br`, `.fq`, 
#' and/or `.st`) to access specific data from each `acc` element.
#' For a given `acc` vector `x`:
#' 
#' - `.br` accesses each element of the list returned by [bursts()]
#' - `.fq` accesses each element of the vector returned by [freqs()]
#' - `.st` accesses each element of the vector returned by [starts()]
#' 
#' @param acc An `acc` vector
#' @param .f A function to be applied to each element of `acc`. This can be
#'   supplied in one of the following ways:
#'   - A named function
#'   - An anonymous function (e.g., `function(.br) nrow(.br) / .fq`)
#'   - A formula (e.g., `~ nrow(.br) / .fq`)
#'   
#'   In all cases, use `.br` to refer to the burst matrix of each element, `.fq`
#'   to refer to the frequency of each element, and `.st` to refer to the
#'   start time of each element. See examples.
#' @param simplify Logical. If `TRUE`, attempts to simplify the output to a
#'   vector. Otherwise, the output will be a list. If the output cannot be 
#'   simplified while maintaining a one-to-one correspondence with the input,
#'   an error will be thrown.
#' @param .progress Whether to show a progress bar. Use `TRUE` to turn on a 
#'   basic progress bar, use a string to give it a name.
#'
#' @export
#' 
#' @examples
#' a <- acc_example()
#' 
#' # Use `.br` to access the burst matrix for each element:
#' n_samp <- map_acc(a, function(.br) nrow(.br))
#' 
#' n_samp
#' 
#' # Use `.fq` to access the frequency value for each element:
#' burst_len <- map_acc(a, function(.br, .fq) nrow(.br) / .fq)
#' 
#' burst_len
#' 
#' # Use `.st` to access the start time for each element:
#' burst_end <- map_acc(
#'   a, 
#'   function(.br, .fq, .st) as.numeric(nrow(.br) / .fq) + .st
#' )
#' 
#' burst_end
#' 
#' # You can also provide a separately defined function
#' get_burst_end <- function(.br, .fq, .st, offset = 2) {
#'   as.numeric(nrow(.br) / .fq) + .st + offset
#' }
#' 
#' map_acc(a, get_burst_end)
#' 
#' map_acc(a, function(.br, .fq, .st) get_burst_end(.br, .fq, .st, offset = 5))
#' 
#' # Use simplify to reduce to a vector format:
#' map_acc(a, get_burst_end, simplify = TRUE)
#' 
#' # Note that this will fail if the result cannot be simplified to the same
#' # length as the input `acc` vector
#' try(
#'   map_acc(a, function(.br) .br, simplify = TRUE)
#' )
map_acc <- function(acc, .f, simplify = FALSE, .progress = FALSE) {
  assertthat::assert_that(inherits(acc, "acc"))
  
  f <- as_acc_mapper(.f)
  
  x <- purrr::pmap(
    list(
      .br = bursts(acc),
      .fq = freqs(acc),
      .st = starts(acc)
    ),
    function(.br, .fq, .st) {
      f(.br = .br, .fq = .fq, .st = .st)
    },
    .progress = .progress
  )
  
  if (simplify) {
    x <- purrr::list_simplify(x)
  }
  
  x
}

as_acc_mapper <- function(.f) {
  if (rlang::is_formula(.f)) {
    # Build function with custom arg names to refer to acc fields
    f <- rlang::new_function(
      args = rlang::pairlist2(.br = , .fq = , .st = ),
      body = rlang::f_rhs(.f),
      env  = rlang::f_env(.f)
    )
  } else if (is.function(.f)) {
    # If a function already, convert to format where not all named args
    # are required. `force()` ensures correct enclosing env for f
    force(.f)
    
    f <- function(.br, .fq, .st) {
      available <- list(.br = .br, .fq = .fq, .st = .st)
      do.call(.f, available[names(available) %in% names(formals(.f))])
    }
  } else { 
    rlang::abort("`.f` must be a function or a formula.")
  }
  
  f  
}
