#' Graphical Approach to Multiple Test Procedures
#'
#' Implementation of Bretz's graphical models for sequentially rejective
#' test procedures. Based on <doi: 10.1002/sim.3495>.
#'
#' @param p A vector of length *m* including m P-values associated with
#'        Hypotheses *j = 1,...,m* to be tested.
#' @param alpha Either a vector of length *m* with the alpha levels against
#'        which hypothesis j should be tested. If a hypothesis should not be
#'        tested initially its alpha value should be *0*. If only one value
#'        is supplied for alpha it is assmued that the alpha value is splitt
#'        equally across the hypotheses.
#' @param G The transition matrix associated with the graphical model assumed
#'        for the sequential testing procedure to be used.
#' @returns A data.frame with testing results and adjusted P-values for each
#'          hypotheses. Please not that results are only provided up until the
#'          first hypothesis that failes.
#' @examples
#'
#' # Example from Figure 3 in the original article
#' p_adjust_graph(p     = c(0.02, 0.055, 0.012),
#'                alpha = 0.05,
#'                G     = matrix(c(0  , 1/2, 1/2,
#'                                 1/2, 0  , 1/2,
#'                                 1/2, 1/2, 0  ),
#'                               nrow = 3,
#'                               byrow = TRUE))
#'
#' # Example from Figure 5 (top left) in the original article
#' p_adjust_graph(p     = c(0.012, 0.02, 0.055),
#'                alpha = 0.05,
#'                G = matrix(c(0  , 1/2, 1/2,
#'                             0  , 0  , 1  ,
#'                             0  , 0  , 0  ),
#'                           nrow = 3,
#'                           byrow = TRUE))
#'
#' # Example of a strict gatekeeping strategy
#' p_adjust_graph(p     = c(0.012, 0.055),
#'                alpha = c(0.05, 0),
#'                G = matrix(c(0  , 1,
#'                             0  , 0),
#'                           nrow = 2,
#'                           byrow = TRUE))
#' @export

p_adjust_graph <- function(p, alpha, G){

  # Initiation
  out <- data.frame()
  failure <- FALSE
  # Order P-values in testing order
  I   <- order(p/(alpha/length(p)))

  # Split alpha equally if not otherwise specified
  if(length(alpha) != length(p)){
    alpha   <- rep(alpha / length(p), length(p))
  }

  # Define inital weights for calculating adjusted P-values
  w <- alpha / sum(alpha)

  # Initiate p-max
  p_max <- 0

  # Iterate over graphs until no P-value is untested
  while(length(I) >= 1 & !failure) {

    # Take the first/next P-value and remove it from the set
    j <- I[1]
    I <- I[-1]

    p_adj <- p_max <- max(p[j] / w[j], p_max)

    # Test H_j
    if(p[j] <= alpha[j]){

      # Output sucess
      cat("\nH", j,
          " passed at " , format(round(alpha[j], digits = 3), nsmall = 3),
          " (p-adjust: ", format(round(p_adj,    digits = 3), nsmall = 3), ")",
          sep = "")

      out <- rbind(out, data.frame(H      = j,
                                   alpha  = alpha[j],
                                   p      = p[j],
                                   passed = TRUE,
                                   p_adj  = p_adj))

      # Update Graph
      alpha[I]  <- alpha[I] + (alpha[j] * G[j,I])
      alpha[-I] <- 0

      w[I]  <- w[I] + (w[j] * G[j,I])
      w[-I] <- 0

      for (l in seq_along(p)){
        for (k in seq_along(p)){
          if (k %in% I | l != k){
            G[l,k] <- (G[l,k] + G[l,j] * G[j,k]) / (1 - G[l,j] * G[j,l])
          } else {
            G[l,k] <- 0
          }
        }
      }

    } else {

      # Stop iteration
      failure <- TRUE

      # Output failure
      cat("\nH", j,
          " failed at " , format(round(alpha[j], digits = 3), nsmall = 3),
          " (p-adjust: ", format(round(p_adj   , digits = 3), nsmall = 3), ")",
          sep = "")

      out <- rbind(out, data.frame(H      = j,
                                   alpha  = alpha[j],
                                   p      = p[j],
                                   passed = FALSE,
                                   p_adj  = p_adj))

      # Add other untested hypothesis to output
      if(length(I) >= 1){
        cat("\nHypotheses", I, "were not evaluated")
        out <- rbind(out, data.frame(H      = I,
                                     alpha  = NA,
                                     p      = p[I],
                                     passed = FALSE,
                                     p_adj  = NA))
      }
    }

  }

  # Print data.frame
  cat("\n\nSUMMARY TABLE\n")
  out

}
