#' Get best fitting FPM
#'
#' Search for the model with the lowest AIC and BIC given the output of a
#' `fpm_test_dfs` call.
#'
#' @param x
#'    A `data.frame` with AIC and BIC criteria for different FPMs obtained from
#'    `fpm_test_dfs` or a list thereof.
#'
#' @return
#'    A `data.frame` where the first row corresponds to the model with the
#'    lowest AIC and the second row to the model with the lowest BIC.
#'
#' @import rstpm2
#'
#' @export fpm_get_best_fit
#'
#' @examples
#' require(rstpm2)
#' require(survival)
#'
#' data(brcancer)
#'
#' test_model <- fpm_test_dfs(survival::Surv(rectime, censrec) ~ hormon + x1,
#'                  dfs_bh  = 1:5,
#'                  dfs_tvc = list(hormon = 1:3,
#'                                 x1     = 1:5),
#'                  same_dfs_tvc = TRUE,
#'                  data = rstpm2::brcancer)
#'
#' fpm_get_best_fit(test_model)

fpm_get_best_fit <- function(x){

  stopifnot(is.list(x))

  if(is.data.frame(x)){

    min_aic_bic(x)

  } else {

    lapply(x, min_aic_bic)

  }

}

# Function for finding the lowest BIC and AIC
min_aic_bic <- function(x){

  rbind(x[x[["aic"]] == min(x[["aic"]]), ],
        x[x[["bic"]] == min(x[["bic"]]), ])

}
