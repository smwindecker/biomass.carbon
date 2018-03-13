#' Produce pca plot and loadings table
#'
#' @param dataframe dataframe of traits
#' @param params parameter vector of starting values for h, s, p, and w
#' @param lb lower bound
#' @param ub upper bound
#' @return saved pca plot and loadings table
#' @importFrom minpack.lm nlsLM
#'
#' @export


# function to do the nls fit with the correct starting values
fs_model <- function (dataframe, params, lb, ub) {

  nlsLM(deriv ~ fs_function(temp_C, h, s, p, w),
        start = list(h = params[1], 
                     s = params[2], 
                     p = params[3],
                     w = params[4]),
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, 
                                 maxfev = 1e6),
        lower = lb,
        upper = ub)

}
