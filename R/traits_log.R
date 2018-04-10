#' Produce matrix of logged traits
#'
#' @param t_mean mean traits dataframe
#' @return matrix of logged traits
#' @importFrom magrittr set_rownames
#' @importFrom dplyr select
#'
#' @export

log_traits <- function (t_mean) {
 
  cov <- t_mean %>%
    magrittr::set_rownames(t_mean[, 'sp_abrev']) %>%
    dplyr::select(SLA, DMC, N, C, HC, CL, LG) 
  cov[] <- log(cov[])
  
  cov
}
