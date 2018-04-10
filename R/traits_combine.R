#' Merge trait datasets
#'
#' @param species species info dataframe
#' @param trt les trait data
#' @param cn carbon and nitrogen leco data
#' @param tga tga data
#' @return merged trait datasets
#' @importFrom dplyr full_join arrange
#'
#' @export

traits_combine <- function (species, trt, cn, tga) {
  
  t <- dplyr::full_join(trt, cn, by = 'species_code') %>%
    dplyr::full_join(., tga, by = 'species_code') %>%
    dplyr::arrange(., species) %>%
    .[, c(1,4:7,2,3,9,8,10:14)]
  
  t
  
}

