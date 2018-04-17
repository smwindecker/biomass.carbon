# Analyse data

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

#' Select mean of traits
#'
#' @param all_traits dataframe of traits
#' @param output_file file path for output plot
#' @return mean traits only
#' @importFrom dplyr filter mutate
#' @importFrom utils write.table
#'
#' @export

traits_mean_only <- function(all_traits, output_file) {
  
  t_mean <- all_traits %>%
    dplyr::filter(wt_type == 'mean') %>%
    dplyr::mutate(HC = HC_1 + HC_2)
  
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  utils::write.table(t_mean, output_file)
  
  t_mean
}

#' Produce matrix of logged traits
#'
#' @param t_mean mean traits dataframe
#' @return matrix of logged traits
#' @importFrom magrittr set_rownames
#' @importFrom dplyr select
#'
#' @export

traits_log <- function (t_mean) {
  
  cov <- t_mean %>%
    magrittr::set_rownames(t_mean[, 'sp_abrev']) %>%
    dplyr::select(SLA, DMC, N, C, HC, CL, LG) 
  cov[] <- log(cov[])
  
  cov
}

#' Conduct PCA
#'
#' @param df logged traits matrix
#' @return pca output
#' @importFrom stats princomp na.omit
#'
#' @export

pca_data <- function(df) {
  
  # calculate loadings 
  prin <- stats::princomp((stats::na.omit(df)), cor = TRUE, scores = TRUE)
  
  prin
}

#' Read phylogenetic tree data
#'
#' @param nwk_file file path for tree
#' @importFrom ape read.tree
#' @return tree as phylogeny
#'
#' @export

phylo_readtree <- function (nwk_file) {
  
  # read tree file
  phylo <- ape::read.tree(nwk_file)
  
  # rewrite the tip labels so that those that aren't my species only show Genus
  phylo$tip.label <- c('Meuhlenbeckia', 'Rumex_crispus', 'Persicaria_decipiens', 
                       'Alternanthera', 'Lycopus', 'Myriophyllum', 'Crassula_helmsii', 
                       'Eucalyptus_camaldulensis', 'Melaleuca', 'Acacia_dealbata', 
                       'Eleocharis_acuta', 'Cyperus_eragrostis', 'Carex', 
                       'Gahnia', 'Baumea_articulata', 'Baumea_rubiginosa', 
                       'Juncus', 'Paspalum_distichum', 'Phragmites_australis', 
                       'Restio_tetraphyllus', 'Typha_domingensis', 'Cycnogeton_procerum', 
                       'Nymphaea_alba', 'Marsilea_drummondii', 'Sphagnum')
  
  phylo
  
}

#' Add functional trait data to phylogenetic tree data
#'
#' @param phylo phylogenetic tree object
#' @param t_mean mean traits dataframe
#' @return phylogenetic data with traits
#'
#' @export

phylo_traits <- function (phylo, t_mean) {
  
  tips <- as.data.frame(phylo$tip.label)
  colnames(tips)[1] <- 'tip_label'
  
  # list of traits
  trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  
  # create empty columns for each traits
  tips[, trait_list] <- NA
  
  # add genus
  t_mean$genus <- gsub(' .*$', '', t_mean$species)
  
  # overall HC
  t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  t_mean$species_genus <- stringr::str_replace_all(t_mean$species, ' ', '_')
  
  # input trait value into matrix from dataframe
  for (j in trait_list) {
    for (i in unique(tips$tip_label)) {
      if (i %in% t_mean$species_genus) {
        tips[i, j] <- t_mean[t_mean$species_genus == i, j]
      }
      if (i %in% t_mean$genus) {
        tips[i, j] <- mean(t_mean[t_mean$genus == i, j])
      }
    }
  }
  
  tips <- tips[is.na(tips$tip_label), - which(names(tips) %in% 'tip_label')]
  
  tips
  
}

#' Deconvolve a single TGA curve
#'
#' @param raw_file TGA raw data
#' @importFrom deconvolve fs_function
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom stats integrate
#' @importFrom grDevices png dev.off 
#' @importFrom graphics legend lines par plot 
#' @return saved deconvolved plot and weight
#'
#' @export

single_deconvolve <- function (raw_file) {
  
  # load and process TGA data
  tmp <- process_raw_tga(raw_file)
  
  # extract data
  mod_df <- tmp$data
  
  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_C < 120 | mod_df$temp_C > 650),]
  
  # name variables
  temp <- mod_df$temp_C
  obs <- mod_df$deriv
  
  # init mass
  mass_init <- tmp$mass_init
  
  W <- mod_df$mass_T
  n <- length(W)
  
  # starting vector, lower and upper bounds
  start_vec <- c(0.003, -0.15, 390, 200)
  lb <- c(0, -0.3, 0, 0)
  ub <- c(0.1, 0.3, 900, 900)
  
  # fit model
  frm <- deriv ~ deconvolve::fs_function(temp_C, h, s, p, w)
  start_list <- list(h = start_vec[1], 
                     s = start_vec[2], 
                     p = start_vec[3], 
                     w = start_vec[4])
  
  fit <- minpack.lm::nlsLM(frm, 
                           start = start_list, 
                           data = mod_df, 
                           control = minpack.lm::nls.lm.control(maxiter = 1024, maxfev = 1e+06), 
                           lower = lb, 
                           upper = ub)
  
  # extract 
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  h <- params[row.names(params) == 'h', 1]
  s <- params[row.names(params) == 's', 1]
  p <- params[row.names(params) == 'p', 1]
  w <- params[row.names(params) == 'w', 1]
  
  f_j <- function (x) {
    deconvolve::fs_function(x, h, s, p, w)
  }
  
  # initialise weights
  weights <- list('HC' = NA, 'CL' = NA, 'LG' = NA)
  
  # area under the curves
  weights <- (stats::integrate(Vectorize(f_j), lower = 120,
                               upper = 650)$value) * 100
  
  return(list(weights = weights,
              temp = temp, 
              obs = obs, 
              h = h, 
              s = s,
              p = p, 
              w = w))
  
}
