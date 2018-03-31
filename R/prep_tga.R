#' Deconvolve species' TGA data
#'
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @param xaxis logical option whether to include x-axis label
#' @param yaxis logical option whether to include y-axis label
#' @param n_curves optional to manually specify number of curves
#' @return saved deconvolved plot and weight
#' @import deconvolve
#'
#' @export

prep_tga <- function (species_data, output_folder, 
                                n_curves = NULL) {
  
  species_data <- species
  species <- as.data.frame(unique(as.character(species_data$species_code))) %>%
    `colnames<-`('species_code')

  species$xaxis <- FALSE
  xaxis_list <- c('BB', 'DD', 'G', 'JJ', 'KK', 'LL', 'MM', 'NN', 'S', 'V')
  species$xaxis[species[,1] %in% xaxis_list] <- TRUE
  
  species$yaxis <- FALSE
  yaxis_list <- c('BB', 'FF', 'H', 'II', 'J', 'JJ', 'K', 'L', 'LL', 'NN', 'Q', 'V')
  species$yaxis[species[,1] %in% yaxis_list] <- TRUE
  
  species$n_curves <- NULL
  three <- c('KK', 'M', 'X')
  species$n_curves[species[,1] %in% three] <- 3
  species$n_curves[species[,1] == 'CC'] <- 4
  
  species_list <- lapply(split(species, seq(nrow(species))), as.list)
  
  species_deconvolve <- function (list_item, species_data, output_folder) {
    
    x <- list_item$species_code
    xaxis <- list_item$xaxis
    yaxis <- list_item$yaxis
    n_curves <- list_item$n_curves
    if (is.na(n_curves)) n_curves = NULL
    
    file <- paste0('raw/TGA/', x, '_TGA.csv')
    munge <- process_raw_tga(file)
    
    output <- deconvolve::deconvolve(munge, upper_temp = 650, n_curves = n_curves)
    
    sp_name <- gsub(' ', '_', species_data$species[species_data$species_code == x][1])
    spname <- species_data$sp_abrev[species_data$species_code == x][1]
    gf <- species_data$gf[species_data$species_code == x][1]
    
    fit <- output$minpack.lm  
    params <- as.data.frame(summary(fit)$coefficients[,1])
    
    temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
    data <- output$data
    
    png(paste0(output_folder, gf, '/', sp_name, '.png'), width = 560, height = 480, 'px', bg = 'transparent')
    
    if (!isTRUE(xaxis) && !isTRUE(yaxis)) {
      
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
      rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
      axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
      axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
      
    } else if (!isTRUE(xaxis) && isTRUE(yaxis)) {
      
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
      rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
      axis(side = 1, at = c(150, 400, 650), labels = FALSE)
      axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
           labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
      
    } else if (isTRUE(xaxis) && isTRUE(yaxis)) {
      
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
      rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
      axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
      axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
           labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
      
    } else {
      
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
      rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
      plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
           ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
      axis(side = 1, at = c(150, 400, 650), labels = FALSE)
      axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
      
    }
    
    if (output$n_peaks == 4) {
      
      y1 <- fs_mixture(x = temp,
                       h1 = params['h1',], s1 = params['s1',],
                       p1 = params['p1',], w1 = params['w1',],
                       h2 = params['h2',], s2 = params['s2',],
                       p2 = params['p2',], w2 = params['w2',],
                       h3 = params['h3',], s3 = params['s3',],
                       p3 = params['p3',], w3 = params['w3',],
                       h0 = params['h0',], s0 = params['s0',],
                       p0 = params['p0',], w0 = params['w0',])
      
      y5 <- fs_function(x = temp,
                        h = params['h0',], s = params['s0',],
                        p = params['p0',], w = params['w0',])
      
      lines(temp, y5, lty = 5, lwd = 2.5, col = 'orange')
      
    } 
    
    if (output$n_peaks == 3) {
      
      y1 <- fs_mixture(x = temp,
                       h1 = params['h1',], s1 = params['s1',],
                       p1 = params['p1',], w1 = params['w1',],
                       h2 = params['h2',], s2 = params['s2',],
                       p2 = params['p2',], w2 = params['w2',],
                       h3 = params['h3',], s3 = params['s3',],
                       p3 = params['p3',], w3 = params['w3',])
    }
    
    lines(temp, y1, lty = 1, lwd = 2)
    
    y2 <- fs_function(x = temp,
                      h = params['h1',], s = params['s1',],
                      p = params['p1',], w = params['w1',])
    lines(temp, y2, lty = 6, lwd = 3.5, col = 'blue')
    
    y3 <- fs_function(x = temp,
                      h = params['h2',], s = params['s2',],
                      p = params['p2',], w = params['w2',])
    lines(temp, y3, lty = 3, lwd = 3.5, col = 'red')
    
    y4 <- fs_function(x = temp,
                      h = params['h3',], s = params['s3',],
                      p = params['p3',], w = params['w3',])
    lines(temp, y4, lty = 4, lwd = 3.5, col = 'green3')
    
    legend(650, .01,
           xjust = 1,
           legend = spname, 
           text.font = 3,
           cex = 3.1,
           bty = 'n')
    
    dev.off()
    
    mean_weights <- data.frame(t(output$mean_weights))
    mean_weights$wt_type <- 'mean'
    ci_weights <- data.frame(output$CI_weights)
    ci_weights$wt_type <- rownames(ci_weights)
    weights <- rbind(mean_weights, ci_weights)
    
    if (ncol(weights) == 4) {
      weights$HC_1 <- NA
      colnames(weights)[1] <- 'HC_2'
    }
    
    params$species_code <- x
    params$parameter <- row.names(params)
    colnames(params) <- c('coefficient', 'species_code', 'parameter')
    params <- reshape2::dcast(params, species_code ~ parameter, value.var = 'coefficient')
    
    weights$species_code <- x
    weights <- weights[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG', 'wt_type')]
    
    return(list(weights = weights, params = params))
    
  }
  
  ###debug -- legend failing. 
  spp_deconvolve <- lapply(species_list[[1:2]], species_deconvolve, 
                           species_data = species_data,
                           output_folder = output_folder)
  
  # collate parameters and save tables
  tga_param_table(species_deconvolved_list = spp_deconvolve, 
                  species_df = species_data, 
                  output_folder = 'docs/')
  
  weight_estimates <- dplyr::bind_rows(lapply(1:length(spp_deconvolve), function(x) {
    return(spp_deconvolve[[x]]$weights)
  }))
  
  weight_estimates
  
}