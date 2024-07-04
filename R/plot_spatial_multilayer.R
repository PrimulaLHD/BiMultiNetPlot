
#' Plot out spatial multilayer networks
#'
#' @param networks a list object of interaction network matrices
#' @param interlayer a data frame with `layer_from`,`node_from`,`layer_to`, `node_to`, and `weight` columns.
#' @param sequence a list
#' @param symmetry_inter TRUE or FALSE
#' @param binary plot a binary network or a weighted network
#' @param y_adjust control the shape of polygon
#' @param y_adjust_h control the shape of polygon
#' @param gap overlap between layers
#' @param label node label
#' @param extend extend information about nodes
#' @param inter_lty line type of inter-links
#' @param inter_lcol line color of inter-links
#' @param intra_lty line type of intra-links
#' @param intra_lcol line color of intra-links
#' @param inter_lsize line size of inter-links
#' @param intra_lsize line size of intra-links
#' @param node_size node size
#' @param label_size size of labels
#' @param alpha transparency of layers
#' @param ... to ggplot2
#'
#' @import ggplot2
#' @import dplyr
#' @return ggplot object
#' @export
#'
#' @examples
#' site_1 <- matrix(c(5,3,5,3,1,1,
#'                    3,3,3,3,0,0,
#'                    3,0,0,0,1,0,
#'                    0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#'                    dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))
#'
#' site_2 <- matrix(c(7,0,5,3,0,1,
#'                    5,2,3,2,0,1,
#'                    1,0,0,0,1,0,
#'                    0,2,2,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#'                  dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))
#'
#' site_3 <- matrix(c(5,1,1,1,0,1,
#'                    3,0,1,1,0,1,
#'                    2,0,0,0,0,0,
#'                    1,0,0,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#'                  dimnames = list(paste('P', 1:4,sep=''), paste('A', 1:6,sep='')))
#'
#'
#'
#' spatial_nets <- list(site_1 = site_1, site_2 = site_2, site_3 = site_3)
#'
#' interlayer_space <- dplyr::tibble(layer_from = c(1, 2, 1, 2, 1, 1),
#'                            node_from = c('P1', 'P1', 'P1', 'P4', 'A3', 'A3'),
#'                            layer_to =  c(2, 3, 3, 3, 2, 3),
#'                            node_to =  c('P1',  'P1', 'P1', 'P4', 'A3', 'A3'),
#'                            weight = c(5, 4, 1, 2, 1, 2))
#'

#' interlayer_space_2 <- dplyr::tibble(layer_from = c(1, 2, 3, 2),
#'                              node_from = c('P1', 'P1', 'P1', 'P4'),
#'                              layer_to =  c(2, 3, 1, 3),
#'                              node_to =  c('P1',  'P1', 'P1', 'P4'),
#'                              weight = c(5, 4, 1, 2))
#' library(ggplot2)
#' plot_spatial_multilayer(networks = spatial_nets, interlayer = interlayer_space,
#'                         symmetry_inter = TRUE, binary = TRUE, y_adjust = 1,
#'                         y_adjust_h = 1, gap = 0.6, label = FALSE,
#'                         extend = NULL, inter_lty = 1, inter_lcol = "#00AFBB",
#'                         intra_lty = 1, intra_lcol = "#E7B800", inter_lsize = 0.5,
#'                         intra_lsize = 1, node_size = 4, label_size = 1.5)+
#'   scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#'
#' plot_spatial_multilayer(networks = spatial_nets, interlayer = interlayer_space_2,
#'                         symmetry_inter = FALSE, binary = TRUE, y_adjust = 1,
#'                         y_adjust_h = 1, gap = 0.6, label = FALSE,
#'                         extend = NULL, inter_lty = 1, inter_lcol = "#00AFBB",
#'                         intra_lty = 1, intra_lcol = "#E7B800", inter_lsize = 0.5,
#'                         intra_lsize = 1, node_size = 4, label_size = 1.5)+
#'   scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#'
plot_spatial_multilayer <- function(networks, interlayer, sequence = NULL,
                                    symmetry_inter = FALSE, binary = FALSE, y_adjust = 1,
                                    y_adjust_h = 1.5, gap = 0.6, label = FALSE,
                                    extend = NULL, inter_lty = 2, inter_lcol = "black",
                                    intra_lty = 1, intra_lcol = "black", inter_lsize = 1,
                                    intra_lsize = 1, node_size = 1, label_size = 1.5,
                                    alpha = 0.2, ...){

  # require(ggplot2)
  # require(dplyr)

  n_layer <- length(networks) # number of layers
  n_low <- nrow(networks[[1]]) # number of low level species
  n_high <- ncol(networks[[1]]) # number of high level species

  if(is.null(sequence)){
    networks <- networks
  } else{
    for (i in 1:n_layer) {
      networks[[i]] <- networks[[i]][sequence[[1]], sequence[[2]]]
    }
  }


  low_position <- list()
  high_position <- list()
  if (n_high > n_low){
    for (i in 1:n_layer) {
      low_position[[i]] <- data.frame(low_level = rownames(networks[[i]]),
                                      low_strength = rowSums(networks[[i]]),
                                      x1 = 2*i - 1,
                                      y1 = seq(from = 1, to = n_high, length = n_low),
                                      layer = i)
      high_position[[i]] <- data.frame(high_level = colnames(networks[[i]]),
                                       high_strength = colSums(networks[[i]]),
                                       x2 = 2*i, y2 = ((1:n_high) + y_adjust_h), layer = i)
    }
  } else {
    for (i in 1:n_layer) {
      low_position[[i]] <- data.frame(low_level = rownames(networks[[i]]),
                                      low_strength = rowSums(networks[[i]]),
                                      x1 = 2*i - 1, y1 = c(1:n_low), layer = i)
      high_position[[i]] <- data.frame(high_level = colnames(networks[[i]]),
                                       high_strength = colSums(networks[[i]]),
                                       x2 = 2*i,
                                       y2 = (seq(from = 1, to = n_low, length = n_high) + y_adjust_h),
                                       layer = i)
    }
  }

  low_position <- do.call("rbind", low_position)
  high_position <- do.call("rbind", high_position)

  species_position <- low_position %>%
    rename(species = low_level, strength = low_strength, x = x1, y = y1) %>%
    mutate(level = "low") %>%
    rbind(., high_position %>%
            rename(species = high_level, strength = high_strength, x = x2, y = y2) %>%
            mutate(level = "high"))

  intra_link_position <- list()

  if (n_high > n_low){
    for (i in 1:n_layer) {
      intra_link_position[[i]] <- mat2df(networks[[i]]) %>%
        rename(low_level = Var1, high_level = Var2, weight = Freq) %>%
        filter(weight > 0) %>%
        left_join(.,data.frame(low_level = rownames(networks[[i]]),
                               low_strength = rowSums(networks[[i]]),
                               x1 = 2*i - 1,
                               y1 = seq(from = 1, to = n_high, length = n_low)),
                  by = "low_level") %>%
        left_join(.,data.frame(high_level = colnames(networks[[i]]),
                               high_strength = colSums(networks[[i]]),
                               x2 = 2*i, y2 = ((1:n_high) + y_adjust_h)),
                  by = "high_level") %>%
        mutate(layer = i)
    }
  } else {
    for (i in 1:n_layer) {
      intra_link_position[[i]] <- as.data.frame.table(networks[[i]]) %>%
        rename(low_level = Var1, high_level = Var2, weight = Freq) %>%
        filter(weight > 0) %>%
        left_join(.,data.frame(low_level = rownames(networks[[i]]),
                               low_strength = rowSums(networks[[i]]),
                               x1 = 2*i - 1, y1 = c(1:n_low)),
                  by = "low_level") %>%
        left_join(.,data.frame(high_level = colnames(networks[[i]]),
                               high_strength = colSums(networks[[i]]),
                               x2 = 2*i,
                               y2 = (seq(from = 1, to = n_low, length = n_high) + y_adjust_h)),
                  by = "high_level") %>%
        mutate(layer = i)
    }

  }

  intra_link_position <- do.call("rbind",intra_link_position)

  interlayer_position <- interlayer %>% left_join(., species_position %>%
                                                    rename(layer_from = layer,
                                                           node_from = species,
                                                           x1 = x, y1 = y) %>%
                                                    select(layer_from, node_from, x1, y1),
                                                  by = c("layer_from", "node_from")) %>%
    left_join(., species_position %>%
                rename(layer_to = layer, node_to = species, x2 = x, y2 = y) %>%
                select(layer_to, node_to, x2, y2), by = c("layer_to", "node_to")) %>%
    mutate(diff_layer = layer_to-layer_from)
  interlayer_position_1 <- interlayer_position %>% filter(abs(diff_layer) == 1)
  interlayer_position_1b <- interlayer_position %>% filter(diff_layer > 0)

  interlayer_position_2 <- interlayer_position %>% filter(abs(diff_layer) != 1)
  interlayer_position_2b <- interlayer_position %>% filter(diff_layer < 0)

  inter_node_position <- interlayer %>% filter(weight > 0) %>% select(layer_from, node_from) %>%
    rename(layer = layer_from, species = node_from) %>%
    left_join(., species_position %>% select(layer, species, x, y, level),
              by = c("layer", "species")) %>%
    rbind(., interlayer %>% filter(weight > 0) %>% select(layer_to, node_to) %>%
            rename(layer = layer_to, species = node_to) %>%
            left_join(., species_position %>% select(layer, species, x, y, level),
                      by = c("layer", "species")))

  polygon_data <- list()

  for (i in 1:n_layer) {
    polygon_data[[i]] <- data.frame(x = c(2*i-1-gap, 2*i + gap, 2*i + gap, 2*i-1-gap),
                                    y = c(-y_adjust, 1, (max(n_low, n_high) + y_adjust + 1),
                                          max(n_low, n_high)),
                                    layer = i)
  }

  polygon_data <- do.call("rbind", polygon_data)
  polygon_data$layer <- as.factor(polygon_data$layer)

  if (symmetry_inter) {
    if (binary){

      if(is.null(extend)){
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2),
                       color = intra_lcol, linewidth = intra_lsize,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol, curvature = 0,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = species_position %>% filter(strength > 0),
                     aes(x = x, y = y, color = level), size = node_size) +
          geom_point(data = inter_node_position,
                     aes(x = x, y = y, color = level), size = node_size) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)
        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        }
      } else{ # with extend data
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2),
                       color = intra_lcol, linewidth = intra_lsize,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol, curvature = 0,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = extend %>% left_join(., species_position,
                                                 by = c("layer", "species")),
                     aes(x = x, y = y, color = factor(color)), size = node_size) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)



        if(label){
          p <- p +
            geom_text(data = extend %>% left_join(., species_position,
                                                  by = c("layer", "species")),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        }
      }
    } else{ # weighted

      if(is.null(extend)){
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2,
                           linewidth = weight),
                       color = intra_lcol,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = species_position %>% filter(strength > 0),
                     aes(x = x, y = y, color = level, size = strength)) +
          geom_point(data = inter_node_position,
                     aes(x = x, y = y, color = level), size = 0) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)



        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        }
      } else{ # with extend data
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2,
                           linewidth = weight),
                       color = intra_lcol,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = extend %>% left_join(., species_position,
                                                 by = c("layer", "species")),
                     aes(x = x, y = y, color = factor(color), size = strength)) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)


        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        }
      }
    }



  } else if (!symmetry_inter){ # asymetry

    if (binary){
      if(is.null(extend)){
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2),
                       color = intra_lcol, linewidth = intra_lsize,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1b,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     #curvature = -0.2,
                     arrow = arrow(length = unit(0.05, "npc")),
                     color = inter_lcol,
                     linewidth = inter_lsize,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2b,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     curvature = 0.2,
                     arrow = arrow(length = unit(0.05, "npc")),
                     color = inter_lcol,
                     linewidth = inter_lsize,
                     linetype = inter_lty) + # inter-links
          geom_point(data = species_position %>% filter(strength > 0),
                     aes(x = x, y = y, color = level), size = node_size) +
          geom_point(data = inter_node_position,
                     aes(x = x, y = y, color = level), size = node_size) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)
        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        }
      } else{ # with extend data
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2),
                       color = intra_lcol, linewidth = intra_lsize,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1b,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol,
                     #curvature = -0.2,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2b,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     linewidth = inter_lsize,
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links

          geom_point(data = extend %>% left_join(., species_position,
                                                 by = c("layer", "species")),
                     aes(x = x, y = y, color = factor(color)), size = node_size) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)



        if(label){
          p <- p +
            geom_text(data = extend %>% left_join(., species_position,
                                                  by = c("layer", "species")),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        }
      }
    } else{ # weighted

      if(is.null(extend)){
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2,
                           linewidth = weight),
                       color = intra_lcol,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1b,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol,
                     #curvature = -0.2,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2b,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = species_position %>% filter(strength > 0),
                     aes(x = x, y = y, color = level, size = strength)) +
          geom_point(data = inter_node_position,
                     aes(x = x, y = y, color = level), size = 0) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)



        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        }
      } else{ # with extend data
        p <- ggplot() +
          geom_segment(data = intra_link_position,
                       aes(x = x1, y = y1, xend = x2, yend = y2,
                           linewidth = weight),
                       color = intra_lcol,
                       linetype = intra_lty) + # intra-links
          geom_curve(data = interlayer_position_1b,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol,
                     #curvature = -0.2,
                     linetype = inter_lty) + # inter-links
          geom_curve(data = interlayer_position_2b,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = inter_lcol, curvature = 0.2,
                     linetype = inter_lty) + # inter-links
          geom_point(data = extend %>% left_join(., species_position,
                                                 by = c("layer", "species")),
                     aes(x = x, y = y, color = factor(color), size = strength)) +
          geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                       alpha = alpha, ...)


        if(label){
          p <- p +
            geom_text(data = species_position %>% filter(strength > 0),
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            geom_text(data = inter_node_position,
                      aes(x = x, y = y, label = species),
                      size = label_size) +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        } else{ # plot nodes without label
          p <- p +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
        }
      }
    }

  } else {
    stop("Error, only TRUE or FALSE can be used!")
  }
  p
}
