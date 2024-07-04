#' plot diagonal coupling
#'
#' @param network1 a network matrix
#' @param network2 another network matrix
#' @param interlayer inter layer data frame
#' @param gap overlap between the two layers
#' @param y_adjust control the shape of layer polygon
#' @param y_adjust_h control the shape of layer polygon
#' @param color_diagonal color of shared species between the two layers
#' @param color_1 color of nodes (none shared) of layer 1
#' @param color_2 color of nodes (none shared) of layer 2
#' @param color_link_1 color of intra-links of layer 1
#' @param color_link_2 color of intra-links of layer 2
#' @param color_interl color of inter-links
#' @param shape_diagonal point shape of shared species between the two layers
#' @param shape_1 point shape of nodes (none shared) of layer 1
#' @param shape_2 point shape of nodes (none shared) of layer 2
#' @param link_lty_1 line type of intra-links of layer 1
#' @param link_lty_2 line type of intra-links of layer 2
#' @param inter_lty line type of inter-links
#' @param link_1_size line size of intra-links of layer 1
#' @param link_2_size line size of intra-links of layer 2
#' @param inter_lsize line size of inter-links
#' @param node_size node sizes
#' @param binary TRUE or FALSE: return a binary or weighted network
#' @param extend node extra information
#' @param label TRUE or FALSE: nodes with label or not, default is FALSE
#' @param label_size size of labels
#' @param alpha transparency of layers
#' @param ... to ggplot2
#'
#' @import ggplot2
#' @import dplyr
#' @return ggplot2 object
#' @export
#'
#' @examples
#' pollination <- matrix(c(5,3,5,3,1,1,
#'                         3,3,3,3,0,0,
#'                         3,0,0,0,1,0,
#'                         0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#' dimnames = list(paste('P',1:4,sep=''), paste('A',1:6,sep='')))
#'
#' herbivory <- matrix(c(7,0,5,3,0,1,
#'                       5,2,3,2,0,1,
#'                       1,0,0,0,1,0,
#'                       0,2,2,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#'                     dimnames = list(paste('P',1:4,sep=''),paste('H',1:6,sep='')))
#'
#' interlayer_diag <- dplyr::tibble(layer_from = c(1, 1, 1, 1),
#'                           node_from = c('P1', 'P2', 'P3', 'P4'),
#'                           layer_to =  c(2, 2, 2, 2),
#'                           node_to =  c('P1',  'P2', 'P3', 'P4'),
#'                           weight = c(5, 4, 1, 2))
#'
#' extend_diag <- data.frame(species = c(paste('P',1:4,sep=''), paste('A',1:6,sep=''),
#'                                       paste('P',1:4,sep=''),paste('H',1:6,sep='')),
#'                           layer = c(rep(1,4), rep(1, 6), rep(2,4), rep(2, 6)),
#'                           color = sample(1:5, size = 20, replace = TRUE))
#'
#' library(ggplot2)
#' plot_diagonal_multilayer(pollination, herbivory,interlayer = interlayer_diag,
#'                          node_size = 6, inter_lty = 2, color_interl = "steelblue",
#'                          gap = 0.4, extend = NULL, binary = TRUE,
#'                          y_adjust = 1, y_adjust_h = 1,
#'                          label = FALSE, label_size = 4 )
#'
#'
plot_diagonal_multilayer <- function(network1, network2, interlayer,
                                     gap = 0.6,
                                     y_adjust = 1.5,
                                     y_adjust_h = 1,
                                     color_diagonal = "#00AFBB",
                                     color_1 = "#E7B800",
                                     color_2 = "#FC4E07",
                                     color_link_1 = "grey80",
                                     color_link_2 = "grey80",
                                     color_interl = "grey80",
                                     shape_diagonal = 19,
                                     shape_1 = 19,
                                     shape_2 = 19,
                                     link_lty_1 = 1,
                                     link_lty_2 = 1,
                                     inter_lty = 2,
                                     link_1_size = 1,
                                     link_2_size =1,
                                     inter_lsize = 1,
                                     node_size = 2,
                                     binary = FALSE,
                                     extend = NULL,
                                     label = FALSE,
                                     label_size = 1.5,
                                     alpha = 0.2, ...){
  # network1 <- pollination
  # network2 <- herbivory

  # require(ggplot2)
  # require(dplyr)
  n_level_1 <- ncol(network1)
  n_level_2 <- ncol(network2)
  n_diagonal_level <- length(unique(c(rownames(network1), rownames(network2))))

  diagonal_species <- unique(c(rownames(network1), rownames(network2)))

  level_1_position <- data.frame(level_1 = colnames(network1), strength = colSums(network1),
                                 x1 = 1, y1 = seq(from = 1,
                                                  to = max(n_level_1, n_level_2, n_diagonal_level),
                                                  length = n_level_1)) %>%
    mutate(layer = 1)

  level_2_position <- data.frame(level_2 = colnames(network2),
                                 strength = colSums(network2),
                                 x3 = 3, y3 = seq(from = 1,
                                                  to = max(n_level_1, n_level_2, n_diagonal_level),
                                                  length = n_level_2)) %>%
    mutate(layer = 2)

  diagonal_position <- data.frame(diagonal = diagonal_species,
                                  y = seq(from = 1,
                                          to = max(n_level_1, n_level_2, n_diagonal_level),
                                          length = n_diagonal_level) + y_adjust_h)

  diagonal_position_1 <- right_join(diagonal_position,
                                    data.frame(diagonal = rownames(network1),
                                               strength = rowSums(network1)),
                                    by = "diagonal") %>%
    rename(y2 = y) %>%
    mutate(x2 = 2, layer = 1)

  diagonal_position_2 <- right_join(diagonal_position,
                                    data.frame(diagonal = rownames(network2),
                                               strength = rowSums(network2)),
                                    by = "diagonal") %>%
    rename(y4 = y) %>%
    mutate(x4 = 4, layer = 2)

  link_position_1 <- mat2df(network1) %>%
    rename(diagonal = Var1, level_1 = Var2, weight = Freq) %>%
    filter(weight > 0) %>%
    left_join(., diagonal_position_1, by = "diagonal") %>%
    left_join(., level_1_position, by = "level_1")

  link_position_2 <- mat2df(network2) %>%
    rename(diagonal = Var1, level_2 = Var2, weight = Freq) %>%
    filter(weight > 0) %>%
    left_join(., diagonal_position_2, by = "diagonal") %>%
    left_join(., level_2_position, by = "level_2")

  inter_position <- interlayer %>%
    left_join(., diagonal_position_1 %>%
                rename(layer_from = layer, node_from = diagonal) %>%
                select(layer_from, node_from, x2, y2),
              by = c("layer_from", "node_from")) %>%
    left_join(., diagonal_position_2 %>%
                rename(layer_to = layer, node_to = diagonal) %>%
                select(layer_to, node_to, x4, y4), by = c("layer_to", "node_to"))

  polygon_data <- list()

  for (i in 1:2) {
    polygon_data[[i]] <- data.frame(x = c(2*i-1-gap, 2*i + gap, 2*i + gap, 2*i-1-gap),
                                    y = c(-y_adjust, 1,
                                          (max(n_level_1, n_level_2,
                                               n_diagonal_level) + y_adjust + 1),
                                          max(n_level_1, n_level_2, n_diagonal_level)),
                                    layer = i)
  }

  polygon_data <- do.call("rbind", polygon_data)
  polygon_data$layer <- as.factor(polygon_data$layer)

  if (binary){
    if(is.null(extend)){
      p <- ggplot() +
        geom_segment(data = link_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     color = color_link_1, linetype = link_lty_1,
                     linewidth = link_1_size) +
        geom_segment(data = link_position_2,
                     aes(x = x3, y = y3, xend = x4, yend = y4),
                     color = color_link_2,linetype = link_lty_2,
                     linewidth = link_2_size) +
        geom_segment(data = inter_position,
                     aes(x = x2, y = y2, xend = x4, yend = y4),
                     color = color_interl,linetype = inter_lty,
                     linewidth = inter_lsize) +

        geom_point(data = level_1_position, aes(x = x1, y = y1),
                   color = color_1, shape = shape_1, size = node_size) +

        geom_point(data = level_2_position, aes(x = x3, y = y3),
                   color = color_2, shape = shape_2, size = node_size) +

        geom_point(data = diagonal_position_1, aes(x = x2, y = y2),
                   color = color_diagonal,
                   shape = shape_diagonal,
                   size = node_size) +

        geom_point(data = diagonal_position_2, aes(x = x4, y = y4),
                   color = color_diagonal,
                   shape = shape_diagonal,
                   size = node_size) +
        geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                     alpha = alpha, ...)

      if(label){
        p <- p +
          geom_text(data = level_1_position,
                    aes(x = x1, y = y1, label = level_1),
                    size = label_size) +
          geom_text(data = level_2_position,
                    aes(x = x3, y = y3, label = level_2),
                    size = label_size) +
          geom_text(data = diagonal_position_1,
                    aes(x = x2, y = y2, label = diagonal),
                    size = label_size) +
          geom_text(data = diagonal_position_2,
                    aes(x = x4, y = y4,label = diagonal),
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
      } else{ # without label
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
    } else{# with extend data
      p <- ggplot() +
        geom_segment(data = link_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2),
                     color = color_link_1, linetype = link_lty_1,
                     linewidth = link_1_size) +
        geom_segment(data = link_position_2,
                     aes(x = x3, y = y3, xend = x4, yend = y4),
                     color = color_link_2,linetype = link_lty_2,
                     linewidth = link_2_size) +
        geom_segment(data = inter_position,
                     aes(x = x2, y = y2, xend = x4, yend = y4),
                     color = color_interl,linetype = inter_lty,
                     linewidth = inter_lsize) +

        geom_point(data = level_1_position %>% rename(species = level_1) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x1, y = y1,
                       color = factor(color)),
                   shape = shape_1,
                   size = node_size) +

        geom_point(data = level_2_position %>% rename(species = level_2) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x3, y = y3,
                       color = factor(color)),
                   shape = shape_2,
                   size = node_size) +

        geom_point(data = diagonal_position_1 %>% rename(species = diagonal) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x2, y = y2,
                       color = factor(color)),
                   shape = shape_diagonal,
                   size = node_size) +

        geom_point(data = diagonal_position_2 %>% rename(species = diagonal) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x4, y = y4,
                       color = factor(color)),
                   shape = shape_diagonal,
                   size = node_size) +
        geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                     alpha = alpha, ...)

      if(label){
        p <- p +
          geom_text(data = level_1_position,
                    aes(x = x1, y = y1, label = level_1),
                    size = label_size) +
          geom_text(data = level_2_position,
                    aes(x = x3, y = y3, label = level_2),
                    size = label_size) +
          geom_text(data = diagonal_position_1,
                    aes(x = x2, y = y2, label = diagonal),
                    size = label_size) +
          geom_text(data = diagonal_position_2,
                    aes(x = x4, y = y4,label = diagonal),
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
      } else{ # without label
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


  } else{ # weighted network
    if(is.null(extend)){
      p <- ggplot() +
        geom_segment(data = link_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = color_link_1, linetype = link_lty_1) +
        geom_segment(data = link_position_2,
                     aes(x = x3, y = y3, xend = x4, yend = y4,
                         linewidth = weight),
                     color = color_link_2,linetype = link_lty_2) +
        geom_segment(data = inter_position,
                     aes(x = x2, y = y2, xend = x4, yend = y4,
                         linewidth = weight),
                     color = color_interl, linetype = inter_lty) +

        geom_point(data = level_1_position,
                   aes(x = x1, y = y1, size = strength),
                   color = color_1, shape = shape_1) +

        geom_point(data = level_2_position,
                   aes(x = x3, y = y3, size = strength),
                   color = color_2, shape = shape_2) +

        geom_point(data = diagonal_position_1,
                   aes(x = x2, y = y2, size = strength),
                   color = color_diagonal,
                   shape = shape_diagonal) +
        geom_point(data = diagonal_position_2,
                   aes(x = x4, y = y4, size = strength),
                   color = color_diagonal,
                   shape = shape_diagonal) +
        geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                     alpha = alpha, ...)

      if(label){
        p <- p +
          geom_text(data = level_1_position,
                    aes(x = x1, y = y1, label = level_1),
                    size = label_size) +
          geom_text(data = level_2_position,
                    aes(x = x3, y = y3, label = level_2),
                    size = label_size) +
          geom_text(data = diagonal_position_1,
                    aes(x = x2, y = y2, label = diagonal),
                    size = label_size) +
          geom_text(data = diagonal_position_2,
                    aes(x = x4, y = y4,label = diagonal),
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
      } else{ # without label
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
    } else{# with extend data
      p <- ggplot() +
        geom_segment(data = link_position_1,
                     aes(x = x1, y = y1, xend = x2, yend = y2,
                         linewidth = weight),
                     color = color_link_1, linetype = link_lty_1) +
        geom_segment(data = link_position_2,
                     aes(x = x3, y = y3, xend = x4, yend = y4,
                         linewidth = weight),
                     color = color_link_2,linetype = link_lty_2) +
        geom_segment(data = inter_position,
                     aes(x = x2, y = y2, xend = x4, yend = y4,
                         linewidth = weight),
                     color = color_interl,linetype = inter_lty) +

        geom_point(data = level_1_position %>% rename(species = level_1) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x1, y = y1,
                       color = factor(color),
                       size = strength),
                   shape = shape_1) +

        geom_point(data = level_2_position %>% rename(species = level_2) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x3, y = y3,
                       color = factor(color),
                       size = strength),
                   shape = shape_2) +

        geom_point(data = diagonal_position_1 %>% rename(species = diagonal) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x2, y = y2,
                       color = factor(color),
                       size = strength),
                   shape = shape_diagonal) +

        geom_point(data = diagonal_position_2 %>% rename(species = diagonal) %>%
                     left_join(.,extend, by = c("species", "layer")),
                   aes(x = x4, y = y4,
                       size = strength,
                       color = factor(color)),
                   shape = shape_diagonal) +
        geom_polygon(data = polygon_data, aes(x = x, y = y, fill = layer),
                     alpha = alpha, ...)

      if(label){
        p <- p +
          geom_text(data = level_1_position,
                    aes(x = x1, y = y1, label = level_1),
                    size = label_size) +
          geom_text(data = level_2_position,
                    aes(x = x3, y = y3, label = level_2),
                    color = color_2,
                    shape = shape_2,
                    size = label_size) +
          geom_text(data = diagonal_position_1,
                    aes(x = x2, y = y2, label = diagonal),
                    size = label_size) +
          geom_text(data = diagonal_position_2,
                    aes(x = x4, y = y4,label = diagonal),
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
      } else{ # without label
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

  p


}
