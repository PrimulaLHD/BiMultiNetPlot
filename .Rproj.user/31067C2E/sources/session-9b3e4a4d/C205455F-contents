#' plot tripartite networks
#'
#' @param network1 a network matrix
#' @param network2 another network matrix
#' @param color_shared color of shared species between the two networks
#' @param color_1 color of nodes (none shared) of network 1
#' @param color_2 color of nodes (none shared) of network 2
#' @param color_link_1 color of intra-links of layer 1
#' @param color_link_2 color of intra-links of layer 2
#' @param shape_shared point shape of shared species between the two networks
#' @param shape_1 point shape of nodes (none shared) of network 1
#' @param shape_2 point shape of nodes (none shared) of network 2
#' @param link_lty_1 line type of intra-links of network 1
#' @param link_lty_2 line type of intra-links of network 2
#' @param link_size_1 line size of intra-links of network 1
#' @param link_size_2 line size of intra-links of network 2
#' @param binary TRUE or FALSE: return a binary or weighted network
#' @param label TRUE or FALSE: nodes with label or not, default is FALSE
#' @param label_size label size
#' @param node_size node size
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
#' library(ggplot2)
#' plot_tripartite(pollination, herbivory, binary = FALSE, label = FALSE) +
#' coord_flip(expand = TRUE)

plot_tripartite <- function(network1, network2,
                            color_shared = "#00AFBB",
                            color_1 = "#E7B800",
                            color_2 = "#FC4E07",
                            color_link_1 = "grey80",
                            color_link_2 = "grey80",
                            shape_shared = 19,
                            shape_1 = 19,
                            shape_2 = 19,
                            link_lty_1 = 1,
                            link_lty_2 = 1,
                            link_size_1 = 1,
                            link_size_2 = 1,
                            binary = FALSE,
                            label = FALSE,
                            label_size = 2,
                            node_size = 4){ # shared species were rows
  # require(ggplot2)
  # require(dplyr)
  n_level_1 <- ncol(network1)
  n_level_2 <- ncol(network2)
  n_shared_level <- length(unique(c(rownames(network1), rownames(network2))))
  shared_species <- unique(c(rownames(network1), rownames(network2)))

  level_1_position <- data.frame(level_1 = colnames(network1), level_1_strength = colSums(network1),
                                 x1 = 1, y1 = seq(from = 1,
                                                  to = max(n_level_1, n_level_2, n_shared_level),
                                                  length = n_level_1))
  level_2_position <- data.frame(level_2 = colnames(network2), level_2_strength = colSums(network2),
                                 x3 = 3, y3 = seq(from = 1,
                                                  to = max(n_level_1, n_level_2, n_shared_level),
                                                  length = n_level_2))
  shared_position <- data.frame(shared = shared_species,
                                x2 = 2, y2 = seq(from = 1,
                                                 to = max(n_level_1, n_level_2, n_shared_level),
                                                 length = n_shared_level))
  shared_position_1 <- right_join(shared_position,
                                  data.frame(shared = rownames(network1),
                                             shared_1_strength = rowSums(network1)),
                                  by = "shared")
  shared_position_2 <- right_join(shared_position,
                                  data.frame(shared = rownames(network2),
                                             shared_2_strength = rowSums(network2)),
                                  by = "shared")

  link_position_1 <- as.data.frame.table(network1) %>%
    rename(shared = Var1, level_1 = Var2, weight = Freq) %>%
    filter(weight > 0) %>% left_join(., shared_position_1, by = "shared") %>%
    left_join(., level_1_position, by = "level_1")

  link_position_2 <- as.data.frame.table(network2) %>%
    rename(shared = Var1, level_2 = Var2, weight = Freq) %>%
    filter(weight > 0) %>% left_join(., shared_position_2, by = "shared") %>%
    left_join(., level_2_position, by = "level_2")

  if (binary){
    p <- ggplot() +
      geom_segment(data = link_position_1,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = color_link_1, linetype = link_lty_1,
                   linewidth = link_size_1) +
      geom_segment(data = link_position_2,
                   aes(x = x2, y = y2, xend = x3, yend = y3),
                   color = color_link_2,linetype = link_lty_2,
                   linewidth = link_size_2) +

      geom_point(data = level_1_position, aes(x = x1, y = y1),
                 color = color_1, shape = shape_1, size = node_size) +
      geom_point(data = level_2_position, aes(x = x3, y = y3),
                 color = color_2, shape = shape_2, size = node_size) +
      geom_point(data = shared_position_1, aes(x = x2, y = y2),
                 color = color_shared, shape = shape_shared,
                 size = node_size) +
      geom_point(data = shared_position_2, aes(x = x2, y = y2,),
                 color = color_shared, shape = shape_shared,
                 size = node_size)
    if (label){
      p <- p +
        geom_text(data = level_1_position, aes(x = x1, y = y1, label = level_1),
                  size = label_size) +
        geom_text(data = level_2_position, aes(x = x3, y = y3, label = level_2),
                  size = label_size) +
        geom_text(data = shared_position, aes(x = x2, y = y2, label = shared),
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
    }else{
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

  } else{
    p <- ggplot() +
      geom_segment(data = link_position_1,
                   aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = weight),
                   color = color_link_1, linetype = link_lty_1) +
      geom_segment(data = link_position_2,
                   aes(x = x2, y = y2, xend = x3, yend = y3, linewidth = weight),
                   color = color_link_2,linetype = link_lty_2) +
      geom_point(data = level_1_position, aes(x = x1, y = y1, size = level_1_strength),
                 color = color_1, shape = shape_1) +
      geom_point(data = level_2_position, aes(x = x3, y = y3, size = level_2_strength),
                 color = color_2, shape = shape_2) +
      geom_point(data = shared_position_1, aes(x = x2, y = y2, size = shared_1_strength),
                 color = color_shared, shape = shape_shared, alpha = 0.5) +
      geom_point(data = shared_position_2, aes(x = x2, y = y2, size = shared_2_strength),
                 color = color_shared, shape = shape_shared)

    if(label) {
      p <- p +
        geom_text(data = level_1_position, aes(x = x1, y = y1, label = level_1),
                  size = label_size) +
        geom_text(data = level_2_position, aes(x = x3, y = y3, label = level_2),
                  size = label_size) +
        geom_text(data = shared_position, aes(x = x2, y = y2, label = shared),
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
    } else{
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
  p
}
