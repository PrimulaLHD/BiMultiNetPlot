#' Plot bipartite networks
#'
#' @param network a bipartite network matrix
#' @param color_low color of species in the lower trophic level (matrix row)
#' @param color_high color of species in the higher trophic level (matrix col)
#' @param shape_low node shape of species in the lower trophic level
#' @param shape_high node shape of of species in the higher trophic level
#' @param color_link color of links
#' @param link_type line type of links
#' @param binary TRUE or FALSE: return a binary or weighted network
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
#' library(ggplot2)
#' plot_monolayer(pollination[names(sort(rowSums(pollination), decreasing = TRUE)),
#' names(sort(colSums(pollination), decreasing = TRUE))],
#' shape_low = 19, shape_high = 19, color_link = "#FC4E07",
#' link_type = 1, binary = FALSE) + coord_flip()
#'
plot_monolayer <- function(network, color_low = "#00AFBB", color_high = "#E7B800",
                           shape_low = 19, shape_high = 19, color_link = "grey80",
                           link_type = 1, binary = FALSE){
  # require(ggplot2)
  # require(dplyr)
  n_low <- nrow(network)
  n_high <- ncol(network)

  low_position <- data.frame(low_level = rownames(network), low_strength = rowSums(network),
                             x1 = 1, y1 = seq(from = 1, to = max(n_low, n_high), length = n_low))

  high_position <- data.frame(high_level = colnames(network), high_strength = colSums(network),
                              x2 = 2, y2 = seq(from = 1, to = max(n_low, n_high), length = n_high))

  link_position <- as.data.frame.table(network) %>%
    rename(low_level = Var1, high_level = Var2, weight = Freq) %>%
    filter(weight > 0) %>% left_join(., low_position, by = "low_level") %>%
    left_join(., high_position, by = "high_level")

  if(binary){
    p <- ggplot() +
      geom_segment(data = link_position,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = color_link, linetype = link_type, size = 1) +

      geom_point(data = low_position, aes(x = x1, y = y1),
                 color = color_low, shape = shape_low, size = 4) +
      # geom_text(data = low_position, aes(x = x1, y = y1, label = low_level),
      #           size = 1.5
      # ) +
      geom_point(data = high_position, aes(x = x2, y = y2),
                 color = color_high, shape = shape_high, size = 4) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            #legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  } else{
    p <- ggplot() +
      geom_segment(data = link_position,
                   aes(x = x1, y = y1, xend = x2, yend = y2, size = weight),
                   color = color_link, linetype = link_type) +

      geom_point(data = low_position, aes(x = x1, y = y1, size = low_strength),
                 color = color_low, shape = shape_low) +
      geom_point(data = high_position, aes(x = x2, y = y2, size = high_strength),
                 color = color_high, shape = shape_high) +
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
  p
}
