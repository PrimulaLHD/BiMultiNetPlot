% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_diagonal_multilayer.R
\name{plot_diagonal_multilayer}
\alias{plot_diagonal_multilayer}
\title{plot diagonal coupling}
\usage{
plot_diagonal_multilayer(
  network1,
  network2,
  interlayer,
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
  link_2_size = 1,
  inter_lsize = 1,
  node_size = 2,
  binary = FALSE,
  extend = NULL,
  label = FALSE,
  label_size = 1.5,
  alpha = 0.2,
  ...
)
}
\arguments{
\item{network1}{a network matrix}

\item{network2}{another network matrix}

\item{interlayer}{inter layer data frame}

\item{gap}{overlap between the two layers}

\item{y_adjust}{control the shape of layer polygon}

\item{y_adjust_h}{control the shape of layer polygon}

\item{color_diagonal}{color of shared species between the two layers}

\item{color_1}{color of nodes (none shared) of layer 1}

\item{color_2}{color of nodes (none shared) of layer 2}

\item{color_link_1}{color of intra-links of layer 1}

\item{color_link_2}{color of intra-links of layer 2}

\item{color_interl}{color of inter-links}

\item{shape_diagonal}{point shape of shared species between the two layers}

\item{shape_1}{point shape of nodes (none shared) of layer 1}

\item{shape_2}{point shape of nodes (none shared) of layer 2}

\item{link_lty_1}{line type of intra-links of layer 1}

\item{link_lty_2}{line type of intra-links of layer 2}

\item{inter_lty}{line type of inter-links}

\item{link_1_size}{line size of intra-links of layer 1}

\item{link_2_size}{line size of intra-links of layer 2}

\item{inter_lsize}{line size of inter-links}

\item{node_size}{node sizes}

\item{binary}{TRUE or FALSE: return a binary or weighted network}

\item{extend}{node extra information}

\item{label}{TRUE or FALSE: nodes with label or not, default is FALSE}

\item{label_size}{size of labels}

\item{alpha}{transparency of layers}

\item{...}{to ggplot2}
}
\value{
ggplot2 object
}
\description{
plot diagonal coupling
}
\examples{
pollination <- matrix(c(5,3,5,3,1,1,
                        3,3,3,3,0,0,
                        3,0,0,0,1,0,
                        0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
dimnames = list(paste('P',1:4,sep=''), paste('A',1:6,sep='')))

herbivory <- matrix(c(7,0,5,3,0,1,
                      5,2,3,2,0,1,
                      1,0,0,0,1,0,
                      0,2,2,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                    dimnames = list(paste('P',1:4,sep=''),paste('H',1:6,sep='')))

interlayer_diag <- dplyr::tibble(layer_from = c(1, 1, 1, 1),
                          node_from = c('P1', 'P2', 'P3', 'P4'),
                          layer_to =  c(2, 2, 2, 2),
                          node_to =  c('P1',  'P2', 'P3', 'P4'),
                          weight = c(5, 4, 1, 2))

extend_diag <- data.frame(species = c(paste('P',1:4,sep=''), paste('A',1:6,sep=''),
                                      paste('P',1:4,sep=''),paste('H',1:6,sep='')),
                          layer = c(rep(1,4), rep(1, 6), rep(2,4), rep(2, 6)),
                          color = sample(1:5, size = 20, replace = TRUE))

library(ggplot2)
plot_diagonal_multilayer(pollination, herbivory,interlayer = interlayer_diag,
                         node_size = 6, inter_lty = 2, color_interl = "steelblue",
                         gap = 0.4, extend = NULL, binary = TRUE,
                         y_adjust = 1, y_adjust_h = 1,
                         label = FALSE, label_size = 4 )


}
