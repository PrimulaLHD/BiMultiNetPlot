# BiMultiNetPlot
by Hai-Dong Li  
*BiMultiNetPlot* is an R package for visualizing ecological bipartite multilayer networks. 
             It is also an open-source, flexible package within the ggplot2 environment, 
             helping ecologists to better understand the multilayer nature of ecological networks.

# Instruction
To install this package, run the following codes:
``` r 
install.packages("devtools")
devtools::install_github("PrimulaLHD/BiMultiNetPlot")

# or you can use the remotes package like this:
install.packages("remotes")
remotes::install_github("PrimulaLHD/BiMultiNetPlot")

```

# Examples      
## Temporal multilayer network
``` r
library(ggplot2)
time_1 <- matrix(c(5,3,5,3,1,1,
                   3,3,3,3,0,0,
                   3,0,0,0,1,0,
                   0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                 dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))

time_2 <- matrix(c(7,0,5,3,0,1,
                   5,2,3,2,0,1,
                   1,0,0,0,1,0,
                   0,2,2,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                 dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))

time_3 <- matrix(c(5,1,1,1,0,1,
                   3,0,1,1,0,1,
                   2,0,0,0,0,0,
                   1,0,0,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                 dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))



time_nets <- list(time_1 = time_1, time_2 = time_2, time_3 = time_3)

interlayer <- dplyr::tibble(layer_from = c(1, 2, 2, 2),
                            node_from = c('P1', 'P1', 'P2', 'P4'),
                            layer_to =  c(2, 3, 3, 3),
                            node_to =  c('P1',  'P1', 'P2', 'P4'),
                            weight = c(5, 4, 1, 2))

plot_temporal_multilayer(networks = time_nets, interlayer = interlayer,
                         sequence = list(c('P1', 'P3', 'P2', 'P4'), c('A3','A5' ,'A1', 'A2', 'A4', 'A6')),
                         binary = TRUE,node_size = 4, intra_lcol = "#00AFBB", label = TRUE,
                         inter_lcol = "#E7B800", inter_lty = 5, y_adjust = 2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

```
![temporal multilayer network plot](https://github.com/PrimulaLHD/files/blob/main/tmn_plot.png)

## Spatial multilayer network       

``` r
site_1 <- matrix(c(5,3,5,3,1,1,
                   3,3,3,3,0,0,
                   3,0,0,0,1,0,
                   0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                   dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))

site_2 <- matrix(c(7,0,5,3,0,1,
                   5,2,3,2,0,1,
                   1,0,0,0,1,0,
                   0,2,2,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                 dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))

site_3 <- matrix(c(5,1,1,1,0,1,
                   3,0,1,1,0,1,
                   2,0,0,0,0,0,
                   1,0,0,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
                 dimnames = list(paste('P', 1:4,sep=''), paste('A', 1:6,sep='')))

spatial_nets <- list(site_1 = site_1, site_2 = site_2, site_3 = site_3)
```
When inter-layer links are symmetric:      
``` r 
interlayer_space <- dplyr::tibble(layer_from = c(1, 2, 1, 2, 1, 1),
                           node_from = c('P1', 'P1', 'P1', 'P4', 'A3', 'A3'),
                           layer_to =  c(2, 3, 3, 3, 2, 3),
                           node_to =  c('P1',  'P1', 'P1', 'P4', 'A3', 'A3'),
                           weight = c(5, 4, 1, 2, 1, 2))
plot_spatial_multilayer(networks = spatial_nets, interlayer = interlayer_space,
                        symmetry_inter = TRUE, binary = TRUE, y_adjust = 1,
                        y_adjust_h = 1, gap = 0.6, label = FALSE,
                        extend = NULL, inter_lty = 1, inter_lcol = "#00AFBB",
                        intra_lty = 1, intra_lcol = "#E7B800", inter_lsize = 0.5,
                        intra_lsize = 1, node_size = 4, label_size = 1.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))




```

When inter-layer links are asymmetric:      

``` r
interlayer_space_2 <- dplyr::tibble(layer_from = c(1, 2, 3, 2),
                             node_from = c('P1', 'P1', 'P1', 'P4'),
                             layer_to =  c(2, 3, 1, 3),
                             node_to =  c('P1',  'P1', 'P1', 'P4'),
                             weight = c(5, 4, 1, 2))

plot_spatial_multilayer(networks = spatial_nets, interlayer = interlayer_space_2,
                        symmetry_inter = FALSE, binary = TRUE, y_adjust = 1,
                        y_adjust_h = 1, gap = 0.6, label = FALSE,
                        extend = NULL, inter_lty = 1, inter_lcol = "#00AFBB",
                        intra_lty = 1, intra_lcol = "#E7B800", inter_lsize = 0.5,
                        intra_lsize = 1, node_size = 4, label_size = 1.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

``` 

## Diagonal coupling multilayer network       

``` r
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
```

## General multiplex network       

``` r

```

## Tripartite network       

``` r

```



















# Contribution              
Feel free to test it. Contributions and suggestions are welcome. You can open an issue or send a pull request.        

# Citation     
Li Hai-Dong. (2024). BiMultiNetPlot: An R package for visualizing ecological bipartite multilayer networks.    
