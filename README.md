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


library(ggplot2)
plot_temporal_multilayer(networks = time_nets, interlayer = interlayer,
                         sequence = list(c('P1', 'P3', 'P2', 'P4'), c('A3','A5' ,'A1', 'A2', 'A4', 'A6')),
                         binary = TRUE,node_size = 4, intra_lcol = "#00AFBB", label = TRUE,
                         inter_lcol = "#E7B800", inter_lty = 5, y_adjust = 2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

```

## Spatial multilayer network       

``` r

```

## Diagonal coupling multilayer network       

``` r

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
