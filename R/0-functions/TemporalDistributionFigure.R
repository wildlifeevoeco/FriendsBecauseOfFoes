# Function to produce the temporal distribution figure
#  with basically no customization at this time
#  .. but at least it is consistent across scripts! ha

# Alec Robitaille
# March 2018

TempDistFig <- function(DT) {
  ggplot(DT[order(mnth), .N, by = .(mnth, yr)]) + 
    geom_tile(aes(factor(mnth), factor(yr), fill = N)) + 
    scale_x_discrete(breaks = seq(1:12),
                     labels = month.abb) +  
    scale_y_discrete(breaks = DT[order(yr), unique(yr)], 
                     labels = as.character(DT[order(yr), unique(yr)])) + 
    labs(x = 'Month', y = 'Year') + 
    scale_fill_distiller(type = "div", palette = 6, direction = -1) + 
    coord_equal()
}


