# Function to produce a plot of locs with DT
#  by some group (to dev multipage pdf)
#  EASTING, NORTHING and idCol provided

# Alec Robitaille
# March 2018
PlotLocsBy <- function(DT, bounds, by, idCol){
  print(
    ggplot(bounds) +
      geom_polygon(aes(long, lat, group = group),
                   color = 'black', fill = 'grey', alpha = 0.25) +
      geom_point(aes(EASTING, NORTHING, color = factor(get(idCol))), 
                 data = DT) + 
      guides(color = FALSE) + 
      labs(title = paste('year: ', by)) + 
      coord_fixed()) 
  return(1)
}


