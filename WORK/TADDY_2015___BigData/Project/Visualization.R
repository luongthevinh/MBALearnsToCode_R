library(ggplot2)
library(GGally)
library(grid)
source("Data_and_Features.R")


plot_trip <- function(trip_data_table, arrow_size_mm = 1, title="", ...) {
  if (all(trip_data_table$velocity_check)) {
    ggp <- ggplot(trip_data_table, aes(x, y)) +
      geom_segment(aes(xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(arrow_size_mm, "mm")), ...) +
      ggtitle(title)
  } else {
    d <- trip_data_table
    d[, `:=`(bad_x = as.numeric(NA),
             bad_y = as.numeric(NA))]
    d[bad_velocity_data_indices(d), `:=`(bad_x = x,
                                         bad_y = y)]
    ggp <- ggplot(trip_data_table, aes(x, y)) +
      geom_segment(aes(xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(arrow_size_mm, "mm")), ...) +
      geom_point(aes(bad_x, bad_y), color='red', size=3) +
      ggtitle(title)
  }
  ggp
}


plot_trip_data_matrix <- function(trip_data_table) {
   ggpairs(trip_data_table[, .(velocity, acceleration,
                                 angular_velocity, #abs_angular_velocity,
                                 angular_acceleration)])  #, abs_angular_acceleration
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)
{
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}