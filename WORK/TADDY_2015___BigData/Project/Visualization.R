plot_trip <- function(trip_data_table, arrow_size_mm = 1, ...)
{
  library(ggplot2)
  library(grid)
  source("Data_and_Features.R")
  
  if (check_trip_data_quality(trip_data_table))
  {
    ggp <- ggplot(trip_data_table, aes(x, y)) +
      geom_segment(aes(xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(arrow_size_mm, "mm")), ...)
  }
  else {
    d <- trip_data_table
    d[, `:=`(bad_x = as.numeric(NA),
             bad_y = as.numeric(NA))]
    d[bad_trip_data_indices(d), `:=`(bad_x = x,
                                     bad_y = y)]
    ggp <- ggplot(trip_data_table, aes(x, y)) +
      geom_segment(aes(xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(arrow_size_mm, "mm")), ...) +
      geom_point(aes(bad_x, bad_y), color='red', size=3)
  }
  ggp
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)
{
  library(grid)
  
  # Make a list from the ... arguments and plotlist
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