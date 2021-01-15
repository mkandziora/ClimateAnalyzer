# make different maps


make_google_map <- function(points_high, plot_points_high, elev_points_high, fn){

  x <- points_high[, 1]
  y <- points_high[, 2]
  
  # get plot to locate area
  bc_bbox <- make_bbox(lat = y, lon = x, f = 0.2)
  bc_big <- get_map(location = bc_bbox, zoom = 5, source = "google", maptype = "terrain")
  
  png(fn)
  ggmap(bc_big) + 
    #geom_polygon(data = fortify(pp), aes(x, y, group = group) ) + 
    geom_point(data = as.data.frame(plot_points_high), aes(x, y, color=elev_points_high), size=0.02)
  dev.off()
}

