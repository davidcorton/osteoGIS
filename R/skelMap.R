
skelMap <- function(data, plot.field="NISP", join.field="El_code", template="Bos", background=T, details=T, bg="gray80", null.col="white") {
      require(raster)
      require(maptools)
      elements <- readShapePoly(file.path(template, "elements.shp"))
      layers <- list()
      if(details==T) {
            dets <- list(readShapeLines(file.path(template, "details.shp")), first=F)
            layers[[length(layers)+1]] <- dets
      }
      if(background==T) {
            backgr <- list(readShapePoly(file.path(template, "background.shp")), fill=bg, first=T)
            layers[[length(layers)+1]] <- backgr
      }
      layers[length(layers)+1] <- list(list(elements, fill=null.col, first=T))
      elements@plotOrder <- 1:length(elements)
      elements@data <- cbind(elements@data, seq=1:length(elements))
      x <- merge(elements@data, data, by=join.field, all.x=T, all.y=F)
      elements@data <- x[order(x$seq),]
      spplot(elements, plot.field, xlim=c(10,200), ylim=c(130, 250), sp.layout=layers)
}


