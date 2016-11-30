#' Plot colour-graded skeletal diagram.
#'
#' A wrapper for raster::spplot. Plots single column from input data frame on one of the built-in skeletal shapefiles.
#' @param data Data frame with at least one numeric or integer column containing data to be plotted,
#'    and at least one field corresponding to the element names or codes used in the shapefile attribute table.
#' @param plot.field Character. The name of the field in 'data' containing the data to be plotted. Defaults to "NISP"
#' @param join.field Character. The name of the field in 'data' that should be used to link to the shapefile attribute table. Defaults to "El_code"
#' @param template Character. The name of the skeletal template to be used. This should be an .Rda file in the package "data" directory. Built-in
#'    options are "Bos", "Ovis", "Equus", "Cervus", "Canis", "Sus". Defauls to "Bos".
#' @param background Logical: should a silhouette of the animal be plotted in the background? Defaults to TRUE.
#' @param details Logical: should details of bone features be drawn, as well as the element outlines? Defaults to TRUE.
#' @param bg Character. Colour for background silhouette, if 'background' is TRUE. Defaults to "gray80".
#' @param null.col. Colour for elements with missing data. Defaults to "white".
#' @return None.
#' @export
#' @examples
#' egData <- data.frame(El_code = c(1:140), NISP = rep(1:2, 70))
#' skelMap(egData)

skelMap <- function(data, plot.field="NISP", join.field="El_code", template="Bos", background=T, details=T, bg="gray80", null.col="white") {
      require(raster)
      require(maptools)
      elements <- readShapePoly(file.path("data", template, "elements.shp"))
      layers <- list()
      if(details==T) {
            dets <- list(readShapeLines(file.path("data", template, "details.shp")), first=F)
            layers[[length(layers)+1]] <- dets
      }
      if(background==T) {
            backgr <- list(readShapePoly(file.path("data", template, "background.shp")), fill=bg, first=T)
            layers[[length(layers)+1]] <- backgr
      }
      layers[length(layers)+1] <- list(list(elements, fill=null.col, first=T))
      elements@plotOrder <- 1:length(elements)
      elements@data <- cbind(elements@data, seq=1:length(elements))
      x <- merge(elements@data, data, by=join.field, all.x=T, all.y=F)
      elements@data <- x[order(x$seq),]
      spplot(elements, plot.field, xlim=c(10,200), ylim=c(130, 250), sp.layout=layers)
}


