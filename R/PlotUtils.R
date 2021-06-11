#' Conveniently save a plot to a file
#'
#' Basically a convenience wrapper that opens graphical device (with Cairo), saves the provided ggplot object and closes it again.  Helpfully it will
#' automatically calculate and use an aspect ratio if possible (and not provided).
#'
#' @param p The plot to be saved.
#' @param filename The filename of the saved plot (character string) omitting the file type extension
#' @param type The type of file to make, defaults to "png" but also "pdf" and others are possible (see options to Cairo::Cairo function)
#' @param height,width The height and width of the plot (as numeric) units will depend on the type parameter,
#' it is pixels for "png" and inches for "pdf
#' @param ... Passed to Cairo function
#'
#' @importFrom Cairo Cairo
#'
#' @return Nothing, function prints a plot to a file.
#' @export


magicPlot <- function(p, filename, type = "png", height = NULL, width = NULL, ...){


  aspect.ratio = tryCatch({
    getGGAR(p)
  }, warning = function(warning_condition) {
    1
  }, error = function(error_condition) {
    1
  }, finally={
  })


  # set a basic height for scaling if nothing provided
  if(missing(height) && missing(width)){
    if(type == "png") {height <- 700 }
    else if(type == "pdf") {height <- 7}
  }

  # scale to height
  if(is.null(width)) width <- height * aspect.ratio

  # or scale to width
  if(is.null(height)) height <- width * aspect.ratio

  # make and write the plot
  Cairo(file = paste(filename, type, sep ="."), type = type, width = width, height = height, ...)
  print(p)
  grDevices::dev.off()

}


#' Aspect ratio for a ggplot
#'
#' This internal function attempts to get the aspect ratio of a ggplot.
#' It takes multiple facets into account but only really works for plots where ggplot::coord_fixed() so that the
#' scales on the x and y axes are compatible.
#'
#' @param p a ggplot object
#'
#' @importFrom ggplot2 ggplot_build layer_scales wrap_dims
#' @keywords internal
getGGAR <- function(p){


  # Get the axes on the panels in terms of plot units
  y.range <- layer_scales(p)$y$range$range
  x.range <- layer_scales(p)$x$range$range
  y.size <- y.range[2] - y.range[1]
  x.size <- x.range[2] - x.range[1]

  #print(y.size)
  #print(x.size)

  # Get the number of facets
  p.built <- ggplot_build(p) # do this once to for efficiency
  all.facets <- c()
  for(table.counter in 1:length(p.built$data)){
    all.facets <- append(all.facets, unique(p.built$data[[table.counter]]$PANEL) )
  }
  n.facets <- length(unique(all.facets))
  #print(n.facets)

  # Given the number of facets and the nrow and ncol parameters, determine the numbers of rows and columns
  par <- p.built$layout$facet$params
  rows_cols <- wrap_dims(n.facets, par$nrow, par$ncol)
  #print(rows_cols)

  # And combine the rowsxcols with the panel sizes to make an approximate aspect ratio
  aspect.ratio <- (x.size * rows_cols[2]) / (y.size * rows_cols[1])

  return(aspect.ratio)

}

#'Get
#'
#' @param x Either the number of panels or something from which the number of panels can be derived
#' currently possible options are: a simple numeric (given the number of panels) or a Field (from
#' from which the number of panels are derived from the layers and additional possible facets)
#' @param rows.more.than.cols Preferred number of rows more than columns as an integer (can be negative).  For
#' example a value of "1" will optimise for more row than columns, "2" for two more rows, "0" for the same
#' number of rows as columns, "-1" for one less row than columns.  Note, the function doesn't guarantee
#' that it will work out like this, just that it will try to respect this preference if there is a choice.
#' @param type The type of plot as a character string (used for determing panels from dimensions).  Currently only
#' implemented for "Spatial".
#' @param nlayers Numeric for number of layers (in case only subset a subset of layers in x are plotted)
#' @param ntimes Numeric for number of time periods (Years and/or Days/Months/Seasons) (in case only subset a subset of
#' time periods in x are plotted)
#' @export


nOptCols <- function(x, rows.more.than.cols = 0, type = "Spatial", nlayers, ntimes) {

  if (! requireNamespace("DGVMTools", quietly = TRUE))  stop("The nOptCols function is designed to work with DGVMTools Field objects,
                                                             please make sure DGVMTools is installed.")


  # simple numeric
  if(is.numeric(x)) {
    npanels <- x
  }
  #  a single field (assume plotting all layers and Days/Months/Seasons/Years)
  else if(DGVMTools::is.Field(x)){

    if(tolower(type) == "spatial") {
      if(missing(nlayers)) nlayers <- length(DGVMTools::layers(x))
      if(missing(ntimes)) {
        ntimes <- 1
        if("Day" %in% DGVMTools::getDimInfo(x)) ntimes <- ntimes * DGVMTools::getDimInfo(x, "size")$Day
        else if("Month" %in% DGVMTools::getDimInfo(x)) ntimes <- ntimes * DGVMTools::getDimInfo(x, "size")$Month
        else if("Season" %in% DGVMTools::getDimInfo(x)) ntimes <- ntimes * DGVMTools::getDimInfo(x, "size")$Season
        if("Year" %in% DGVMTools::getDimInfo(x)) ntimes <- ntimes * DGVMTools::getDimInfo(x, "size")$Year
      }
      npanels <- ntimes * nlayers

    }

    else {
      stop(paste0("nOptCols not implemented for type = ", type))
    }

  }
  else if(is.list(x)){

    # npanels for accumulation
    npanels <- 0

    for(this.x in x) {

      if(tolower(type) == "spatial") {
        if(missing(nlayers)) nlayers <- length(DGVMTools::layers(this.x))
        if(missing(ntimes)) {
          ntimes <- 1
          if("Day" %in% DGVMTools::getDimInfo(this.x)) ntimes <- ntimes * DGVMTools::getDimInfo(this.x, "size")$Day
          else if("Month" %in% DGVMTools::getDimInfo(this.x)) ntimes <- ntimes * DGVMTools::getDimInfo(this.x, "size")$Month
          else if("Season" %in% DGVMTools::getDimInfo(this.x)) ntimes <- ntimes * DGVMTools::getDimInfo(this.x, "size")$Season
          if("Year" %in% DGVMTools::getDimInfo(this.x)) ntimes <- ntimes * DGVMTools::getDimInfo(this.x, "size")$Year
        }
        npanels <- npanels + (ntimes * nlayers)

      }

      else {
        stop(paste0("nOptCols not implemented for type = ", type))
      }





    }

  }

  else {
    stop(paste0("nOptCols not implemented for class = ", class(x)[1]))
  }

  opt.ncols <- 1
  while(opt.ncols * (opt.ncols + rows.more.than.cols) < npanels) {
    opt.ncols <- opt.ncols + 1
  }
  if(rows.more.than.cols < 0) opt.ncols <- opt.ncols + rows.more.than.cols

  if(opt.ncols >= 10) warning("More than 10 columns for optimal facetting, that is a lot")
  return(opt.ncols)

}



