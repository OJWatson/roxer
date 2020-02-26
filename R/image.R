
extract_points <- function(file, id1, id2, 
                           x0=NULL,y0=NULL,x1=NULL,y1=NULL,
                           expected_points = NULL, 
                           reg_split = "m |,| c", debug = FALSE) {
  
  
  # function to regex grab points
  p_dat <- function(x){
    lapply(strsplit(x,split = "m |,| c"),function(x){x[2:3]})
  }
  
  # Grab user point data 
  # ----------------------------------------------------------------------------
  
  # grab the values for the ids provided
  if(is.null(x0)){
  x0 <- as.numeric(readline(prompt="ID 1 x value:"))
  }
  if(is.null(y0)){
  y0 <- as.numeric(readline(prompt="ID 1 y value:"))
  }
  if(is.null(x1)){
  x1 <- as.numeric(readline(prompt="ID 2 x value:"))
  }
  if(is.null(y1)){
  y1 <- as.numeric(readline(prompt="ID 2 y value:"))
  }

  # read in the image and create the points corresponding
  svg <- xml2::as_list(xml2::read_xml(file))
  gs <- unlist(lapply(svg$svg$g,attr,"id"))
  
  p1 <- svg$svg$g[grep(id1, gs)]
  xy0 <- as.numeric(p_dat(attr(p1$g$path,"d"))[[1]])
  p2 <- svg$svg$g[grep(id2, gs)]
  xy1 <- as.numeric(p_dat(attr(p2$g$path,"d"))[[1]])
  
  # debug check if the strsplit is working as expected
  if(debug) {
    message(attr(p1$g$path,"d"))
    print(xy0)
  }
  
  # build our known data frame
  known <- data.frame("x"=c(xy0[1],xy1[1]),
                      "y"=c(xy0[2],xy1[2]),
                      "xval"=c(x0,x1),
                      "yval"=c(y0,y1))
  
  # Extract points from image
  # ----------------------------------------------------------------------------
  
  # read in the image svg by lines and create the points dataframe
  rl <- readLines(file)
  points <- p_dat(rl[grep("         d=\"m ",rl)])
  xs <- as.numeric(unlist(lapply(points,"[[",1)))
  ys <-  as.numeric(unlist(lapply(points,"[[",2)))
  
  points <- data.frame("x"=xs[which(!is.na(xs) & !is.na(ys))],
                       "y"=ys[which(!is.na(xs) & !is.na(ys))])
  
  # throw a message here if incorrect points
  if (!is.null(expected_points)){
  
    if(expected_points != nrow(points)) {
      message("Warning: ", nrow(points)," extracted rather than ", expected_points)
      message("Example strsplit:")
      message(attr(p1$g$path,"d"))
      print(xy0)
    }
    
  }
  
  # what is the distance between the known points
  xd <- known$x[2]-known$x[1]
  xst <- known$xval[2]-known$xval[1]
  yd <- known$y[2]-known$y[1]
  yst <- known$yval[2]-known$yval[1]
  
  # loop and create the points and append
  xvals <- rep(0,nrow(points))
  yvals <- rep(0,nrow(points))
  
  for(i in 1:nrow(points)){
    
    x <- points$x[i]
    y <- points$y[i]
    
    xvals[i] <- known$xval[1] + round((x - known$x[1])/xd)*xst
    yvals[i] <- known$yval[1] + round((y - known$y[1])/yd)*yst
    
  }
  
  points$xvals <- xvals
  points$yvals <- yvals
  
  # now show it for the user
  plot(xvals,yvals)
  
  return(points)
}

