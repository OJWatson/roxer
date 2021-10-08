patch_column <- function(figs){
  l <- length(figs)
  ev <- rep(0,l)
  for(i in 1:l){
  ev[i] <- paste0("figs[[",i,"]]")
  }
 eval(parse(text=paste0(ev,collapse="/") ))
}

patch_columns <- function(figs1,figs2){
  l <- length(figs1)
  ev <- rep(0,l)
  for(i in 1:l){
    ev[i] <- paste0("(figs1[[",i,"]] | figs2[[",i,"]])")
  }
  eval(parse(text=paste0(ev,collapse="+") ))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

map_plot <- function(df, lat= "Lat",long="Long", type = "point") {

  coords2country = function(points)
  {
    countriesSP <- rworldmap::getMap(resolution='low')

    #setting CRS directly to that from rworldmap
    pointsSP = sp::SpatialPoints(points, proj4string=sp::CRS(sp::proj4string(countriesSP)))

    # use 'over' to get indices of the Polygons object containing each point
    indices = sp::over(pointsSP, countriesSP)

    # return the ADMIN names of each country
    indices$GEOUNIT
    #indices$ISO3 # returns the ISO3 code
    #indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
  }

  neighbours <- function(country){

    countriesSP <- rworldmap::getMap(resolution='low')
    excess <- subset(countriesSP, !GEOUNIT %in% as.character(country))
    conf <- subset(countriesSP, GEOUNIT %in% as.character(country))
    tst <- logical(nrow(excess))
    for (i in 1:nrow(excess)) {

      tst[i] <- any(vapply(seq_along(conf), function(x) {
        rgeos::gTouches(conf[x,], excess[i,])
      }, logical(1)))
    }
    return(as.character(excess$GEOUNIT[tst]))
  }

  df$lat <- df[[lat]]
  df$long <- df[[long]]

  if(!("color" %in% names(df))) {
    df$color <- 1
  }

  points <- df[,c("long","lat")]
  points <- na.omit(points)
  points <- as.data.frame(points)

  country <- as.character(unique(coords2country(points)))
  neighbs <- neighbours(na.omit(country))

  ## weird overlap errors
  if("Ethiopia" %in% country) {
    neighbs <- c(neighbs, "South Sudan", "Sudan")
  }

  country_sp <- rworldmap::getMap(resolution='low')
  country_sp <- country_sp[country_sp$NAME %in% na.omit(country),]
  r <- as.numeric(apply(dplyr::select(ggplot2::fortify(country_sp),long,lat),2,range))
  regions <- c(country,neighbs)

  match_errors <- function(x){

    missing <- c("Aland","Ashmore and Cartier Islands","Antigua and Barbuda",
                 "The Bahamas","Republic of the Congo","Northern Cyprus",
                 "Federated States of Micronesia","Gaza","United Kingdom",
                 "Guinea Bissau","Hong Kong S.A.R.","Heard Island and McDonald Islands",
                 "Indian Ocean Territories","British Indian Ocean Territory",
                 "Saint Kitts and Nevis","Macau S.A.R",
                 "South Georgia and South Sandwich Islands","Somaliland",
                 "Republic of Serbia","East Timor","Trinidad and Tobago",
                 "United States of America","Saint Vincent and the Grenadines",
                 "British Virgin Islands","United States Virgin Islands",
                 "West Bank","Tuvalu")

    found <- rep("",length(missing))
    found[5] <- "Republic of Congo"
    found[9] <- "UK"
    found[10] <- "Guinea-Bissau"
    found[18] <- "Somalia"
    found[20] <- "Timor-Leste"
    found[22] <- "USA"

    x[which(x %in% missing)] <- found[match(x[which(x %in% missing)],missing)]
    return(x)
  }
  regions <- match_errors(regions)

  if(length(regions) > 50) {
    regions <- NULL
  }


  map <- ggplot2::ggplot(df, ggplot2::aes(x=long,y=lat)) +
    ggplot2::borders(fill="#F0F0F0", regions = regions) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed(xlim = c(r[1],r[2]),
                ylim = c(r[3],r[4])) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill="#F0F0F0"),
          panel.border = ggplot2::element_blank())

  if(type == "point") {
  map <- map + ggplot2::geom_point(alpha=0.6, ggplot2::aes(color = color))
  } else {
  map <- map + ggplot2::geom_text(alpha=0.6, ggplot2::aes(label = color))
  }

  print(map)
  return(map)

}


save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      root = "analysis/manuscript/edits_hoseah/comments/latest/last/lastest/latest_latest/revisions/figures") {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  svg(filename = fig_path(paste0(name,".svg")), width = width, height = height)
  print(fig)
  dev.off()

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()


}


# Facet <- ggproto(
#   ...
#   init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
#     scales <- list()
#     if (!is.null(x_scale)) {
#       scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
#     }
#     if (!is.null(y_scale)) {
#       scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
#     }
#     scales
#   },
#   ...
# )
#
# scale_override <- function(which, scale) {
#   if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
#     stop("which must be an integer of length 1")
#   }
#
#   if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
#     stop("scale must be an x or y position scale")
#   }
#
#   structure(list(which = which, scale = scale), class = "scale_override")
# }
#
# CustomFacetWrap <- ggproto(
#   "CustomFacetWrap", FacetWrap,
#   init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
#     # make the initial x, y scales list
#     scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
#
#     if(is.null(params$scale_overrides)) return(scales)
#
#     max_scale_x <- length(scales$x)
#     max_scale_y <- length(scales$y)
#
#     # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
#     for(scale_override in params$scale_overrides) {
#       which <- scale_override$which
#       scale <- scale_override$scale
#
#       if("x" %in% scale$aesthetics) {
#         if(!is.null(scales$x)) {
#           if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
#           scales$x[[which]] <- scale$clone()
#         }
#       } else if("y" %in% scale$aesthetics) {
#         if(!is.null(scales$y)) {
#           if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
#           scales$y[[which]] <- scale$clone()
#         }
#       } else {
#         stop("Invalid scale")
#       }
#     }
#
#     # return scales
#     scales
#   }
# )
#
# CustomFacetGrid <- ggproto(
#   "CustomFacetGrid", FacetGrid,
#   init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
#     # make the initial x, y scales list
#     scales <- ggproto_parent(FacetGrid, self)$init_scales(layout, x_scale, y_scale, params)
#
#     if(is.null(params$scale_overrides)) return(scales)
#
#     max_scale_x <- length(scales$x)
#     max_scale_y <- length(scales$y)
#
#     # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
#     for(scale_override in params$scale_overrides) {
#       which <- scale_override$which
#       scale <- scale_override$scale
#
#       if("x" %in% scale$aesthetics) {
#         if(!is.null(scales$x)) {
#           if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
#           scales$x[[which]] <- scale$clone()
#         }
#       } else if("y" %in% scale$aesthetics) {
#         if(!is.null(scales$y)) {
#           if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
#           scales$y[[which]] <- scale$clone()
#         }
#       } else {
#         stop("Invalid scale")
#       }
#     }
#
#     # return scales
#     scales
#   }
# )
#
# facet_wrap_custom <- function(..., scale_overrides = NULL) {
#   # take advantage of the sanitizing that happens in facet_wrap
#   facet_super <- facet_wrap(...)
#
#   # sanitize scale overrides
#   if(inherits(scale_overrides, "scale_override")) {
#     scale_overrides <- list(scale_overrides)
#   } else if(!is.list(scale_overrides) ||
#             !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
#     stop("scale_overrides must be a scale_override object or a list of scale_override objects")
#   }
#
#   facet_super$params$scale_overrides <- scale_overrides
#
#   ggproto(NULL, CustomFacetWrap,
#           shrink = facet_super$shrink,
#           params = facet_super$params
#   )
# }
#
# facet_grid_custom <- function(..., scale_overrides = NULL) {
#   # take advantage of the sanitizing that happens in facet_wrap
#   facet_super <- facet_grid(...)
#
#   # sanitize scale overrides
#   if(inherits(scale_overrides, "scale_override")) {
#     scale_overrides <- list(scale_overrides)
#   } else if(!is.list(scale_overrides) ||
#             !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
#     stop("scale_overrides must be a scale_override object or a list of scale_override objects")
#   }
#
#   facet_super$params$scale_overrides <- scale_overrides
#
#   ggproto(NULL, CustomFacetGrid,
#           shrink = facet_super$shrink,
#           params = facet_super$params
#   )
# }
