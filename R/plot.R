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
