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
