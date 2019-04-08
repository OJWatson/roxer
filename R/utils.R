

## helper functions - not package related - but easiest to have here while developing
## ----------------------------------------------


# call_tree
match_call_defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}

# chracterise vector
rechar_vec <- function(what) cat(paste0("c(\"",paste0(what,collapse="\",\""),"\")"))

# itemise vector
itemise_vec <- function(what) {
  cat("##' \\itemize{\n")
  invisible(sapply(what,function(x) paste0("##'       \\item{\"",x,"\"}{ }\n") %>% cat))
  cat("##'       }")
}

# itemise data.frame of 2 attributes
itemise_df2 <- function(what) {
  paste0("##' \\itemize{\n",
         paste0(apply(what,MARGIN = 1,function(x) paste0("##'       \\item{\"",x[1],"\"}{",paste0(x[2],collapse=""),"}\n") %>% paste0(collapse="")),collapse=""),
         "##'       }")
}

# camelcase: Credit: http://www.stat.cmu.edu/~hseltman/files/camelCase.R
camelCase <- function(sv, upper=FALSE, capIsNew=FALSE, alreadyTrimmed=FALSE) {
  if (!is.character(sv)) stop("'sv' must be a string vector")
  if (!alreadyTrimmed) sv <- gsub("[[:space:]]*$", "", gsub("^[[:space:]]*", "", sv))
  if (capIsNew) {
    sv <- gsub("([A-Z])", " \\1", sv)
    sv <- gsub("^[[:space:]]", "", sv)
    sv <- tolower(sv)
  }
  apart <- strsplit(sv, split="[[:space:][:punct:]]")
  apart <- lapply(apart, tolower)
  capitalize <- function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
  if (upper) {
    apart <- lapply(apart, capitalize)
  } else {
    apart <- lapply(apart, function(x) c(x[1], capitalize(x[-1])))
  }
  return(sapply(apart, paste, collapse=""))
}


rm_all <- function() {
  rm(list = ls())
}

param_formals <- function(FUN){
  paste0("#' @param ", names(formals(FUN)), "  Default = ",formals(FUN),"\n") %>% cat
}

ranges <- function(diff = 12, end = 132){

  r <- list();
  for(i in 1:(end/diff)){

    r[[i]] <- (1 + ((i-1) * diff)) : (diff*i)

  }

  return(ranges)
}
