# linux replace all in package - good for changing variable names etc throughout
proj_gsub <- function(old, new) {

  cwd <- getwd()
  setwd(here::here())
  str <- paste0("grep -rli '",old,"' * | xargs -i@ sed -i 's/",old,"/",new,"/g' @")
  system(str)
  setwd(cwd)
}

# open file outside'
sopen <- function(txt_path) {

  if(Sys.info()["sysname"] == "Linux") {
    system(paste0("xdg-open ","\"",txt_path,"\""))
  } else {
    system(paste0("open ","\"",txt_path,"\""))
  }

}

# open folder outside
sdir <- function(x)  {
  if(Sys.info()["sysname"] == "Windows") {
    shell.exec(x)
  } else {
    system(paste0("xdg-open ",x))
  }
}
