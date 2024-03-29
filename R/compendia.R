minimial_compendia <- function(path){

  usethis::create_project(path = path,rstudio = TRUE)

  dir.create(file.path(path,"analysis"))

  dir.create(file.path(path,"analysis/data/raw"), recursive = TRUE)
  dir.create(file.path(path,"analysis/data/derived"), recursive = TRUE)

  file.create(file.path(path,"analysis/00_data_input.R"))
  file.create(file.path(path,"analysis/01_data_analysis.R"))

  file.copy(system.file("extdata/template.Rmd",package = "roxer"),
            file.path(path,"analysis/99_report.Rmd"))

  lines <- readLines(file.path(path,"analysis/99_report.Rmd"))
  lines[2] <- paste0("title: \"",path,"\"")
  writeLines(lines,file.path(path,"analysis/99_report.Rmd"))

  usethis::use_git()

  p <- usethis::proj_get()
  system(paste("xdg-open",paste0(a,"/",basename(a),".Rproj")))

}




pckgs_in_dir <- function(dir) {

  fs <- list.files(dir, full.names = TRUE)
  fs <- grep("\\.R$", fs, value = TRUE)
  pckgs <- c()
  for(f in fs) {

    file_lines <- readLines(con = f)

    ps <- lapply(file_lines, function(l) {
      if(grepl("::", l)){
        gsub(".*?([[:alnum:]\\.]+)::.*","\\1", l)
      } else {
        return(NULL)
      }

    })

    pckgs <- c(pckgs, unique(unlist(ps)))

  }

  return(unique(pckgs))

}
