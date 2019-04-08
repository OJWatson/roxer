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

  usethis:::open_project(usethis::proj_get())

}
