contextualise <- function(workdir = "M:/OJ/MAGENTA_Results",
                          packages = c("MAGENTA"),
                          package_sources = provisionr::package_sources(
                            local=getwd()),
                          new_dir = "tests",
                          use_workers=TRUE,
                          sources=NULL,
                          cluster = "fi--dideclusthn",
                          rtools=TRUE,
                          initialise=TRUE){

  dir.create(paste0(workdir,"/",new_dir),showWarnings = FALSE)
  didehpc::didehpc_config_global(workdir=workdir,
                                 credentials="C:\\Users\\Oliver\\.smbcredentials",
                                 temp=didehpc::path_mapping("tmp",
                                                            "T:",
                                                            "//fi--didef3.dide.ic.ac.uk/tmp",
                                                            "T:"),
                                 home=didehpc::path_mapping("OJ",
                                                            "M:",
                                                            "//fi--didef3.dide.ic.ac.uk/Malaria",
                                                            "M:"),
                                 cluster = cluster)
  didehpc::web_login()

  root <- file.path(workdir, new_dir)
  packages.vector <- packages

  context::context_log_start()
  ## set up context
  ctx <- context::context_save(root,sources = sources,
                               packages = packages.vector,
                               package_sources = package_sources)

  config <- didehpc::didehpc_config(use_workers = use_workers, rtools = rtools)
  obj <- didehpc::queue_didehpc(ctx, config = config,initialise = initialise)
  return(obj)
}

redrat <- function(package = "MAGENTA"){

  devtools::build(binary = TRUE,
                  pkg = paste0("C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/git/", package)
                  )
  f <- list.files(path = "C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/git/")
  fp <- grep(paste0(package,"_"),f,fixed=TRUE)
  f <- list.files(path = "C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/git/", full.names = TRUE)[fp]
  unlink(list.files(paste0("M:/OJ/",package,"/bin/windows/contrib/3.5"),full.names = TRUE),force = TRUE)
  drat::insertPackage(f,paste0("M:/OJ/",package))
  dir.create(paste0("M:/OJ/",package,"/src/contrib"),recursive = TRUE)
  file.copy(from = list.files(paste0("M:/OJ/",package,"/bin/windows/contrib/3.5/"),full.names = TRUE),
            to=paste0("M:/OJ/",package,"/src/contrib"))

}


retry <- function(dir = "GEM",  initialise = TRUE, ...){

  if(exists("obj") & exists("grp")){
    obj$task_status_dide(grp$ids)
  }
  f0 <- paste0("M:/OJ/MAGENTA_Results/",dir,"/lib/windows/3.5/MAGENTA/")
  f1 <- paste0(f0, c("libs","R","extdata"))
  f2 <- paste0("C:/Program Files/R/R-3.5.0/library/MAGENTA/",
               c("libs","R","extdata"))


  while(any(dir.exists(f1))){
    unlink(f1,recursive = TRUE,force=TRUE)
  }
  suppressMessages(sapply(f1, dir.create))
  for(i in 1:length(f1)){
    file.copy(f2[i],f0,recursive = TRUE, overwrite = TRUE)
  }
  obj <- contextualise(new_dir = dir,use_workers = FALSE,cluster="fi--didemrchnb", rtools = TRUE)


  return(obj)
}


logs <- function(grp){
  lapply(grp$tasks,function(x) {tail(capture.output(x$log()),1)})
}

cluster_result <- function(x){
  x$root$db$driver$get_object(hash = x$root$db$driver$get_hash(x$id,"task_results"))
}
