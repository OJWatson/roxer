contextualise <- function(workdir = "M:/OJ/MAGENTA_Results",
                          packages = c("MAGENTA"),
                          linux = TRUE,
                          package_sources = provisionr::package_sources(
                            local=getwd()),
                          new_dir = "tests",
                          use_workers=TRUE,
                          sources=NULL,
                          cluster = "fi--dideclusthn",
                          cores=NULL,
                          rtools=TRUE,
                          template=NULL,
                          initialise=TRUE){

  if (is.null(template)){
  template <- "GeneralNodes"
  }

  dir.create(paste0(workdir,"/",new_dir),showWarnings = FALSE)
  if(linux) {

    workdir <- gsub("M:/OJ","/home/oj/net/Malaria/OJ",workdir)
    if(grepl("nas", workdir)){
      home <- "//fi--didenas1/Malaria"
      local_home <- "/home/oj/net/nas"
    } else{
      home <- "//fi--didef3.dide.ic.ac.uk/Malaria"
      local_home <- "/home/oj/net/Malaria"
    }
    didehpc::didehpc_config_global(workdir=workdir,
                                   credentials="/home/oj/.smbcredentials",
                                   temp=didehpc::path_mapping("tmp",
                                                              "/home/oj/net/temp",
                                                              "//fi--didef3.dide.ic.ac.uk/tmp",
                                                              "T:"),
                                   home=didehpc::path_mapping("OJ",
                                                              local_home,
                                                              home,
                                                              "M:"),
                                   cluster = cluster)
  } else {

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
  }

  didehpc::web_login()

  root <- file.path(workdir, new_dir)
  packages.vector <- packages

  context::context_log_start()
  ## set up context
  ctx <- context::context_save(root,sources = sources,
                               packages = packages.vector,
                               package_sources = package_sources)

  if(is.null(cores)){
  config <- didehpc::didehpc_config(use_workers = use_workers, rtools = rtools,
                                    template = template)
  } else {
    config <- didehpc::didehpc_config(use_workers = use_workers, rtools = rtools,
                                      template = template,cores = cores)
  }
  config$resource$parallel <- "FALSE"
  config$resource$type <- "Cores"

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


retry <- function(dir = "GEM", root = "/home/oj/net/Malaria/OJ/MAGENTA_Results/",
                  package = "magenta", initialise = TRUE, ...){

  if(exists("obj") & exists("grp")){
    obj$task_status_dide(grp$ids)
  }
  f0 <- paste0(root,dir,"/lib/windows/3.5/",package,"/")
  f1 <- paste0(f0, c("libs","R","extdata"))
  f2 <- paste0("/home/oj/R/x86_64-pc-linux-gnu-library/3.5/",package,"/",
               c("libs","R","extdata"))


  while(any(dir.exists(f1))){
    unlink(f1,recursive = TRUE,force=TRUE)
  }
  suppressMessages(sapply(f1, dir.create))
  for(i in 1:length(f1)){
    file.copy(f2[i],f0,recursive = TRUE, overwrite = TRUE)
  }
  obj <- contextualise(new_dir = dir,packages=package,..., rtools = TRUE)


  return(obj)
}


logs <- function(grp){
  lapply(grp$tasks,function(x) {tail(capture.output(x$log()),1)})
}

cluster_result <- function(x){
  x$root$db$driver$get_object(hash = x$root$db$driver$get_hash(x$id,"task_results"))
}

grps_list <- function(obj, reg=NULL){
  nms <- obj$task_bundle_list()
  if (!is.null(reg)) {
    nms <- grep(reg, nms, value = TRUE)
  }
  grps <- lapply(nms, function(x) obj$task_bundle_get(x))
  names(grps) <- nms
  return(grps)
}

try_fail_catch <- function(expr, attempts = 3){
  r <- NULL
  attempt <- 1
  while( is.null(r) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(
      r <- eval(expr)
    )
  }

}


task_status_dide_compare_new <- function (obj, task_ids = NULL)
{
  status_check <- c("PENDING", "RUNNING")
  if (is.null(task_ids)) {
    task_ids <- obj$task_list()
  }
  st_ctx <- obj$task_status(task_ids)
  db <- obj$db
  i <- st_ctx %in% status_check
  if (!any(i)) {
    return(data.frame(id = character(0), old = character(0),
                      hpc = character(0), new = character(0), stringsAsFactors = FALSE))
  }
  task_ids <- task_ids[i]
  st_ctx <- st_ctx[i]
  message("Fetching job status from the cluster...")
  dat <- didehpc:::didehpc_jobstatus(obj$config)
  message("  ...done")

  i <- match(task_ids, dat$name)
  if (any(is.na(i))) {
    if(inherits(obj$rrq, "rrq_controller")) {
      work <-  obj$rrq$worker_task_id()
      i[is.na(i)] <- names(work)[match(task_ids[is.na(i)], work)]
      i <-  match(i, dat$name)
    }
    if (any(is.na(i))) {
      stop("Did not find information on tasks: ", paste(task_ids[is.na(i)],
                                                        collapse = ", "))
    }

  }
  st_hpc <- dat$status[i]
  st_new <- dat$status[i]
  i <- st_hpc == "COMPLETE" & st_ctx %in% status_check
  if (any(i)) {
    check <- obj$task_status(task_ids[i])
    j <- !(check %in% status_check)
    if (any(j)) {
      drop <- which(i)[j]
      task_ids <- task_ids[-drop]
      st_ctx <- st_ctx[-drop]
      st_hpc <- st_hpc[-drop]
    }
  }
  st_new[st_hpc == "COMPLETE"] <- "ERROR"
  data.frame(id = unname(task_ids), old = unname(st_ctx),
             hpc = unname(st_hpc), new = unname(st_new), stringsAsFactors = FALSE)
}


task_change <- function(task_ids, obj, old, new){


  dat <- data.frame(id = (task_ids), old = old,
             hpc = new, new = new, stringsAsFactors = FALSE)
  didehpc:::task_status_dide_update(obj, dat)
  didehpc:::task_status_dide_report(dat)

}
