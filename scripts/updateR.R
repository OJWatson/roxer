installr::updateR()
install.packages("devtools")
install.packages("digest")
install.packages("roxygen2")
install.packages("stringi")
install.packages("Rcpp")
install.packages("commonmark")
update.packages()
install.packages("xml2")
install.packages("jsonlite")
install.packages("rappdirs")
install.packages("sp")
install.packages("rgdal")
install.packages("tibble")
install.packages("rlang")
install.packages("haven")
install.packages("iotools")
install.packages("curl")
install.packages("testhat") # not good on 3.5

ip <- installed.packages()
l <- loadedNamespaces()
p <- rownames(ip)
p[-which(p %in% l)]
install.packages(p[-which(p %in% l)])