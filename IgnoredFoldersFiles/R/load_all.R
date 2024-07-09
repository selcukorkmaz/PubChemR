load_all <- function(path, ...){
  fileList <- dir(path)
  for (f in fileList){
    source(file.path(path, f))
  }
}

load_all("./R/")
