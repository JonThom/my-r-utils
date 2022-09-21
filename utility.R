
##################################################################################################
########################################### PACKAGES #############################################
##################################################################################################

# if install_github fails, download the file, unzip, then install from source using
# install.packages("/projects/jonatan/tools/SoupX/SoupX-master/", repos=NULL,type="source")

detach_and_unload_all_pkgs <- function() {
  #' @usage detach and unload all packages
  #' @value none
  #' # https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
  while (!is.null(names(sessionInfo()$otherPkgs))) lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
}

####################################################################################################
########################################### FILES ##################################################
####################################################################################################

dlfile <- function(url, destfile, correct_checksum) {
  #' @usage: check md5sum and download file if needed
  #' @param url: passed to download.file().
  #' @param destfile: passed to download.file().
  #' @param correct_checksum: to check file integrity
  #' @value: none

  if(!file.exists(destfile)){
    print("Downloading file")
    download.file(url, destfile, quiet = FALSE)
  } else{
    print("Verifying file integrity...")
    checksum = tools::md5sum(destfile)
    if(checksum != correct_checksum){
      print("Existing file looks corrupted or is out of date, downloading again.")
      try(download.file(url, destfile, quiet = FALSE))
    } else{
      print("Latest file already exists.")
    }
  }
}

######################################################################################################
################################################# IO #################################################
######################################################################################################

load_obj <- function(f) {
  #' @usage Loads (compressed) file from .RData, .RDS, .loom, .csv, .txt, .tab, .delim
  #' @param f: path to file
  #' @value object
  #' 
  
  stopifnot(file.exists(f))
  
  if (grepl(pattern = "\\.RDS", x = f, ignore.case = T)) {
    out <- readRDS(file=f)
  } else if (grepl(pattern="\\.Rda.*", x=f, ignore.case = T)) {
    env <- new.env()
    nm <- load(f, env)[1]
    out <- env[[nm]]
  } else if (grepl(pattern="\\.loom", x=f)) {
    require(loomR)
    out <- connect(filename=f, mode = "r+")
  } else if (grepl(pattern = "\\.csv", x=f)) {
    out <- read.csv(file= f, stringsAsFactors = F, quote="", header=T)
  } else if (grepl(pattern = "\\.tab|\\.tsv", x=f)) {
    out <- read.table(file=f, sep="\t", stringsAsFactors = F, quote="", header=T)
  } else if (grepl(pattern = "\\.txt", x=f)) {
    out <- read.delim(file=f, stringsAsFactors = F, quote="", header=T)
  }
  closeAllConnections()
  return(out)
}

######################################################################################################
########################################## profiling #################################################
######################################################################################################

n_largest_objects <- function(n) {
    sapply(ls(), function(x) object.size(eval(parse(text=x)))) %>% sort(., decreasing=T) %>% head(., n)
}

######################################################################################################
########################################## parallelising #############################################
######################################################################################################

detectCores_plus <- function(Gb_max=250,
                             additional_Gb=1) {
  #' @param Gb_max ceiling on session memory usage in Gb, assuming that each worker duplicates the session memory
  #' @param additional_Gb: max additional memory requirement for new (temporary) objects created within a parallel session
  #' @returns: max number of cores (integer)
  #' @depends parallel package
  obj_size_Gb <- as.numeric(sum(sapply(ls(envir = .GlobalEnv), function(x) object.size(x=eval(parse(text=x))))) / 1024^3)
  max(1, min(parallel::detectCores(), Gb_max %/% (obj_size_Gb + additional_Gb))-1)
}

