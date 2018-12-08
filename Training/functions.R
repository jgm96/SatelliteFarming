#############################################################################
#############################################################################
#' Creates directories for storing data in the main execution, only if they 
#' do not exist.
#'                   
#'@param parent A string with the parent path of every directory. 
#'                 
#'@param listOfMethods A list of strings with every method used.
#'
##############################################################################

cat("======================================================\n")
cat("CREATING DIRECTORIES FOR OUTPUT DATA\n")
cat("======================================================\n")

#Parent directory
createDirectories <- function(parent, listOfMethods){
  if(dir.exists(parent)){
    message(paste0("Directory \"", parent, "\" already exists."))
  } else {
    cat("Creating directory \"", parent, "\" ...", "\n", sep = "")
    dir.create(parent, showWarnings = FALSE)
    cat("Done.", "\n")
  }
  
  #Child directories
  for(selectedMethod in listOfMethods){
    newDir <- paste0(parent, "/", selectedMethod)
    if(dir.exists(newDir)){
      message(paste0("Directory \"", newDir, "\" already exists."))  
    }
    else{
      cat("Creating directory \"", newDir, "\" ...", "\n", sep = "")
      dir.create(newDir, showWarnings = FALSE)
      cat("Done.", "\n")
    }
  }
}
