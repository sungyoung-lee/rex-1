detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE, quitely = TRUE)
  }
}
is.package.exists <- function(v) {
  libp <- .libPaths()
  for (j in libp) {
    path <- paste0(j, "/", v, "/NAMESPACE")
	if (file.exists(path)) return(TRUE)
  }
  return(FALSE)
}
ccat <- function(...) {
  cat(...)
  flush.console()
}

adcode <- suppressWarnings(system("fltmc",show.output.on.console=FALSE))
if (adcode == 0) {
} else if (adcode == 1) {
  ccat("\n\n !!! ERROR : Installing R packages required for REx requires administrator privilege !!!\n\n")
  stop()
} else {
  ccat("\n\n !!! ERROR : REx does not support this operating system !!!\n\n")
  stop()
}

# ETS
req = c("Rserve", "caTools", "digest", "R2HTML", "markdown", "moments", "mice", "VIM", "randomForest", "car", "AICcmodavg", "agricolae", "MASS", "testit", "plyr", "philentropy", "ade4", "DescTools", "vcd", "oii", "coin", "vcdExtra", "cluster", "fpc", "dbscan", "tree", "party", "rpart", "caret", "partykit", "e1071", "klaR", "psy", "psych", "GPArotation", "FactoMineR", "devtools", "factoextra", "forecast", "ggplot2", "ggfortify", "rugarch", "fishmethods", "survival", "KMsurv", "rms", "plsdepot", "AER", "pscl", "ResourceSelection", "MKmisc", "lmtest", "VGAM", "lme4", "leaps", "scatterplot3d", "flashClust", "withr", "ggExtra", "GGally", "expm", "manipulate", "Matrix", "biotools", "discSurv", "moonBook", "ggrepel", "cowplot", "ggpubr", "rootSolve", "PASWR", "popbio", "fitdistrplus")
sapply(req, function(v) {
  if (is.package.exists(v) == FALSE) {
    ccat("[INST] Package [", v, "]\n")

    install.packages(v)
    tmp <- try(library(v, quietly=TRUE, character.only=TRUE), T)
    if (is.package.exists(v) == FALSE) {
	  stop("Package [", v, "] install failed")
	}
    detach_package(v)
  }# else ccat("[SKIP] Package [", v, "]\n")
})  

ccat("\n !!! Installing R packages required for REx was COMPLETED !!!\n\n")
