detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
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

srvs = c("http://healthstat.snu.ac.kr/CRAN",
	"http://cran.yu.ac.kr",
	"http://cran.biodisk.org")
set.seed(round((as.numeric(Sys.time())*10000)%%10000))
repo = sample(srvs,1)
specpkg = list(
  "ggfortify" = "0.4.1",
  "data.table" = "1.10.4-3"
)
req = c("Rserve", "caTools", "digest", "R2HTML", "markdown", "moments", "mice", "VIM", "randomForest", "car", "AICcmodavg", "agricolae", "MASS", "testit", "plyr", "philentropy", "ade4", "DescTools", "vcd", "oii", "coin", "vcdExtra", "cluster", "fpc", "dbscan", "tree", "party", "rpart", "caret", "partykit", "e1071", "klaR", "psy", "psych", "GPArotation", "FactoMineR", "devtools", "factoextra", "forecast", "ggplot2", "ggfortify", "rugarch", "fishmethods", "survival", "KMsurv", "rms", "plsdepot", "AER", "pscl", "ResourceSelection", "MKmisc", "lmtest", "VGAM", "lme4", "leaps", "scatterplot3d", "flashClust", "withr", "ggExtra", "GGally", "expm", "manipulate", "Matrix", "biotools", "crayon", "ROCR", "exactRankTests", "lsr", "discSurv", "moonBook", "ggrepel", "cowplot", "ggpubr", "rootSolve", "PASWR", "popbio", "fitdistrplus", "tidyselect", "epiR", "pROC", "meta", "metafor", "metasens", "mada", "lavaan", "semTools", "semPlot", "glmnet", "plotmo", "qcc")

defpkg = c("R", "stats", "graphics", "methods", "utils", "grDevices", "grid", "stats4", "splines", "survival", "lattice", "MASS", "tools", "parallel", "datasets", "rJava", "JavaGD", "JGR", "Deducer", "base", "openssl", "tcltk")

rrepo = options("repos")
rrepo.orig = rrepo
rrepo["CRAN"] = repo
options(repos=rrepo)
rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')

proc = function(path) {
	ccat("[", path, "] start\n")
	
	fn = paste0(path, "/PACKAGES")
	pkg = sapply(strsplit(paste(readLines(fn), collapse="\n"), "\n\n")[[1]], function(v) {
			tmp = lapply(strsplit(gsub("/\n[ ]+/", " ", v), "\n")[[1]], function(vv) {
				strsplit(vv, ": ")[[1]]
			})
			ret = sapply(tmp, function(v)v[2])
			names(ret) = sapply(tmp, function(v)v[1])
			ret
		})
	names(pkg) = sapply(pkg, function(v)v["Package"])
	
	#ccat("[", length(pkg), "] packages found\n")
	list(
	  pkg = pkg,
	  pkgs2inst = findDependency(pkg, path)
	)
}
allsz = 0
findDependency = function(pkg, path) {
	L = 1
	pkgs <<- c()
	curreq <<- req
	ret <- list()
	while (length(curreq)) {
		ret[[L]] <- curreq
		ccat("Required packages [LEVEL ", L, ",", length(curreq), " packages] : ", paste(curreq, collapse=", "), "\n")
		L = L+1
		
		dep = unique(unlist(lapply(curreq, function(v) {
		   #ccat("Check", v, is.null(pkgs[v]), "\n")
			if (is.null(pkg[[v]]) && is.null(specpkg[[v]])) {
			  ccat("Package [", v, "] not found in CRAN\n")
			  stop("The current R version [", rversion, "] does not satisfy the minimum condition of REx installation")
			} else if (is.null(pkgs[v]) || is.na(pkgs[v])) {
			    szret = file.info(paste0(path,"/",dir(path, pattern=paste0("^",v,"\\_"))))
				#print(szret)
				allsz = allsz + szret$size
				tmp <- pkgs
				tmp[v] <- TRUE
				pkgs <<- tmp
				
				if (is.null(pkg[[v]]['Depends'])) pkg[[v]]['Depends'] = ""
				if (is.null(pkg[[v]]['Imports'])) pkg[[v]]['Imports'] = ""
				
				tmp1 = trim(strsplit(paste(trim(pkg[[v]]['Depends']), ",", trim(pkg[[v]]['Imports'])), ",")[[1]])
				tmp = sapply(tmp1, function(vv) {
					strsplit(trim(vv), "[ \\(]")[[1]][1]
				})
				
				return( tmp[is.na(match(tmp, names(pkgs)))] )
			} else NULL
		})))
		if (!is.null(dep))
		  curreq <<- dep[(nchar(dep) > 0) & is.na(match(dep, defpkg)) & (dep != "NA") & !is.na(dep)]
		else
		  curreq <<- c()
	}
	ret
}
is.package.exists <- function(v) {
  libp <- .libPaths()
  for (j in libp) {
    path <- paste0(j, "/", v, "/NAMESPACE")
	if (file.exists(path)) return(TRUE)
  }
  return(FALSE)
}

ccat("\n !!! START installing R packages required for REx !!!\n\n")
ccat(" R version: ", rversion, "\n")
ccat(" Use repository [", repo, "]\n\n")

arch <- R.version$arch
if (arch != "x86_64" && arch != "i386") {
  stop("REx does not support this architecture [", arch, "], please report this to the REx team")
} else if (arch == "x86_64") arch <- "x64"

pkgs2inst <- NULL
ret <- proc(paste0(repo, "/bin/windows/contrib/", rversion))
pkgs2inst <- ret$pkgs2inst
pkg <- ret$pkg
options(warn=-1)
if (!is.null(pkgs2inst)) for (i in length(pkgs2inst):1) {
  sapply(pkgs2inst[[i]], function(v) {
	if (is.package.exists(v) == FALSE) {
	  ccat("[INST] Package [", v, "]\n")

	  install.packages(v, repo=repo)
	  if (is.package.exists(v) == FALSE) {
	    if (!is.null(specpkg[[v]])) {
		  ver <- specpkg[[v]]
		  crepo <- "http://rexsoft.org/alt"
		} else {
	      ver <- pkg[[v]]['Version']
		  crepo <- repo
		}
        localfile <- paste0(tempdir(),"\\", v, "_", ver, ".zip")
        download.file(paste0(crepo, "/bin/windows/contrib/", rversion, "/", v, "_", ver, ".zip"), localfile, mode="wb")
	    install.packages(localfile, repos=NULL, type="win.binary")
	    if (is.package.exists(v) == FALSE) {
	      stop("Package [", v, "] install failed")
		}
      }
	}# else ccat("[SKIP] Package [", v, "]\n")
  })  
}
options(warn=0)

# For VSTO installation error
path <- paste0(Sys.getenv("CommonProgramFiles"),"\\microsoft shared\\VSTO") 
tmp <- dir(path) 
tgt <- tmp[file.info(paste0(path,"\\",tmp))$isdir] 
tgt.files <- paste0(path,"\\",tgt,"\\VSTOInstaller.exe.config") 
suppressWarnings(file.remove(tgt.files[!is.na(file.info(tgt.files)$size)]))

ccat("\n\n !!! Find updates of installed R packages... !!!\n\n")
update.packages(repos="http://healthstat.snu.ac.kr/CRAN",ask=FALSE)

ccat("\n\n !!! Installing R packages required for REx was COMPLETED !!!\n\n")

options(repos=rrepo.orig)
rm(list=ls())
