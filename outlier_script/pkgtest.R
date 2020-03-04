#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.  

pkgTest <- function(x, mode="regular", extra="")
{
  if (!require(x,character.only = TRUE))
        if(mode == "regular") {
            install.packages(x,dep=TRUE)
            if(!require(x,character.only = TRUE)) stop("Package not found")
        } else {
                if (mode=="github") {
                        pkgTest("remotes")
                        install_github(x, extra)
                }
        }
}

