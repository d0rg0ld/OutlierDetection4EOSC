
#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536. 

# Doron Goldfarb (doron DOT goldfarb AT umweltbundesamt DOT at)
# Johannes Kobler (johannes DOT kobler AT umweltbundesamt DOT at)

# Environment Agency Austria, 2020

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

