#env:
#  APP_NAME: ${{ secrets.APP_NAME }}
#run: Rscript --verbose ./config.R


if (file.exists(".Renviron")) {
    
    readRenviron(".Renviron")
    cat("Env file loaded.\n")
}

profile_lock <- "./renv/profiles/UBCleanDash/renv.lock"
root_lock    <- "renv.lock"

if (file.exists(profile_lock)) {
    message("Copying profile lockfile to root for deployment...")
    
    file.copy(from = profile_lock, to = root_lock, overwrite = TRUE)
    
} else {
    warning("Profile lockfile not found! Using existing renv.lock if available.")
}


if(!require("rsconnect",quietly = T)) install.packages("rsconnect")


account_name <- Sys.getenv("ACNT_NAME")
account_token <- Sys.getenv("ACNT_TOKEN")
account_secret <- Sys.getenv("ACNT_SECRET")

if (account_name == "" || account_token == "" || account_secret == "") {
    stop("Error: Credentials missing in .Renviron")
}
    
rsconnect::setAccountInfo(name = account_name, 
                          token = account_token, 
                          secret = account_secret)


cat("Account info set.\n")


rsconnect::deployApp(appDir = getwd(),appName = "UBCleanDash",
                     forceUpdate = TRUE)
