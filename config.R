#env:
#  APP_NAME: ${{ secrets.APP_NAME }}
#run: Rscript --verbose ./config.R

if (file.exists(".Renviron")) {
    
    readRenviron(".Renviron")
    
}


if(!require("rsconnect",quietly = T)) {
    
    install.packages("rsconnect")
    rsconnect::setAccountInfo(name=Sys.getenv("ACNT_NAME"), 
                              token=Sys.getenv("ACNT_TOKEN"),
                              secret=Sys.getenv("ACNT_SECRET"))
    
    
}else{
    
    rsconnect::setAccountInfo(name=Sys.getenv("ACNT_NAME"), 
                              token=Sys.getenv("ACNT_TOKEN"),
                              secret=Sys.getenv("ACNT_SECRET"))
    
}

rsconnect::deployApp(appDir = getwd(),appName = "UBCleanDash",
                     forceUpdate = TRUE)
