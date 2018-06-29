complete <- function(directory, id=1:332) {
    numfiles <- length(id)
    nobs = array(dim=numfiles)
    for (idx in 1:numfiles){
        this  <- id[idx]
        fname <- paste(directory, "/", sprintf("%03d", this), ".csv", sep='')
        data  <- read.csv(fname)
        sulf  <- data["sulfate"][,1]
        nitr  <- data["nitrate"][,1]
        nobs[idx] <- length( which((!is.na(sulf)) & (!is.na(nitr))) ) 
    }
    
    data.frame(id, nobs)
}