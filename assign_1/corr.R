corr <- function(directory, threshold = 0) {
    id = 1:322
    numfiles <- length(id)
    corrs = array(dim=numfiles)
    for (idx in 1:numfiles){
        this  <- id[idx]
        fname <- paste(directory, "/", sprintf("%03d", this), ".csv", sep='')
        data  <- read.csv(fname)
        sulf  <- data["sulfate"][,1]
        nitr  <- data["nitrate"][,1]
        
        goods <- which( (!is.na(sulf)) & (!is.na(nitr)) ) 
        if(length(goods)>=threshold)
        {
            corrs[idx] <- cor(sulf[goods], nitr[goods])
        }
    }
    
    corrs[which(!is.na(corrs))]
    
}