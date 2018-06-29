pollutantmean <- function(directory, pollutant, id = 1:332){
    numfiles <- length(id)
    means <- array(dim=numfiles)
    nums  <- array(dim=numfiles)
    for (idx in 1:numfiles){
        this  <- id[idx]
        fname <- paste(directory, "/", sprintf("%03d", this), ".csv", sep='')
        data  <- read.csv(fname)
        pollute <- na.omit(data[pollutant][,1])
        means[idx] <- mean(pollute)
        nums[idx]  <- length(pollute)
    }
    # return properly weighted mean
    sum(means*nums)/sum(nums)
}