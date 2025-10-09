rmse <- function(x,y){
    return(sqrt(mean((x-y)^2,na.rm=TRUE)))
}
rrmse <- function(x,y){
    return(rmse(x,y)/mean(x,na.rm=TRUE))
}
rval <- function(x,y){
    which_na <- is.na(x) | is.na(y)
    x <- x[!which_na]
    y <- y[!which_na]
    return(cor(x,y))
}

performance_row <- function(x,y){
    return(c(RMSE=rmse(x,y),RRMSE=rrmse(x,y),R=rval(x,y)))
}

