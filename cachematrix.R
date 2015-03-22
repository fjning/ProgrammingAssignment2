## Frances Ning / frances.ning@gmail.com
## https://github.com/fjning/


## cachematrix.R: Takes in a matrix and caches the value. It also calculates
## the inverse of a matrix and avoids re-evaluating the same matrix by 
## checking and using the cached inverse matrix. 

## Example To run: 
## > m<-matrix(c(-1,-2,1,1),2,2)
## > m<-makeCacheMatrix(m)
## > mc<-makeCacheMatrix(m)
## > mc$get()
## > inverse_m <- cacheSolve(mc)
## > inverse_m         ##  evaluates the inverse of matrix 'm' for 1st time
## > inverse_m <- cacheSolve(mc)
## > inverse_m         ##  returns the cached inverse matrix 



## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(data_matrix = matrix()) {
    
    stored_inverse <- NULL
    set <- function(y) {
        data_matrix <<- y
        stored_inverse <<- NULL
    }
    
    ## Create function get and assign a matrix to it
    get <- function() {
        return(data_matrix)
    }
    
    setInverse <- function(replacement_matrix) {
        stored_inverse <<- replacement_matrix
    }
    
    getInverse <- function() {
         stored_inverse
    }
    
    ## Lists out the values of the functions in the makeCacheMatrix frame
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}




## cacheSolve: This function calculates the inverse of a matrix but checks
## if it has been evaluated before. If so, it returns the cached inverse 
## matrix. If not, it saves it for future calls.
cacheSolve <- function(made_matrix, ...) {
    
    ## Goes to the made_matrix environment and assigns the stored_inverse
    ## value from that environment to this local_inverse variable
    local_inverse<-made_matrix$getInverse()
    
    ## If the made_matrix environment has been evaluated before, the function 
    ## prints out the value of the stored_inverse (cache inverse matrix)
    if(!is.null(local_inverse)) {
        message("getting cache data")
        local_inverse
    }
    
    ## If this made_matrix environment has never been evaluated before, 
    ## assign the made_matrix to the local_matrix variable
    local_matrix <- made_matrix$get()
    
    local_inverse <- solve(local_matrix, ...)
    
    ## Assign the calculated local_inverse to the made_matrix environment
    made_matrix$setInverse(local_inverse)
    
    local_inverse
}


#########################################################################
## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are used 
## to create a special object that stores a numeric vector and cache's 
## its mean.

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeVector <- function(data_vector = numeric()) {
    ## Initialize variable so function doesn't hang the 1st time
    stored_mean <- NULL 
    set <- function(y) {    
        data_vector <<- y
        stored_mean <<- NULL
    }
    
    ## Create func "get" in the makeVector parent and assign a vector to it
    get <- function() {  
        return (data_vector)
    }
    
    setmean <- function(replacement_mean) {  
        stored_mean <<- replacement_mean
    }
    
    getmean <- function() {
        return(stored_mean)
    }
    
    ## Lists out the values of the functions in the makeVector frame
    list(set = set, get = get,   
         setmean = setmean,
         getmean = getmean)
}



## This function calculates the mean of the special "vector" created 
## with the above function. However, it first checks to see if the 
## mean has already been calculated. If so, it gets the mean from the 
## cache and skips the computation. Otherwise, it calculates the mean 
## of the data and sets the value of the mean in the cache via the 
## setmean function.

cachemean <- function(made_vector, ...) {
    
    ## Goes to the made_vector environment and assigns the stored_mean 
    ## value from that environment to this local_mean
    local_mean <- made_vector$getmean()
    
    ## If the made_vector environment has been evaluated before, the function 
    ## prints out the value of the stored_mean (cache mean)
    if(!is.null(local_mean)) {
        message("getting cached data")
        return(local_mean)
    }
    
    ## If this made_vector environment has never been evaluated before, 
    ## assign the made_vector to the local_data variable
    local_data <- made_vector$get()
    
    local_mean <- mean(local_data, ...)
    
    ## Assign the calculated local_mean to the made_vector environment
    made_vector$setmean(local_mean)
    
    return(local_mean)
}