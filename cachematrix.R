## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    ## get the value of the matrix
    get<-function() x
    ## set the inverse of the matrix
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    
    ## get the inverse of the matrix
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## get the inverse of the matrix  
    m<-x$getmatrix()
    
    ## check if the inverse is already been calculated
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## if not: get the inverse 
    matrix<-x$get()
    m<-solve(matrix, ...)
    ## set the inverse
    x$setmatrix(m)
    m
}
