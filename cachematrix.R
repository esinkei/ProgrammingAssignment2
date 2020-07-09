##  This R script has 2 functions viz. makeCacheMatrix and cacheSolve.
##makeCacheMatrix function makes a matrix object that can cache its inverse.
## cacheSolve function computes the inverse of matrix object retrieved from the
## makeCacheMatrix but if the inverse has already been cached then it returns
## the inverse from the cache.

## This function creates a special "matrix" object that can cache
##its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
            x<<-y
            inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse 
        getinverse<-function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()
       if(!is.null(inv)){
               message("gettign cached inverse")
               return(inv)
       }
       data<-x$get()
       inv<-solve(data,...)
       x$setinverse(inv)
       inv
}
##cacheSolve makes use of of the makeCacheMatrix to retrieve cached inverse if it
##exists