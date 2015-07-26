## Put comments here that give an overall description of what your
## functions that cache the inverse of a matrix


## This function creates a matrix object that cache its inverse

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        
        set <- function(y){
                matrix <<- y
                inv <<- NULL
        }
        
        get <- function(){
                matrix
        }
        
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        getInverse <- function() {
                inv
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by "makeCacheMatrix" function. 
## If the inverse is already calculated and there are no changes to the matrix, the "cachesolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()

         m <- solve(data) %*% data

        x$setInverse(m)
        
        m 
}
