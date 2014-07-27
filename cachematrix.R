## makeCacheMatrix: creates a 'Matrix' object that can cache it's inverse
## In detail:
## 1 Sets value of matrix
## 2 Gets value of matrix
## 3 Sets inverse of matrix
## 4 Gets inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve retrieves the inverse 
## from the cache.



cacheSolve <- function(x, ...) {
       m<- x$getinverse
       if(!is.null(m)) {
               message("Getting cached data...")
               return (m)
       }
       data <- solve(data)
       x$setinverse(m)
       m
        
}
