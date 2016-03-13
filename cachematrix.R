## The concept behind these functions is to save time on matrix
##inversion calculations by creating a function "makeCacheMatrix"
##that store the inverse of an inputed matrix as a cached value.
##This allows the second function "cacheSolve" to pull the inverse
## from the value cached in the "makeCacheMatrix"


##This function create a matrix with a cached inverse
makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }

    get <- function() x
    setinvs <- function(inverse) invs <<- inverse
    getinvs <- function() invs
    list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}

##This function solves for the inverse of a matrix
##If the a cached value is found, the string
##"getting cached data" is printed,
##along with the value of the cached inverse.

cacheSolve <- function(x, ...) {
    invs <-x$getinvs()
    if(!is.null(invs)) {
      message("getting cached data")
      return(invs)
    }

    nocache <-x$get()
    invs <- solve(nocache, ...)
    x$setinvs(invs)
    invs
    
}
