## function makeCacheMatrix creats a special "vector", which is a 
## list containing a function to 
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get, 
         setMatrix = setMatrix, 
         getMatrix = getMatrix)
}


## cacheSolve will compute the inverse matrix of the special "vector" created
## with the above function. However, it first checks to see if the inverse 
## matrix has already been computed. If so, it gets the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse matrix
## of the given matrix and sets the value of the inverse matrix in the cache via
## the setMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix)
    x$setMatrix(matrix)
    m
}
