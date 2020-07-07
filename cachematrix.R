## The makeCacheMatrix function works almost exactly the same as the function
## showed as an example at the description of the Programming Assignment 2.

## The first function is subdivided into 4 functions, each one having a different
## goal. Set sets a new value to the matrix. Get returns the matrix. Setsolve
## calculates the inverse of the matrix. Getsolve returns the inverse matrix,
## which will be NULL if the second main function still wasn't called upon,
## because it is actually the second function that will calculate the inverse
## matrix.

## The cacheSolve function is called upon by the user when he wants the value
## of the inverse matrix. If it is the first time the user is calling the function,
## it will calculate the inverse matrix. If the parameters of the matrix are
## the same as the last time the function was called upon, the result will come
## from the cache memory (m).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {    ##  Sempre que se desejar atribuir novos valores
        x <<- y             ## ? matriz depois que a mesma j? foi criada,
        m <<- NULL          ## ser? atrav?s desta fun??o set
    }
    get <- function() x                     ## Retorna os valores da matriz
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting the inverse matrix from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
