## makeCacheMatrix creates an object which contains a data matrix and its inverse
## cacheSolve returns an inverse matrix inside makeCacheMatrix object or
## caches its value if its not yet cached

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #Define set function 
    #m = inverse
    #x = data matrix
    m <- NULL #Inverse
    set <- function(y) { 
        x <<- y
        m <<- NULL
    }
    #get data matrix
    get <- function() x 
    #set inverse matrix to setinv var
    setinv <- function(xinv) m <<- xinv #recebe o valor de uma media e passa p m
    #get inverse matrix 
    getinv <- function() m #pega o valor da inversa
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #Check if there is already a inverse matrix calculated.
    #It there is, it will simply return the matrix
    #If not, it will read the data matrix and calculate
    #the inverse. 
    #Then using the method setinv it will pass the inverse to m and return
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    
}
