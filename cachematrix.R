

## makeCacheMatrix function returns a special vector containing
## fuctions to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve function returns the inverse of a matrix by taking 
## special vector from above function as input and checking whether
## it is already present

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        
        i <- solve(data,...)
        
        x$setinv(i)
        i
        
}
