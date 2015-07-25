## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# create input matrix and cache
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) { # cache input matrix
                x <<- y
                inver <<- NULL
        }
        get <- function() x #get input matrix
        setinverse <- function(inverse) inver <<- inverse #cache inverse matrix
        getinverse <- function() inver #get inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

# returns a matrix that is the inverse of 'x', and cache inverse
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) { # check if inverse has already been calcuated
                message("getting cached data")
                return(inver) #if inverse exist, return inverse
        }
        data <- x$get() # if inverse does not exist, calculate and return
        inver <- solve(data, ...) #calculating inverse
        x$setinverse(inver) #cache inverse
        inver
}




