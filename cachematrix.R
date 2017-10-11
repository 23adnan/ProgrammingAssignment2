## cacheing the inverse of a matrix

## this fuction will cache the special matrix and inverse of a matrix
## and will return the following fonctions a list:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## setting the chache inverse variable to NULL
        set <- function(y) { ## function to set the a matrix to 'x'.can be specially called 
                x <<- y
                m <<- NULL
        }
        get <- function() x ## function to get value of the cached matrix
        setinverse <- function(inverse) inv <<- inverse ## function to directly set the inverse of a matrix
        getinverse <- function() inv ## function to get the cached inverse of a matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # returns the list of functions that are defined in this function
}


## this function will solve a matrix for its inverse if it doesnot cached already

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## gets the cached inverse of the matix 'x'
        if(!is.null(inv)) { ## checks if the cached inverse it NULL,if false returns the cached inverse
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## gets the cached matrix
        inv <- solve(data, ...) ## solves the matrix for its inverse
        x$setinverse(inv) ## cache the inverse along with cache matrix 
        inv ## returns the inverse
}
