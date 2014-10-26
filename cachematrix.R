## Below are two functions that are used to create a
## special object that stores a numeric matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x # define functions to ... get the matrix
        setinv <- function(solve) inv <<- solve # ... set the inverse
        getinv <- function() inv # ... get the inverse
        list(set = set, get = get, # return the list of functions
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() # retrieve inverse (or NULL) from special object x
        if(!is.null(inv)) { # is TRUE if the inverse has already been computed
                message("getting cached data") # print message
                return(inv) # `return` exits the function without computation
        }
        data <- x$get() # get the matrix from the special object
        inv <- solve(data, ...) # compute the inverse of the matrix
        x$setinv(inv) # store the inverse in the special object
        inv # return the inverse of the matrix
}



## The following code shows

# - how to use the two functions, and
# - illustrates the changes in the special matrix object

set.seed(1) # set random seed

# create 3x3 matrix with random draws from standard normal distribution
mat <- makeCacheMatrix() # create the special object
mat$set(matrix(rnorm(3*3), 3, 3)) # assign the matrix 
mat$get() # show values
mat$getinv() # still empty since the inverse matrix has not yet been cached 

inv <- cacheSolve(mat) # calculate the inverse
inv # show inverse matrix
mat$getinv() # no longer empty since the inverse matrix has been cached 

# (matrix-)multiplying a matrix with its inverse gives the identity matrix
# (http://en.wikipedia.org/wiki/Invertible_matrix
round(mat$get() %*% inv, 2)
# rounding makes it easier to see that it indeed is the identity matrix
# (up to some numerical precision)

inv2 <- cacheSolve(mat) # cacheSolve now retrieves the cached value
inv2 # show inverse matrix
round(mat$get() %*% inv2, 2) # gives also the inverse

identical(inv, inv2) # the inverses of the matrix are identical