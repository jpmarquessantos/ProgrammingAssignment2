## This code calculates the inverse of a given matrix. The result is cached in order to be
## immediately available, saving computing time. It consists of two functions.
## The first sets the environment and the structure. The second tests if the inverse was
## already calculated and, if not, calculates it.

## This function defines the environment for the inverse and holds four other functions that:
## set and get the matrix, and set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                         ## Initialize the inverse matrix and flags it
    set_matrix <- function(mat1) {          ## Set the matrix
        x <<- mat1
        inverse <<- NULL
    }
    get_matrix <- function() {              ## Get the matrix
        x
    }
    set_inverse <- function(inv1) {         ## Set the inverse
        inverse <<- inv1
    }
    get_inverse <- function() {             ## Get the inverse
        inverse
    }
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}

## Returns a matrix that is the inverse of 'x'. However, it calculates the inverse only once.
## In the second and further atempts returns the cached inverse matrix.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {                 ## Tests if the inverse matrix was already calculated
        message("getting cached data")      ##    yes; return the cached inverse
        return(inverse)
    }
    mat2 <- x$get_matrix()                  ##    no; calculates the inverse
    inverse <- solve(mat2)
    x$set_inverse(inverse)
    inverse
}
