## Two functions are used: "makeCacheMatrix" to create a special matrix capable of 
## caching and "cacheSolve" to provide the inverse of this special matrix. 
##
## To initalize:                my_special_matrix <- makeCacheMatrix(normal_matrix)
## To return the matrix:        my_special_matrix$get()
## To get the inverse:          cacheSolve(my_special_matrix)
## To overwrite matrix:         my_special_matrix.set(another_normal_matrix)


## This function takes a matrix as input and stores it and its inverse.
makeCacheMatrix <- function(matrix_storage = matrix()) {
        
        inverse_storage <- NULL                 ## initalizes the storage variable
        
        set <- function(y) {                    ## use to overwrite this special matrix
                matrix_storage <<- y            ## matrix stored in parent environment
                inverse_storage <<- NULL        ## stored inverse (if any) deleted
        }
        
        get <- function() {                     ## returns the special matrix
                matrix_storage                  ## available in the parent environment 
        }
        
        setinverse <- function(solution) {      ## stores the inverse in parent envir.
                inverse_storage <<- solution
        }
        
        getinverse <- function() {              ## returns the inverse
                inverse_storage
        }
         
        list( set = set, get = get,             ## returns a list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a special matrix. It tries to get the inverse
## from the storage. If there is no stored value, the inverse is calculated and stored.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()               ## tries to get stored inverse
        
        if(!is.null(inverse)) {                 ## stored inverse available
                message("getting cached data")
                
        }
        else {                                  ## no stored value: 
                matrix <- x$get()
                inverse <- solve(matrix, ...)   ## calculation of inverse
                x$setinverse(inverse)           ## storage of inverse
        }
        
        inverse                                 ## returns the inverse
}
