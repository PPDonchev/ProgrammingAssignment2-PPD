## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following code is part of Coursera`s Programming Asssignment 2: Lexical Scoping
##
## Computing the inverse of large square matrices is usually costly and takes time and resourceses especially when it has to be
## repeated in a loop. If we assume that the matrix supplied is always invertible and in the same time its content doesn`t change
## it is rather practicle to cach the matrix inverse in order to be used when needed again. 
##
## The objective in this Programming Assignment is to take advantave of the scoping rules of the R language and secondly to learn how to 
## complete the assignment while working in GitHub environment. 

## For this assignment <<- operator will be used to assign a value to an object in an environment that is different from the current environment. 
## The code contains two functions that will be used to create a special object that stores a square matrix and caches its inverse.
## The first function is makeCacheMatrix() which creates a special matrix that performs the following tasks:
##
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. set the elements of the inverse matrix
## 4. get the elements of the inverse matrix
##
## The second function is called casheSolve() and its role is to compute the inverse of the special matrix, returned by makeCacheMatrix(). In case
## the inverse has been already calculated and no changes to the matrix have been introduced this function will retrieve the inverse from the cache.
## Setting up the matrix

makeCacheMatrix <- function(x = matrix()) {
         # x is initialized as a function argument and no further initialization is required within the function
         # setting x to NULL means it is defined as an object within makeCacheMatrix environment to be used later

         invMatrix <- NULL

         set <- function(y) {
        
         # The 'superassignment' operator <<- does the assignemnt in the enclosing environment 
         # starting with the enclosing frame and works its way up 
         # towards the global environment until it finds a variable
 
              x <<- y 
       
              invMatrix <<- NULL
         }
     
         get <- function() {
   
             x ## return special matrix
         }

         setInvMatrix <- function(inverse) invMatrix <<- inverse ##function that returns invers matrix, assigned to a variable

         getInvMatrix <- function() invMatrix ##calling the inverse matrix from the variable
 
         list(set = set, ## creats new object by returning a list. This section assigns each of the functions above as an element within a list() and returns it to the parent environment
              get = get,
              setInvMatrix = setInvMatrix,
              getInvMatrix = getInvMatrix)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' generated in the previous step by makeCacheMatrix()
        
        invMatrix <- x$getInvMatrix()
         
        if(!is.null(invMatrix)){  ## verifies if the inverse has already been calculated
           if(identical(x$get() %*% invMatrix, invMatrix %*% x$get())){  ## checks if the return invM is identical
              
               message("getting cached data")  ##skips computation and gets inverse matrix from cache
               
               return(invMatrix)
        }
    }
        data <- x$get()
        invMatrix <- solve(data)
        x$setInvMatrix(invMatrix)
       message("getting new computated data")
       invMatrix

}
