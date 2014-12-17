## The following lines of code are two functions which are used to create special object to store matrix and
## Cache's its inverse 

## The first function 'makeCacheMatrix' creates a special matrix,
## which is actually a list a containing functions
## 1.set_matrix :- set the value of the matrix
## 2.get_matrix :- get the value of the matrix
## 3.set_inverse :- set the value of the inverse of matrix
## 4.get_inverse :- get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
         inverse_matrix<-NULL                                             ## Assign 'NULL'to inverse
                                                                            
         set_matrix <- function(y){                                              
                 x <<- y                                                  ## Set the matrix 'x'
                 inverse_matrix <<- NULL                                      
         }                                                                   
         get_matrix <-function() x                                        ## Return the matrix 'x'
         set_inverse <- function(solve) inverse_matrix <<- solve          ## Cache the value of inverse
         get_inverse <- function() inverse_matrix                         ## Return inverse
         list(set_matrix=set_matrix,get_matrix=get_matrix,                ## Return a list of the functions
              set_inverse=set_inverse,                                       
              get_inverse=get_inverse)                                      
}                                                                                      
## The function below calculates inverse of the special matrix created with the above function
## The function initially checks whether inverse has been computed            
## If it is true then returns inverse from the cache otherwise it calculates and 
## sets the value of inverse in cache
                                                                                    
cacheSolve <- function(x, ...) {                                             
         inverse_matrix <- x$get_inverse()                                ## Gets the inverse      
         if(!is.null(inverse_matrix)){                                    ## Checks for inverse in cache   
                 message("getting cached data")                                  
                 return(inverse_matrix)                                      
         }                                                                             
         data <- x$get_matrix()                                           ## Getting matrix  
         inverse_matrix <- solve(data, ...)                               ## Computing inverse
         x$set_inverse(inverse_matrix)                                    ## Caching inverse
         inverse_matrix                                                   ## Return a matrix that is the inverse of 'x'                
                                                                          
}
