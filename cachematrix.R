##Ove funkcije racunaju inverznu matricu i smestaju u kes
##a u slucaju da je vec sacuvana, vade je iz kes-a

##Ova funkcija vraca listu 4 funkcije
##koje rade: prva postavlja matricu argumenta, 
##druga uzima matricu datu argumentom, 
##treca postavlja vrednost inverzne matrice argumenta
##cetvrta uzima vrednost inverzne matr.(ako je sacuvana u kes-u)

makeCacheMatrix <- function(x = matrix()){

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() {
                 x
        }

        setinverse <- function(inverse) {
                i <<- inverse
        }

        getinverse <- function() {
                i
        }

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Ova funkcija stavlja u kes mem.inverznu matricu
##argumenta, ili (ako je vec sacuvana), vadi
##inverznu matricu

casheSolve <- function(x, ...){
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

