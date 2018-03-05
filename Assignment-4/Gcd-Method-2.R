main <- function(){
    i <- as.integer(readline(prompt="Enter 1 digit:"))
    j <- as.integer(readline(prompt="Enter 2 digit:"))
    while(i!=j){
        if( i > j){
            i <- i%%j
        }
        else{
            j <- j%%i
            j <-as.integer(j)
        }
    }
    print (i)
}

main()