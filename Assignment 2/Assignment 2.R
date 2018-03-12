

# Prints the uppercase of names out of a list  whose length is grt 5
Names <- list("Harish","Mahesh","arun","satya","sreedhar","prasanth","Anvesh","sai","varma")
count <- 0
for (Name in Names){
  if (nchar(Name) > 5){
    count <- count +1
    cat(toupper(Name))
    cat("\n")
  }
}
cat("Total : ")
cat(count)

