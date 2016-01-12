perms <-
function(x, loc){
  # Function to create a list of all possible permitations.
  # Saves csv file.
  temp <- list()
  opts <- length(x)
 for (i in 1:opts) {
   temp[[i]] <- data.frame(Combinations = apply(combn(x,i), 2,
                                           paste, collapse = ", "))
 }
  temp <- do.call("rbind", temp)
  write.csv(temp, loc,row.names = FALSE, quote = FALSE)
}
