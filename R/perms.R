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

geoSeriesLookup <- function(series){
  temp <- list()
  for (i in seq_along(series)) {
    temp[[i]] <- combn(series,i)
  }
  temp <- lapply(temp, FUN = function(x){
    return(split(x, f = col(x)))
  })

  temp <- lapply(temp, FUN = function(x) {

    temp <- lapply(x, FUN = function(y){
      return(data.frame(MEDIA_COMBO = sum(y), COMBO_LOOKUP = y))
    })
    temp <- do.call("rbind", temp)
    return(temp)
  })

  return(do.call("rbind", temp))
}
