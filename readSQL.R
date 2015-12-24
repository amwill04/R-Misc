parseSQL <- function(fileLoc) {
  # Function to read, parsed and return SQL queries as strings.
  # fileLoc variable is the directory location of the SQL file to be parsed.
  # The functions first reads each lines of the SQL query into a vector
  # Next, inline comments are removed
  # The vector is then collapsed into a single character vector
  # All comment blocks are removed before finally trimming any leading or
  # trailing whitespace.
  # Finally, SQL as character string is passed out of function.
  
  sqlParsed <- readLines(fileLoc, warn = FALSE)
  sqlParsed <- gsub("\\--(.*)", "", sqlParsed, perl = TRUE)
  sqlParsed <- paste(sqlParsed, collapse = " ")
  sqlParsed <- gsub("(/\\*)+?[\\w\\W]+?(\\*/)", "", sqlParsed, perl = TRUE)
  sqlParsed <- gsub("\\s\\s+", " ", sqlParsed, perl = TRUE)
  sqlParsed <- gsub("^\\s+|\\s+$", "", sqlParsed)
  return (sqlParsed)
  
}
