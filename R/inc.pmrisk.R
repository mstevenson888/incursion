inc.pmrisk <- function(crisk, src, des) {
  
  path <- system.file("extdata", "pmovt_risk.csv", package = "incursion")
  if (path == "") stop("Lookup table not found in package 'incursion'.")
  
  lookup.df <- utils::read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  
  # "-" in the table means the combination isn't applicable -> NA
  # lookup.df$decision[lookup.df$decision == "-"] <- NA_character_
  
  make_key <- function(a, b, c) tolower(paste(trimws(a), trimws(b), trimws(c), sep = "|"))
  table_key <- make_key(lookup.df$crisk, lookup.df$src, lookup.df$des)
  input_key <- make_key(crisk, src, des)
  
  idx <- match(input_key, table_key)
  
  if (any(is.na(idx))) {
    warning(sum(is.na(idx)),
            " input row(s) did not match any crisk/src/des combination in the lookup table.")
  }
  
  tolower(lookup.df$decision[idx])
}