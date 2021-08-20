to_binary <- function(x) stringr::str_sub(paste(rev(as.integer(intToBits(x))), collapse=""), start = -5)
binary_vector <- function(x) stringr::str_split(to_binary(x), "", simplify = TRUE)
hamming_distance <- function(x, y) sum(x != y)