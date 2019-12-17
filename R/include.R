#' Include R script file
#'
#' Include R script with behaviour similar to C++ \code{#include
#' "header.h"}, by searching in the directory where the 
#' current script file resides.
#'
#' @param file name
#' @export
include <- function(file) {
	source(file, local=parent.frame())
}
