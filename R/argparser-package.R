#' Command-line argument parser
#'
#' \code{argparser} provides functions for parsing command-line arguments.
#'
#' To use the parser,
#' \enumerate{
#' \item create an \code{arg.parser} object with \code{\link{arg_parser}};
#' \item add arguments to the parser with \code{\link{add_argument}};
#' \item call \code{\link{parse_args}} to parse the command line arguments.
#' }
#' To execute the script, invoke \code{Rscript}.
#' Alternatively on Linux, insert a shebang on the first line
#' (\code{#!/usr/bin/env Rscript}) and \code{chmod +x} the script,
#' 
#' @keywords internal
"_PACKAGE"

#' @import methods
NULL

