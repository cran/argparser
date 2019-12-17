#!/usr/bin/env Rscript

library(argparser, quietly=TRUE)
    
# Create a parser
p <- arg_parser("Calculate the sum of two numbers")

# Add command line arguments
# Note: The values for the numbers argument have to provided as a comma
#       separated string, since we are specifying a position argument here.
p <- add_argument(p, "numbers", help="comma separated list of two numbers to add",
	nargs=2, type="numeric")

# Parse the command line arguments
argv <- parse_args(p)

# Do work based on the passed arguments
cat( sum(argv$numbers), "\n")

