#!/usr/bin/env Rscript

library(argparser, quietly=TRUE);
    
# Create a parser
p <- arg.parser("Round a floating point number");

# Add command line arguments
p <- add.argument(p, "number", help="number to round", type="numeric");
p <- add.argument(p, "--digits", help="number of decimal places", default=0);

# Parse the command line arguments
argv <- parse.args(p);

# Do work based on the passed arguments
cat( round(argv$number, argv$digits), "\n");

