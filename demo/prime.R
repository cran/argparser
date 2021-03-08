#!/usr/bin/env Rscript

library(argparser, quietly=TRUE)

# Create a parser
p <- arg_parser("Check if positive integer is prime")

# Add command line arguments
p <- add_argument(p, "--number", help="numbers to check",
	type="integer")

# Parse the command line arguments
argv <- parse_args(p)

n <- argv$number;

if (is.na(n)) {
	stop("No number specified")
}

if (n == 1) {
	prime <- FALSE;
} else if (n == 2) {
	prime <- TRUE;
} else if (n > 2) {
	prime <- TRUE;
	# inefficient check
	for (i in 2:(n-1)) {
		if (n %% i == 0) {
			prime <- FALSE;
			break;
		}
	}
}

if (prime) {
	message(n, " is prime.")
} else {
	message(n, " is not prime.")
}

