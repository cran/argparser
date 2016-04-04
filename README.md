# README #

`argparser` is a cross-platform command-line argument parser for R, written in R,
with no external dependencies. This package is useful with the Rscript
front-end and facilitates turning an R script into an executable script.

### History ###

* v0.4 (in progress)
       *Incompatibility*: '-' in argument names are substitute by '_'

* v0.3 Added support for multi-valued arguments

* v0.2 Function names change
       *Incompatibility*: '.' in function names are replaced with '_' 

* v0.1 Initial release


### Dependencies ###

* R (>= 3.0)
* roxygen2 (>= 4.0, for building only)

### Build ###

Clone the repository, build the documentation with roxygen2, then install.

    $ git clone https://bitbucket.org/dshih/argparser.git
    $ cd argparser
    $ R

    R> library(roxygen2)
    R> roxygenize()
    R> quit()

    $ R CMD INSTALL .

### Usage ###

Create a R script (e.g. round.R) with a shebang line (Linux only).

Import the argparser library, create a parser, populate the parser with arguments
and parse the command line arguments.


    #!/usr/bin/env Rscript

    library(argparser, quietly=TRUE)
    
    # Create a parser
    p <- arg_parser("Round a floating point number")
    
    # Add command line arguments
    p <- add_argument(p, "number", help="number to round", type="numeric")
    p <- add_argument(p, "--digits", help="number of decimal places", default=0)
    
    # Parse the command line arguments
    argv <- parse_args(p)
    
    # Do work based on the passed arguments
    cat( round(argv$number, argv$digits), "\n")

Then, set the R script as executable and execute (Linux only).

    $ chmod +x round.R
    
    # Print the help message
    $ ./round.R -h

Alternatively, run the script using Rscript (Linux or Windows).

    $ Rscript round.R 3.141
    # 3
    
    $ Rscript round.R 3.141 -d 2
    # 3.14

For R help, see `?argparser`.
