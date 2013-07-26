#' Command-line argument parser
#'
#' \code{argparser} provides functions for parsing command-line arguments.
#'
#' To use the parser,
#' \enumerate{
#' \item create an arg.parser object with \code{\link{arg.parser}};
#' \item add arguments to the parser with \code{\link{add.argument}};
#' \item call \code{\link{parse.args}} to parse the command line arguments.
#' }
#' To execute the script, invoke \code{Rscript}.
#' Alternatively on Linux, insert a shebang on the first line
#' (\code{#!/usr/bin/env Rscript}) and \code{chmod +x} the script,
#' 
#' @import methods
#' @docType package
#' @name argparser
NULL

#' Create an argument parser.
#'
#' This function creates an arg.parser object. It infers the program name from 
#' the file name of the invoked script.
#' 
#' @param description  description of the program
#' @param name         name of the program
#' @return a new arg.parser object
#' @export
#'
#' @examples
#' p <- arg.parser("A test program");
#'
arg.parser <- function(description, name=NULL) {

	# set default name
	if (is.null(name)) {
		# extract file name from command arguments, which will be empty 
		# if the program is not invoked as a script
		prefix <- "--file=";
		name <- sub(prefix, "", grep(paste(prefix, "(.+)", sep=""), commandArgs(), value=TRUE));
	}
	if (length(name) == 0) name <- "<script>";

	# create object
	parser <- structure(
		list(name = name, description = description),
		class = "arg.parser"
	);

	# add default arguments
	parser <- add.argument(parser, "--help", "show this help message and exit", flag=TRUE);
	parser <- add.argument(parser, "--opts", "list of named values for optional arguments (in a RDS file)", short="-x");

	parser
}

#' Add an argument to a parser.
#'
#' This function adds an argument to an arg.parser object and returns the 
#' modified object.
#' 
#' This function supports multiple arguments in a vector. To ensure that the
#' argument variable type is set correctly, either specify \code{type} directly
#' or supply \code{default} argument values as a list. Arguments that consume 
#' more than one values are not supported.
#' 
#' @param parser  an arg.parser object
#' @param arg     argument name (use no prefix for positional arguments,
#'                \code{--} or \code{-} prefix for optional arguments or flags)
#' @param help    help description for the argument
#' @param default default value for the argument [default: NA]
#' @param type    variable type of the argument (which can be inferred from 
#'                \code{default}), assumed to be \code{character} otherwise
#' @param flag    whether argument is a flag (and does not consume a value)
#'                [default: FALSE]
#' @param short   short-form for flags and positional arguments;
#'                short-forms can be assigned automatically based on the first
#'                character of the argument name, unless a conflict arises with
#'                an existing short-form; to avoid conflicts, add the argument 
#'                as early as possible
#' @return an arg.parser object with the argument added
#' @export
#'
#' @examples
#' p <- arg.parser("A text file modifying program");
#'
#' # add a positional argument
#' p <- add.argument(p, "input", help="input file");
#'
#' # add an optional argument
#' p <- add.argument(p, "--output", help="output file", default="output.txt");
#'
#' # add a flag
#' p <- add.argument(p, "--append", help="append to file", flag=TRUE);
#'
#' # add multiple arguments together
#' p <- add.argument(p,
#'     c("ref", "--date", "--sort"),
#'     help = c("reference file", "date stamp to use", "sort lines"),
#'     flag = c(FALSE, FALSE, TRUE));
#'
#' # print the help message
#' print(p);
#' 
add.argument <- function(
	parser,
	arg, help,
	default=NULL, type=NULL, flag=NULL, short=NULL
) {

	stopifnot(is(parser, "arg.parser"));

	## Set parameters
	if (is.null(default)) {
		default <- as.list(rep(NA, length(arg)));
	}
	if (is.null(type)) {
		type <- rep("character", length(arg));
	}
	if (is.null(flag)) {
		flag <- rep(FALSE, length(arg));
	}
	if (is.null(short)) {
		short <- rep(NA, length(arg));
	}

	# all argument properties should be of the same length
	stopifnot(length(arg) == length(help));
	stopifnot(length(arg) == length(default));
	stopifnot(length(arg) == length(type));
	stopifnot(length(arg) == length(flag));
	stopifnot(length(arg) == length(short));

	# to avoid automatic conversion of `default` variable types by R,
	# require that default be either single-valued or a list 
	# (which supports multi-type elements)
	if (length(default) > 1 && !is.list(default)) {
		stop("To ensure that argument type inference works correctly, `default` must be either single-valued or a list");
	}

	## Append new argument
	parser$args <- c(parser$args, arg);
	parser$helps <- c(parser$helps, help);
	# infer type based on the default values (original default variable), 
	# whenever available
	type[!is.na(default)] <- unlist(lapply(default, class));
	# NB  upon concatenation, all default values will be converted to the most
	#     permissive variable type (most likely a character)
	parser$defaults <- c(parser$defaults, unlist(default));
	parser$types <- c(parser$types, type);
	parser$is.flag <- c(parser$is.flag, flag);

	# optional arguments are prefixed with at least one '-' character
	is.opt.arg <- !flag & 1:length(arg) %in% grep("^-", arg);
	parser$is.opt.arg <- c(parser$is.opt.arg, is.opt.arg);

	# positional arguments (required arguments) are neither flags nor 
	# optional arguments
	is.req.arg <- !(flag | is.opt.arg);
	parser$is.req.arg <- c(parser$is.req.arg, is.req.arg);

	# if a short-hand is not given, automatically assign a short-hand
	replace.idx <- is.na(short) & (flag | is.opt.arg);
	if (sum(replace.idx) > 0) {
		# use the first letter, prefixed with a single '-'
		short[replace.idx] <- sub("--(.).*", "-\\1", arg[replace.idx]);
	}
	parser$shorts <- c(parser$shorts, short);
	# remove duplicate short-form arguments (remove the later ones)
	parser$shorts[duplicated(parser$shorts, fromLast=FALSE)] <- NA;

	parser
}

#' Print the help message for an arg.parser.
#'
#' This function prints the help message.
#'
#' At the command line, we would use the \code{--help} or \code{-help} flag
#' to print the help message:
#' \code{$ script --help}
#'
#' @param x   an arg.parser object
#' @param ... unused arguments
#' @export
#'
print.arg.parser <- function(x, ...) {
	
	parser <- x;
	
	# print usage
	opt.args <- parser$args[parser$is.opt.arg];
	message("usage: ", parser$name, " ",
		paste(sub("^(.*)$", "[\\1]", parser$args[parser$is.flag]), collapse=" "),
		" ",
		paste(sub("^(.*)$", "[\\1 ", opt.args),
			toupper(sub("^--(.*)$", "\\1]", opt.args)),
			sep="", collapse=" "),
		" ",
		paste(parser$args[parser$is.req.arg], collapse=" "),
		"\n"
	);
	# print description
	message(parser$description, "\n");

	# print position arguments
	if (sum(parser$is.req.arg) > 0) {
		message("positional arguments:");
		for (i in which(parser$is.req.arg)) {
			message("  ", parser$args[i], "\t\t\t", parser$helps[i]);
		}
	}
	message("");

	# print flags
	if (sum(parser$is.flag) > 0) {
		message("flags:");
		for (i in which(parser$is.flag)) {
			if (is.na(parser$shorts[i])) {
				arg.name <- parser$args[i];
			} else {
				arg.name <- paste(parser$shorts[i], parser$args[i], sep=", ");
			}
			if (is.na(parser$defaults[i])) {
				arg.help <- parser$helps[i];
			} else {
				arg.help <- paste(parser$helps[i], " [default: ",
					parser$defaults[i], "]", sep="");
			}
			message("  ", arg.name, "\t\t\t", arg.help);
		}
	}
	message("");

	# print optional arguments
	if (sum(parser$is.opt.arg) > 0) {
		message("optional arguments:");
		for (i in which(parser$is.opt.arg)) {
			if (is.na(parser$shorts[i])) {
				arg.name <- parser$args[i];
			} else {
				arg.name <- paste(parser$shorts[i], parser$args[i], sep=", ");
			}
			arg.name <- paste(arg.name, toupper(sub("^-+", "", parser$args[i])));
			if (is.na(parser$defaults[i])) {
				arg.help <- parser$helps[i];
			} else {
				arg.help <- paste(parser$helps[i], " [default: ",
					parser$defaults[i], "]", sep="");
			}
			message("  ", arg.name, "\t\t\t", arg.help);
		}
	}
}

#' Parse arguments with a parser.
#' 
#' This function uses an arg.parser object to parse command line arguments or a
#' character vector.
#'
#' @param parser  an arg.parser object
#' @param argv    a character vector to parse (arguments and values should 
#'                already be split by whitespace)
#' @return a list with argument values
#' @export
#'
#' @examples
#' p <- arg.parser('pi');
#' p <- add.argument(p, "--digits",
#'   help="number of significant digits to print", default=7);
#' 
#' \dontrun{
#' # if arguments are passed from the command line,
#' # then we would use the following:
#' argv <- parse.args(p);
#' }
#' 
#' # for testing purposes, we can pass a character vector:
#' argv <- parse.args(p, c("-d", "30"));
#'
#' # now, the script runs based on the passed arguments
#' digits <- if (argv$digits > 22) 22 else argv$digits;
#' print(pi, digits=digits);
#' 
#' \dontrun{
#' # we can also save an argument list for later use
#' saveRDS(argv, "arguments.rds");
#'
#' # to use the saved arguments, use the --opts argument at the command line
#' #$ script --opts arguments.rds
#' } 
#'
parse.args <- function(parser, argv=commandArgs(trailingOnly=TRUE)) {
	stopifnot(is(parser, "arg.parser"));
	values <- list();

	## Replace short-forms with long-forms
	ind <- match(argv, parser$shorts);
	ind.valid <- !is.na(ind);
	argv[ind.valid] <- parser$args[ind[ind.valid]];

	## Extract flag arguments
	arg.flags <- parser$args[parser$is.flag];
	x <- as.logical(parser$defaults[parser$is.flag]);
	x[is.na(x)] <- FALSE;
	names(x) <- sub("^-+", "", arg.flags);
	# find argument in argv
	flag.idx <- match(arg.flags, argv);
	flag.idx <- flag.idx[!is.na(flag.idx)];
	if (length(flag.idx) > 0) {
		# set flags to TRUE
		x[match(argv[flag.idx], arg.flags)] <- TRUE;
		# remove extracted arguments
		argv <- argv[-flag.idx];
	}
	# append argument values
	values <- c(values, x);

	## Process special argument: help
	if (values$help) {
		# print usage and exit
		print(parser);
		quit();
	}

	## Use default values as placeholder for optional arguments
	x <- parser$defaults[parser$is.opt.arg];
	arg.opt <- parser$args[parser$is.opt.arg];
	names(x) <- sub("^-+", "", arg.opt);
			
	## Process special argument: opts
	i <- match("--opts", argv);
	if (!is.na(i)) {
		opts <- readRDS(argv[i+1]);
		idx <- match(names(opts), names(x));
		if (any(is.na(idx))) {
			stop("Error: extra arguments supplied in OPTS file (", paste(setdiff(names(opts), names(x)), collapse=", "), ").");
		}
		x[match(names(opts), names(x))] <- opts;
	}

	## Extract optional arguments (each of which is a tuple of (arg, value))
	arg.idx <- match(arg.opt, argv);
	arg.idx <- arg.idx[!is.na(arg.idx)];
	arg.opt.types <- parser$types[parser$is.opt.arg];
	# Set optional arguments
	if (length(arg.idx) > 0) {
		# extract values following the argument flag
		x[match(argv[arg.idx], arg.opt)] <- argv[arg.idx+1];
		# convert type of extraced values
		x <- mapply(as, object=x, Class=arg.opt.types, SIMPLIFY=FALSE);
		# remove extracted arguments
		to.remove <- c(arg.idx, arg.idx+1);
		argv <- argv[-to.remove];
	}
	# append argument values to list
	# (since values is a list, x is not type-converted)
	values <- c(values, x);

	## Extract remaining arguments as required (positional) arguments
	x <- argv;
	args.req <- parser$args[parser$is.req.arg];
	args.req.types <- parser$types[parser$is.req.arg];
	if (length(args.req) > 0) {
		if (length(x) < length(args.req)) {
			print(parser);
			stop("Error: missing required arguments (", paste(setdiff(args.req, x), collapse=", "), ").");
		} else if (length(x) > length(args.req)) {
			print(parser);
			stop("Error: extra arguments supplied (", paste(setdiff(x, args.req), collapse=", "), ").");
		} else {
			names(x) <- args.req;
			# convert type of extracted value
			x <- mapply(as, object=x, Class=args.req.types, SIMPLIFY=FALSE);
		}
	}
	# append argument values
	values <- c(values, x);

	values
}

