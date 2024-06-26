#' Create an argument parser.
#'
#' This function creates an \code{arg.parser} object. It infers the program 
#' name from the file name of the invoked script.
#'
#' The argument parser will be created by default with two arguments:
#' \code{--help} and \code{--opts}. The latter argument can be used for
#' loading a list of argument values that are saved in a RDS file.
#' 
#' @param description  description of the program
#' @param name         name of the program
#' @param hide.opts    hide the \code{--opts} argument
#' @return a new \code{arg.parser} object
#' @export
#'
#' @examples
#' p <- arg_parser("A test program")
#'
arg_parser <- function(description, name=NULL, hide.opts=FALSE) {

	# set default name
	if (is.null(name)) {
		# extract file name from command arguments, which will be empty 
		# if the program is not invoked as a script
		prefix <- "--file=";
		name <- basename(sub(prefix, "",
			grep(paste(prefix, "(.+)", sep=""), commandArgs(), value=TRUE)
		));
		
	}
	if (length(name) == 0) name <- "<script>";

	# create object
	parser <- structure(
		list(name = name, description = description),
		class = "arg.parser"
	);

	# add default arguments
	parser <- add_argument(parser, "--", "placeholder", flag=TRUE);
	parser <- add_argument(parser, "--help", "show this help message and exit", flag=TRUE);
	if (!hide.opts) {
		parser <- add_argument(parser, "--opts", "RDS file containing argument values", short="-x");
	}

	parser
}

#' Add an argument to a parser.
#'
#' This function adds an argument to an \code{arg.parser} object and returns 
#' the modified object.
#' 
#' @details
#' This function supports multiple arguments in a vector. To ensure that the
#' argument variable type is set correctly, either specify \code{type} directly
#' or supply \code{default} argument values as a list. 
#' Custom types are supported by defining a new class and a S4 method for
#' \code{coerce}, see the examples section.
#'
#' @note Dashes \code{-} that occur in the stem of the argument names
#' (e.g. --argument-name) will be converted to underscores \code{_} 
#' (e.g. argument_name) in the name of the corresponding variable.
#' 
#' @param parser  an \code{arg.parser} object
#' @param arg     argument name (use no prefix for positional arguments,
#'                \code{--} or \code{-} prefix for optional arguments or flags)
#' @param help    help description for the argument
#' @param default default value for the argument [default: NA]
#' @param type    variable type of the argument (which can be inferred from 
#'                \code{default}); assumed to be \code{character} otherwise.
#'                See details for more information.
#' @param nargs   number of argument values (which can be inferred from 
#'                \code{default}); set to \code{Inf} for an indefinite number;
#'                an optional argument with an indefinite number of values may
#'                need to be followed by another optional argument or flag (e.g.
#'                \code{--}) to separate the indefinite optional argument from
#'                possible position arguments
#' @param flag    whether argument is a flag (and does not consume a value)
#'                [default: FALSE]; during argument parsing, a flag argument 
#'                is FALSE by default if it is not set
#' @param short   short-form for flags and positional arguments;
#'                short-forms can be assigned automatically based on the first
#'                character of the argument name, unless a conflict arises with
#'                an existing short-form; to avoid conflicts, add the argument 
#'                as early as possible
#' @return an \code{arg.parser} object with the argument added
#' @export
#'
#' @examples
#' p <- arg_parser("A text file modifying program")
#'
#' # Add a positional argument
#' p <- add_argument(p, "input", help="input file")
#'
#' # Add an optional argument
#' p <- add_argument(p, "--output", help="output file", default="output.txt")
#'
#' # Add a flag
#' p <- add_argument(p, "--append", help="append to file", flag=TRUE)
#'
#' # Add multiple arguments together
#' p <- add_argument(p,
#'     c("ref", "--date", "--sort"),
#'     help = c("reference file", "date stamp to use", "sort lines"),
#'     flag = c(FALSE, FALSE, TRUE))
#'
#' # Print the help message
#' print(p)
#'
#' # Example of custom type, using the example from pythons argparse
#' setClass("perfectSquare")
#' setMethod("coerce", c(from = "ANY", to = "perfectSquare"),
#'     function(from, to) {
#'       from <- as.numeric(from)
#'       if (!all.equal(from, as.integer(from))) {
#'         stop("Type error: ", from, " is not an integer!")
#'       }
#'       sqt <- sqrt(from)
#'       if (sqt != as.integer(sqt)) {
#'          stop("Type error: ", from, " is not a perfect square!")
#'       }
#'       from
#'     }
#' )
#' 
#' p2 <- arg_parser("Perfect square checker")
#' p2 <- add_argument(p2, arg = c("--perfect-square"), 
#'                    help = "A perfect square integer",
#'                    type = "perfectSquare")
#'
#' parse_args(p2, c("--perfect-square", 144))
#' 
add_argument <- function(
	parser,
	arg, help,
	default=NULL, type=NULL, nargs=NULL, flag=NULL, short=NULL
) {

	stopifnot(is(parser, "arg.parser"));

	check_arg_conflict(parser, arg);

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
	if (is.null(nargs)) {
		nargs <- ifelse(flag, 0, 1);
	}

	## Error checking

	# when multiple arguments are being defined, default must be a list!
	# in fact, default should always be a list to support multi-type elements
	# since it is tedious to wrap all default values in a list,
	# perform this packaging as necessary 
	if (!is.list(default)) {
		if (length(arg) > 1) {
			stop("When multiple arguments are being defined, `default` must be a list so that inadvertent typecasting does not occur");
		}
		default <- list(default);
	}

	# many arguments can be specified in `arg`,
	# but all argument properties should be of the same length
	stopifnot(length(arg) == length(help));
	stopifnot(length(arg) == length(default));
	stopifnot(length(arg) == length(type));
	stopifnot(length(arg) == length(flag));
	stopifnot(length(arg) == length(short));

	# flags cannot have user-specified default argument value
	# the default value of flag will be FALSE
	if (any(flag & !is.na(default))) {
		stop("Flags cannot have default value specified");
	}

	## Append new argument
	parser$args <- c(parser$args, arg);
	parser$helps <- c(parser$helps, help);

	# infer type based on the default values (original default variable), 
	# whenever available
	type[!is.na(default)] <- unlist(lapply(default[!is.na(default)], class));
	# infer number of arguments based on default values
	nargs[!is.na(default)] <- unlist(lapply(default[!is.na(default)], length));

	parser$defaults <- c(parser$defaults, default);
	parser$types <- c(parser$types, type);
	parser$nargs <- c(parser$nargs, nargs);
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


	# Additional error checking

	if (any(is.req.arg & !is.finite(nargs))) {
		stop("Positional arguments cannot have an indefinite number of argument values");
	}

	parser
}

# Pretty print.
#
# Pretty print a string with line wrap to stderr.
# @param x   character vector that will be collapsed on a space
# @param con connection object
pprint <- function(x, con = stderr(), ...) {
	writeLines(strwrap(paste(x, collapse=" "), ...), con);
}

#' Space string.
#'
#' @param n  number of spaces
#' @return a character string containing \code{n} spaces
spaces <- function(n) {
	paste0(rep(" ", n), collapse="")
}

#' Extract label and help strings from parser.
#'
#' @param parser  \code{arg.parser} object
#' @return a list containing a \code{reg.args}, \code{flags}, and
#'         \code{opt.args} list, which each containing a \code{label}
#'         string and a \code{help} string
show_arg_labels <- function(parser) {
	req.args <- lapply(which(parser$is.req.arg),
		function(i) {
			list(
				label = parser$args[i],
				help = parser$helps[i]
			)
		}
	);

	# skip special flag
	flags <- lapply(which(parser$is.flag & parser$args != "--"),
		function(i) {
			if (is.na(parser$shorts[i])) {
				arg.name <- parser$args[i];
			} else {
				arg.name <- paste(parser$shorts[i], parser$args[i], sep=", ");
			}

			list(
				label = arg.name,
				help = make_arg_help(parser, i)
			)
		}
	);

	opt.args <- lapply(which(parser$is.opt.arg),
		function(i) {
			if (is.na(parser$shorts[i])) {
				arg.name <- parser$args[i];
			} else {
				arg.name <- paste(parser$shorts[i], parser$args[i], sep=", ");
			}
			#arg.name <- paste(arg.name, toupper(sub("^-+", "", parser$args[i])));
			
			list(
				label = arg.name,
				help = make_arg_help(parser, i)
			)
		}
	);

	list(
		req.args = req.args, flags = flags, opt.args = opt.args
	)
}

# Left pad string with spaces.
#
# @param x  character vector
# @param n  target width
pad_to <- function(x, width) {
	d <- width - nchar(x);
	if (d > 0) {
		paste0(x, spaces(d))
	} else {
		x
	}
}

# Pretty print labels.
#
# @param x  a list containing label and help character vectors
pprint_label <- function(x, indent, width) {
	# insert indent
	if (width == 0) {
		# print new line instead of pad
		pprint(x$label, indent=indent);
		# indent the subsequent lines to the next level
		prefix <- spaces(indent + 6);
		pprint(x$help, initial = prefix, prefix = prefix)
	} else {
		# pad labels to fixed width
		label <- pad_to(x$label, width);
		pprint(x$help, initial = paste0(spaces(indent), label), prefix = spaces(width + indent))
	}
}

#' Print the help message for an arg.parser.
#'
#' This function prints the help message.
#'
#' At the command line, we would use the \code{--help} or \code{-help} flag
#' to print the help message:
#' \code{$ script --help}
#'
#' @param x   an \code{arg.parser} object
#' @param ... unused arguments
#' @export
#'
print.arg.parser <- function(x, ...) {
	
	parser <- x;

	# number of indent spaces before label
	indent <- 2;

	# number of padding spaces after argument label
	padding <- 2;

	# maximum label length before inserting help message on next line
	max.len <- 40;
	
	# print usage
	opt.args <- parser$args[parser$is.opt.arg];
	usage <- c("usage: ", parser$name, " ",
		# flags
		paste(sub("^(.*)$", "[\\1]", parser$args[parser$is.flag]), collapse=" "),
		" ",
		# optional arguments
		paste(sub("^(.*)$", "[\\1 ", opt.args),
			toupper(sub("^--(.*)$", "\\1]", opt.args)),
			sep="", collapse=" "),
		" ",
		# positional arguments
		paste(parser$args[parser$is.req.arg], collapse=" ")
	);
	pprint(usage, exdent=7);
	pprint("");

	# print description
	pprint(c(parser$description, "\n"));
	pprint("");

	labs <- show_arg_labels(parser);

	lab.lens <- unlist(lapply(labs, function(xs) {
		lapply(xs, function(x) {
			nchar(x$label)
		})
	}));
	max.lab.len <- max(lab.lens);

	# padding width
	pwidth <- max.lab.len + padding;
	if (indent + pwidth > max.len) {
		pwidth <- 0;
	}

	# print position arguments
	if (length(labs$req.args) > 0) {
		pprint("positional arguments:");
		lapply(labs$req.args, pprint_label, indent=indent, width=pwidth);
		pprint("");
	}

	# print flags
	if (length(labs$flags) > 0) {
		pprint("flags:");
		lapply(labs$flags, pprint_label, indent=indent, width=pwidth);
		pprint("");
	}

	# print position arguments
	# print optional arguments
	if (length(labs$opt.args) > 0) {
		pprint("optional arguments:");
		lapply(labs$opt.args, pprint_label, indent=indent, width=pwidth);
		pprint("");
	}
}

make_arg_help <- function(parser, i) {
	if (length(parser$defaults[[i]]) > 1) {
		arg.help <- paste(parser$helps[i], " [default: (",
			paste(parser$defaults[[i]], collapse=getOption("argparser.delim")),
			")]", sep="");
	} else if (is.na(parser$defaults[[i]])) {
		arg.help <- parser$helps[i];
	} else {
		arg.help <- paste(parser$helps[i], " [default: ",
			parser$defaults[[i]], "]", sep="");
	}
}

#' Parse arguments with a parser.
#' 
#' This function uses an \code{arg.parser} object to parse command line arguments or a
#' character vector.
#'
#' @param parser  an \code{arg.parser} object
#' @param argv    a character vector to parse (arguments and values should 
#'                already be split by whitespace);
#'                if \code{NULL}, values will be obtained from \code{argv} if
#'                \code{argv} exists in the global scope, or from
#'                \code{commandArgs(trailingOnly=TRUE)}.
#' @return a list with argument values
#' @export
#'
#' @examples
#' p <- arg_parser('pi')
#' p <- add_argument(p, "--digits",
#'   help="number of significant digits to print", default=7)
#' 
#' \dontrun{
#' # If arguments are passed from the command line,
#' # then we would use the following:
#' argv <- parse_args(p)
#' }
#' 
#' # For testing purposes, we can pass a character vector:
#' argv <- parse_args(p, c("-d", "30"))
#'
#' # Now, the script runs based on the passed arguments
#' digits <- if (argv$digits > 22) 22 else argv$digits
#' print(pi, digits=digits)
#' 
#' \dontrun{
#' # We can also save an argument list for later use
#' saveRDS(argv, "arguments.rds")
#'
#' # To use the saved arguments, use the --opts argument at the command line
#' #$ ./script.R --opts arguments.rds
#' } 
#'
parse_args <- function(parser, argv=NULL) {
	stopifnot(is(parser, "arg.parser"));
	values <- list();

	# obtain argument vector from global environment (littler)
	# or from commandArgs (R)
	if (is.null(argv)) {
		if (exists("argv", envir = .GlobalEnv)) {
			argv0 <- get("argv", envir = .GlobalEnv);
			if (is.character(argv0)) {
				argv <- argv0;
			}
		}
		# failed to get valid argv from global environment: argv is still null
		if (is.null(argv)) {
			argv <- commandArgs(trailingOnly=TRUE);
		}
	}

	argv <- preprocess_argv(argv, parser);

	## Extract flag arguments, ignoring special flag
	arg.flags <- parser$args[parser$is.flag];
	# any non-zero, non-NA numeric value will be converted to TRUE
	# "FALSE", "False", and "false" are converted to FALSE
	x <- as.logical(parser$defaults[parser$is.flag]);
	# convert everything else to FALSE
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

		# remove special arguments
		opts <- opts[! names(opts) %in% c("opts", "help")];

		# match the sanitized argument names
		idx <- match(sanitize_arg_names(names(opts)), sanitize_arg_names(names(x)));

		if (any(is.na(idx))) {
			stop("Extra arguments supplied in OPTS file: (", paste(setdiff(names(opts), names(x)), collapse=", "), ").");
		}
		x[idx] <- opts;
	}

	## Extract optional arguments (each of which is a tuple of (arg, value))
	arg.idx <- match(arg.opt, argv);
	arg.idx <- arg.idx[!is.na(arg.idx)];
	arg.opt.types <- parser$types[parser$is.opt.arg];
	arg.opt.nargs <- parser$nargs[parser$is.opt.arg];
	# Set optional arguments
	if (length(arg.idx) > 0) {
		# extract values following the optional argument label
		x[match(argv[arg.idx], arg.opt)] <- argv[arg.idx+1];
		# convert type of extracted values; x is now a list
		x <- mapply(convert_type, 
			object=x, class=arg.opt.types, nargs=arg.opt.nargs,
			SIMPLIFY=FALSE);
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
	args.req.nargs <- parser$nargs[parser$is.req.arg];
	if (length(x) < length(args.req)) {
		print(parser);
		stop(sprintf("Missing required arguments: expecting %d values but got %d values: (%s).",
			length(args.req), length(x), paste(x, collapse=", ")));
	} else if (length(x) > length(args.req)) {
		print(parser);
		stop(sprintf("Extra arguments supplied: expecting %d values but got %d values: (%s).",
			length(args.req), length(x), paste(x, collapse=", ")));
	} else if (length(args.req) > 0) {
		names(x) <- args.req;
		# convert type of extracted value
		x <- mapply(convert_type,
			object=x, class=args.req.types, nargs=args.req.nargs,
			SIMPLIFY=FALSE);
	}
	# append argument values
	values <- c(values, x);

	names(values) <- sanitize_arg_names(names(values));

	values
}

# strip possible leading - and replace '-' with '_'
sanitize_arg_names <- function(x) {
	gsub("-", "_", sub("^-+", "", x), fixed=TRUE)
}

# split concatenated short-form argument names
split_short_forms <- function(x) {
	tokens <- strsplit(x, "")[[1]];
	# first token is "-"
	# re-create short-form names
	paste0("-", tokens)
}


# Preprocess argument vector
preprocess_argv <- function(argv, parser) {

	## Extract concacenated short-forms into separate tokens
	# to avoid matching negative numbers, disallow the first token to be digit
	idx <- grep("^-[^0-9-][^-]+", argv);
	expanded <- lapply(idx, function(i) split_short_forms(argv[i]));
	# put expanded arguments into their original position
	argvl <- as.list(argv);
	argvl[idx] <- expanded;
	argv <- unlist(argvl);

	## Replace short-forms with long-forms
	ind <- match(argv, parser$shorts);
	ind.valid <- !is.na(ind);
	argv[ind.valid] <- parser$args[ind[ind.valid]];

	# determine whether each argument is an argument label
	idx.labels <- grepl("^--", argv);

	## Check that each argument label is defined in the parser
	arg.idx <- match(argv[idx.labels], parser$arg);
	arg.idx.na <- is.na(arg.idx);
	if (any(arg.idx.na)) {
		for (i in which(arg.idx.na)) {
			message("Argument '", argv[idx.labels][i],
				"' is not a defined optional argument or flag");
		}
		stop("Undefined argument labels supplied");
	}

	## Convert multi-value argument to a single character-delimited argument
	# after this step, each argument label should be followed by either one
	# or zero argument values
	argv2 <- NULL;
	i <- 1;
	while (i <= length(argv)) {
		argv2 <- c(argv2, argv[i]);
		if (idx.labels[i]) {

			idx <- match(argv[i], parser$arg);
			nargs <- parser$nargs[idx];

			if (!is.finite(nargs)) {
				# determine number of arguments supplied
				# will later consume all values up to the next argument label
				# NB  required arguments may be consumed:
				#     need to insert an optional argument or flag (e.g. "--")
				#     after an argument with an indefinite number of values
				next.arg <- i + 1;
				while (next.arg <= length(argv) && !idx.labels[next.arg]) {
					next.arg <- next.arg + 1;
				}
				nargs <- next.arg - i - 1;
			}

			if (nargs > 0) {
				# consume succeeding arguments for the optional argument	
				if (i + nargs > length(argv) || any(idx.labels[i + (1:nargs)])) {
					stop("Insufficient number of arguments supplied for ", argv[i]);
				} else {
					tmp <- argv[i + (1:nargs)];
					# push concatenated multi-value argument onto new argument vector
					argv2 <- c(argv2, paste(tmp, collapse=getOption("argparser.delim")));
					i <- i + nargs;
				}
			}
		}
		i <- i + 1;
	}

	argv2
}

# Convert `object` into class `class` using as and handle multi-element value
convert_type <- function(object, class, nargs) {
	if (any(is.na(object))) return(NA);

	if (nargs > 1 && is.character(object) && length(object) == 1) {
		# `object` is a character vector containing a delimiter: it is a multi-element value
		# strip away possible enclosing brackets
		object <- gsub("\\((.+)\\)", "\\1", object);
		# split the values
		object <- strsplit(object, getOption("argparser.delim"), fixed=TRUE)[[1]];
		stopifnot(!is.finite(nargs) || length(object) == nargs);
	}

	# specifically test for the most common input error:
	# conversion of non-numeric string to numeric
	# canCoerce does not help here because canCoerce("a", "numeric") == TRUE
	if (class == "integer") {
		# NB  When using scientific notation, it is difficult to check whether
		#     the number of decimal places exceeds the exponent,
		#     so we will allow some non-integers in scientific notation
		#     e.g. 1.2e3 is an integer while 1.23e1 is not, but it is difficult
		#          to determine this with regex alone
		#	    
		if (! all(grepl("^(([-+]?((([0-9]+)(\\.?[0-9]+[eE][+]?[0-9]+)?)|Inf))|NA|NaN)$", object))) {
			stop(sprintf("Invalid argument value: expecting integer but got: (%s).", 
				paste(object, collapse=", ")))
		}
	} else if (class == "numeric") {
		if (! all(grepl("^(([-+]?(((([0-9]+)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|Inf))|NA|NaN)$", object))) {
			stop(sprintf("Invalid argument value: expecting numeric but got: (%s).",
				paste(object, collapse=", ")))
		}
	}
	
	as(object, class)
}

check_arg_conflict <- function(parser, arg) {
	args.new.clean <- sanitize_arg_names(arg);
	args.clean <- sanitize_arg_names(parser$args);
	idx <- args.new.clean %in% args.clean;

	if (any(idx)) {
		ridx <- args.clean %in% args.new.clean;
		stop(
			"Argument(s) conlicts with an existing argument:\n  ",
			paste(arg[idx], collapse=", "), " conflicts with ",
			paste(parser$args[ridx], collapse=", ")
		);
	}

	if (any(arg %in% parser$shorts)) {
		stop("Argument(s) conflicts with an existing argument short-form: ",
			paste(arg[arg %in% parser$shorts], collapse=", "));
	}
}

