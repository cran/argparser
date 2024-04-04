#!/usr/bin/env Rscript

library(argparser)

ap <- arg_parser("test")
ap <- add_argument(ap, "--n-cores", default=1L, help="number of cores")
ap <- add_argument(ap, "--num-proc", default=1L, help="number of processes")
args <- parse_args(ap)
print(names(args))

