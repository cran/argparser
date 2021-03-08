parser <- arg_parser("MWE")
parser <- add_argument(parser,
	arg = c("--method", "--grid-length", "--problem-type"),
	help = c("arg1.","arg2.", "arg3."),
	default = list(NA, 400, "search"),
	type = c("character", "integer", "character"),
	nargs = c(1, 1, 1),
	flag = c(FALSE, FALSE, FALSE),
	short = c("-m", "-l", "-t")
)

test_that("argument vector is parsed correctly", {
	argv <- parse_args(parser, c("-m", "novel", "-l", "400", "-t", "binary"))
	expect_equal(argv$method, "novel")
	expect_equal(argv$grid_length, 400)
	expect_equal(argv$problem_type, "binary")
})
