p <- arg_parser("Test")
p <- add_argument(p, "--arg1", help = "arg1", type = "numeric")
p <- add_argument(p, "--arg2", help = "arg2", type = "integer")

test_that("numerics are parsed correctly", {
	argv <- parse_args(p)
	expect_true(is.na(argv$arg1))
	expect_true(is.na(argv$arg2))

	expect_equal(parse_args(p, c("--arg1", "1e-9"))$arg1, 1e-9)
	expect_equal(parse_args(p, c("--arg1", "1.5e9"))$arg1, 1.5e9)
	expect_equal(parse_args(p, c("--arg1", "-1234.5"))$arg1, -1234.5)

	expect_error(parse_args(p, c("--arg1", "A")))
})

test_that("integers are parsed correctly", {
	expect_equal(parse_args(p, c("--arg2", "100"))$arg2, 100)
	expect_equal(parse_args(p, c("--arg2", "-99"))$arg2, -99)

	expect_error(parse_args(p, c("--arg2", "A")))
	expect_error(parse_args(p, c("--arg2", "4.3")))
	expect_error(parse_args(p, c("--arg2", "1.0e-9")))
})

test_that("missing arguments are accepted", {
	argv <- parse_args(p, c("--arg1", "1.5", "--arg2", "2"))
	expect_equal(argv$arg1, 1.5)
	expect_equal(argv$arg2, 2)

	argv <- parse_args(p, c("--arg1", "3"))
	expect_equal(argv$arg1, 3)
	expect_true(is.na(argv$arg2))

	argv <- parse_args(p, c("--arg2", "4"))
	expect_equal(argv$arg2, 4)
	expect_true(is.na(argv$arg1))
})

