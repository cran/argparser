# Define greeting type with a defined set of valid values
setClass("greeting_type")
setMethod("coerce", c(from = "ANY", to = "greeting_type"),
	function(from, to) {
		if(!from %in% c("hello", "hey", "hi")) {
			stop("Invalid type value for greeting_type: ", from)
		}
		from
	}
)

p <- arg_parser("parser with S4 type")
p <- add_argument(p, arg = "--number", help = "a number", type = "numeric")
p <- add_argument(p, arg = "--greeting", help = "hello | hey | hi",
                  type = "greeting_type")


test_that("parser with S4 type parses correctly", {

	expect_equal(
		parse_args(p, c("--number", "4.2"))$number, 4.2
	)

	expect_match(
		parse_args(p, c("--greeting", "hello"))$greeting, "hello"
	)

	expect_match(
		parse_args(p, c("--greeting", "hey"))$greeting, "hey"
	)

	expect_error(
		parse_args(p, c("--greeting", "bye"))
	)

})

