## ----setup, include = FALSE------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)
options(width = 83)

## --------------------------------------------------------------------------------
# Create a directory in tempdir
path <- tempfile(pattern = "git2r-")
dir.create(path)
# Create dummy data
set.seed(20190222)
x <- data.frame(
  x = sample(LETTERS),
  y = factor(
    sample(c("a", "b", NA), 26, replace = TRUE),
    levels = c("a", "b", "c")
  ),
  z = c(NA, 1:25),
  abc = c(rnorm(25), NA),
  def = sample(c(TRUE, FALSE, NA), 26, replace = TRUE),
  timestamp = seq(
    as.POSIXct("2018-01-01"),
    as.POSIXct("2019-01-01"),
    length = 26
  ),
  stringsAsFactors = FALSE
)
str(x)

## ----first_write-----------------------------------------------------------------
library(git2rdata)
write_vc(x = x, file = "first_test", root = path, strict = FALSE)

## ----manual_data-----------------------------------------------------------------
print_file <- function(file, root, n = -1) {
  fn <- file.path(root, file)
  data <- readLines(fn, n = n)
  cat(data, sep = "\n")
}
print_file("first_test.tsv", path, 10)
print_file("first_test.yml", path)

## ----write_verbose---------------------------------------------------------------
write_vc(x = x, file = "verbose", root = path, optimize = FALSE, strict = FALSE)

## ----manual_verbose_data---------------------------------------------------------
print_file("verbose.csv", path, 10)
print_file("verbose.yml", path)

## ----first_read------------------------------------------------------------------
y <- read_vc(file = "first_test", root = path)
all.equal(x, y, check.attributes = FALSE)
y2 <- read_vc(file = "verbose", root = path)
all.equal(x, y2, check.attributes = FALSE)

## ----echo = FALSE, results = "hide"----------------------------------------------
stopifnot("X" %in% x$x, "b" %in% x$y)

## ----na_string, error = TRUE-----------------------------------------------------
try({
write_vc(x, "custom_na", path, strict = FALSE, na = "X", optimize = FALSE)
write_vc(x, "custom_na", path, strict = FALSE, na = "b", optimize = FALSE)
write_vc(x, "custom_na", path, strict = FALSE, na = "X")
write_vc(x, "custom_na", path, strict = FALSE, na = "b")
})

## ----manual_na_data--------------------------------------------------------------
print_file("custom_na.tsv", path, 10)
print_file("custom_na.yml", path, 4)

## ----empty_na--------------------------------------------------------------------
write_vc(x, "custom_na", path, strict = FALSE, na = "")

