## ----setup, include = FALSE------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width = 83)

## ----initialise------------------------------------------------------------------
# Create a directory in tempdir
root <- tempfile(pattern = "git2r-")
dir.create(root)
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

## ----row_order-------------------------------------------------------------------
library(git2rdata)
write_vc(x, file = "row_order", root = root)
write_vc(x[sample(nrow(x)), ], file = "row_order", root = root)

## ----apply_sorting, error = TRUE-------------------------------------------------
try({
fn <- write_vc(x, "row_order", root, sorting = "y")
})

## ----update_sorting--------------------------------------------------------------
fn <- write_vc(x, "row_order", root, sorting = "y", strict = FALSE)
fn <- write_vc(x, "row_order", root, sorting = c("y", "x"), strict = FALSE)

## ----update_sorted---------------------------------------------------------------
print_file <- function(file, root, n = -1) {
  fn <- file.path(root, file)
  data <- readLines(fn, n = n)
  cat(data, sep = "\n")
}
print_file("row_order.yml", root, 7)
fn <- write_vc(x[sample(nrow(x)), ], "row_order", root)
fn <- write_vc(x[sample(nrow(x)), ], "row_order", root, sorting = c("y", "x"))
fn <- write_vc(x[sample(nrow(x), replace = TRUE), ], "row_order", root)

## ----variable_order--------------------------------------------------------------
write_vc(x, "column_order", root, sorting = c("x", "abc"))
print_file("column_order.tsv", root, n = 5)
write_vc(x[sample(nrow(x)), sample(ncol(x))], "column_order", root)
print_file("column_order.tsv", root, n = 5)

## ----factor----------------------------------------------------------------------
old <- data.frame(color = c("red", "blue"), stringsAsFactors = TRUE)
write_vc(old, "factor", root, sorting = "color")
print_file("factor.yml", root)

## ----factor2---------------------------------------------------------------------
updated <- data.frame(
  color = c("red", "green", "blue"),
  stringsAsFactors = TRUE
)
write_vc(updated, "factor2", root, sorting = "color")
print_file("factor2.yml", root)

## ----factor_update, error = TRUE-------------------------------------------------
try({
write_vc(updated, "factor", root)
fn <- write_vc(updated, "factor", root, strict = FALSE)
print_file("factor.yml", root)
})

## ----factor_deleted--------------------------------------------------------------
deleted <- data.frame(
  color = factor(c("red", "green"), levels = c("red", "green"))
)
write_vc(deleted, "factor", root, sorting = "color", strict = FALSE)
print_file("factor.yml", root)

## ----factor_ordered--------------------------------------------------------------
ordered <- data.frame(
  color = factor(c("red", "green"), levels = c("red", "green"), ordered = TRUE)
)
write_vc(ordered, "factor", root, sorting = "color", strict = FALSE)
print_file("factor.yml", root)

## --------------------------------------------------------------------------------
write_vc(old, "write_vc", root, sorting = "color")
print_file("write_vc.yml", root)
relabeled <- old
# translate the color names to Dutch
levels(relabeled$color) <- c("blauw", "rood")
write_vc(relabeled, "write_vc", root, strict = FALSE)
print_file("write_vc.yml", root)

## --------------------------------------------------------------------------------
write_vc(old, "relabel", root, sorting = "color")
relabel("relabel", root, change = list(color = c(red = "rood", blue = "blauw")))
print_file("relabel.yml", root)
relabel(
  "relabel", root,
  change = data.frame(
    factor = "color", old = "blauw", new = "blue", stringsAsFactors = TRUE
  )
)
print_file("relabel.yml", root)

