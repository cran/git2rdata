## ----setup--------------------------------------------------------------------
library(git2rdata)
root <- tempfile("git2rdata-convert")
dir.create(root)

## ----case-conversion----------------------------------------------------------
# Create sample data
data <- data.frame(
  id = 1:3,
  name = c("alice", "bob", "charlie"),
  stringsAsFactors = FALSE
)

# Write with case conversion
write_vc(
  data,
  file = "people",
  root = root,
  sorting = "id",
  convert = list(
    name = c(
      write = "base::toupper", # Convert to uppercase when writing
      read = "base::tolower" # Convert to lowercase when reading
    )
  )
)

## ----check-storage------------------------------------------------------------
# Check the raw file content
raw_content <- readLines(file.path(root, "people.tsv"))
cat(raw_content, sep = "\n")

## ----read-back----------------------------------------------------------------
# Read the data back
result <- read_vc("people", root = root)
print(result)

# The convert specification is stored in the attributes
attr(result, "convert")

## ----multiple-columns---------------------------------------------------------
data2 <- data.frame(
  id = 1:2,
  first_name = c("alice", "bob"),
  last_name = c("smith", "jones"),
  stringsAsFactors = FALSE
)

write_vc(
  data2,
  file = "names",
  root = root,
  sorting = "id",
  convert = list(
    first_name = c(write = "base::toupper", read = "base::tolower"),
    last_name = c(write = "base::toupper", read = "base::tolower")
  )
)

result2 <- read_vc("names", root = root)
print(result2)

## ----unsupported, eval = FALSE------------------------------------------------
# mtcars2 <- mtcars |>
#   dplyr::mutate(cyl = bit64::as.integer64(cyl))
# write_vc(
#   mtcars2,
#   file = "mtcars2",
#   convert = list(
#     cyl = c(write = "bit64::as.character", read = "bit64::as.integer64")
#   )
# )

## ----numeric-conversion, eval=FALSE-------------------------------------------
# # Example with custom conversion functions
# # (requires defining custom functions in a package)
# write_vc(
#   data,
#   file = "data",
#   root = root,
#   sorting = "id",
#   convert = list(
#     large_number = c(
#       write = "mypackage::to_scientific",
#       read = "mypackage::from_scientific"
#     )
#   )
# )

## ----standardization, eval=FALSE----------------------------------------------
# # Convert dates to ISO format
# write_vc(
#   data,
#   file = "events",
#   root = root,
#   sorting = "id",
#   convert = list(
#     event_date = c(
#       write = "mypackage::to_iso_date",
#       read = "mypackage::from_iso_date"
#     )
#   )
# )

## ----cleanup, include=FALSE---------------------------------------------------
unlink(root, recursive = TRUE)

