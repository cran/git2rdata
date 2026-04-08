## ----setup--------------------------------------------------------------------
library(git2rdata)
root <- tempfile("git2rdata-package")
dir.create(root)

## ----create-data--------------------------------------------------------------
# Write several datasets in non-optimized (CSV) format
write_vc(
  iris,
  file = "iris",
  root = root,
  sorting = c("Species", "Sepal.Length"),
  optimize = FALSE # Use CSV format instead of optimized TSV
)

write_vc(
  mtcars,
  file = "mtcars",
  root = root,
  sorting = "mpg",
  optimize = FALSE
)

# Check what files were created
list.files(root, recursive = TRUE)

## ----create-package-----------------------------------------------------------
# Create the datapackage.json file
package_file <- data_package(root)
cat("Created:", package_file, "\n")

## ----show-package-------------------------------------------------------------
# Read and display the package file
package_data <- jsonlite::read_json(package_file)

# Show the structure
str(package_data, max.level = 2)

## ----show-schema--------------------------------------------------------------
# Show the schema for the iris dataset
iris_resource <- package_data$resources[[1]]
cat("Resource name:", iris_resource$name, "\n")
cat("Number of fields:", length(iris_resource$schema$fields), "\n\n")

# Show first few fields
for (i in seq_len(min(3, length(iris_resource$schema$fields)))) {
  field <- iris_resource$schema$fields[[i]]
  cat(sprintf(
    "Field %d: %s (type: %s)\n",
    i,
    field$name,
    field$type
  ))
}

## ----csv-required, error=TRUE-------------------------------------------------
try({
# This will fail because optimized files use TSV format
optimized_root <- tempfile("git2rdata-optimized")
dir.create(optimized_root)

write_vc(
  iris,
  file = "iris",
  root = optimized_root,
  sorting = "Species",
  optimize = TRUE # This creates TSV files
)

# This will fail with an error
try(data_package(optimized_root))

unlink(optimized_root, recursive = TRUE)
})

## ----subdirectories-----------------------------------------------------------
# Create a subdirectory
subdir <- file.path(root, "subset")
dir.create(subdir)

# Write data in subdirectory
write_vc(
  head(iris, 50),
  file = file.path("subset", "iris_subset"),
  root = root,
  sorting = "Species",
  optimize = FALSE
)

# Recreate the package - it will include the subdirectory file
data_package(root)

# Check the package contents
package_data <- jsonlite::read_json(package_file)
cat("Number of resources:", length(package_data$resources), "\n")

## ----sharing, eval=FALSE------------------------------------------------------
# # After creating your data files
# write_vc(my_data, "my_data", root = "data", optimize = FALSE)
# 
# # Create the package
# data_package("data")
# 
# # Share the entire 'data' directory
# # Others can now use Frictionless Data tools to read your data

## ----validation, eval=FALSE---------------------------------------------------
# # After creating the package, use frictionless-py or other tools
# # to validate your data package
# system("frictionless validate datapackage.json")

## ----cleanup, include=FALSE---------------------------------------------------
unlink(root, recursive = TRUE)

