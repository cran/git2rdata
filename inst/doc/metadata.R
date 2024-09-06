## ----store-metadata-----------------------------------------------------------
library(git2rdata)
root <- tempfile("git2rdata-metadata")
dir.create(root)
write_vc(iris, file = "iris", root = root, sorting = "Sepal.Length")

## ----read-metadata------------------------------------------------------------
my_iris <- read_vc("iris", root = root)
str(my_iris)
print(head(my_iris))
summary(my_iris)
display_metadata(my_iris)

## ----update-metadata----------------------------------------------------------
update_metadata(
  file = "iris", root = root, name = "iris", title = "Iris dataset",
  description =
"The Iris dataset is a multivariate dataset introduced by the British
statistician and biologist Ronald Fisher in his 1936 paper The use of multiple
measurements in taxonomic problems.",
  field_description = c(
    Sepal.Length = "The length of the sepal in cm",
    Sepal.Width = "The width of the sepal in cm",
    Petal.Length = "The length of the petal in cm",
    Petal.Width = "The width of the petal in cm",
    Species = "The species of the iris"
  )
)
my_iris <- read_vc("iris", root = root)
display_metadata(my_iris)
str(my_iris)

