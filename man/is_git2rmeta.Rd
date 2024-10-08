% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_git2rmeta.R
\name{is_git2rmeta}
\alias{is_git2rmeta}
\title{Check Whether a Git2rdata Object Has Valid Metadata.}
\usage{
is_git2rmeta(file, root = ".", message = c("none", "warning", "error"))
}
\arguments{
\item{file}{the name of the git2rdata object. Git2rdata objects cannot
have dots in their name. The name may include a relative path. \code{file} is a
path relative to the \code{root}.
Note that \code{file} must point to a location within \code{root}.}

\item{root}{The root of a project. Can be a file path or a \code{git-repository}.
Defaults to the current working directory (\code{"."}).}

\item{message}{a single value indicating the type of messages on top of the
logical value. \code{"none"}: no messages, \code{"warning"}: issue a warning in case of
an invalid metadata file. \code{"error"}: an invalid metadata file results in an
error. Defaults to \code{"none"}.}
}
\value{
A logical value. \code{TRUE} in case of a valid metadata file. Otherwise
\code{FALSE}.
}
\description{
Valid metadata is a file with \code{.yml} extension. It has a top level item
\code{..generic}. This item contains \code{git2rdata} (the version number), \code{hash} (a
hash on the metadata) and \code{data_hash} (a hash on the data file). The version
number must be the current version.
}
\examples{
# create a directory
root <- tempfile("git2rdata-")
dir.create(root)

# store a file
write_vc(iris[1:6, ], "iris", root, sorting = "Sepal.Length")
# check the stored file
is_git2rmeta("iris", root)
is_git2rdata("iris", root)

# Remove the metadata from the existing git2rdata object. Then it stops
# being a git2rdata object.
junk <- file.remove(file.path(root, "iris.yml"))
is_git2rmeta("iris", root)
is_git2rdata("iris", root)

# recreate the file and remove the data and keep the metadata. It stops being
# a git2rdata object, but the metadata remains valid.
write_vc(iris[1:6, ], "iris", root, sorting = "Sepal.Length")
junk <- file.remove(file.path(root, "iris.tsv"))
is_git2rmeta("iris", root)
is_git2rdata("iris", root)
}
\seealso{
Other internal: 
\code{\link{is_git2rdata}()},
\code{\link{meta}()},
\code{\link{print.git2rdata}()},
\code{\link{summary.git2rdata}()},
\code{\link{upgrade_data}()}
}
\concept{internal}
