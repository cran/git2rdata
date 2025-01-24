## ----setup, include = FALSE------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(20120225)

## ----initialize------------------------------------------------------------------
# initialize a bare git repo to be used as remote
remote <- tempfile("git2rdata-workflow-remote")
remote <- normalizePath(remote, winslash = "/")
dir.create(remote)
git2r::init(remote, bare = TRUE)

# initialize a local git repo
path <- tempfile("git2rdata-workflow")
path <- normalizePath(path, winslash = "/")
dir.create(path)
init_repo <- git2r::clone(remote, path, progress = FALSE)
git2r::config(init_repo, user.name = "me", user.email = "me@me.com")
# add an initial commit with .gitignore file
writeLines("*extra*", file.path(path, ".gitignore"))
git2r::add(init_repo, ".gitignore", force = TRUE)
git2r::commit(init_repo, message = "Initial commit")
# push initial commit to remote
branch_name <- git2r::branches(init_repo)[[1]]$name
git2r::push(
  init_repo, "origin", file.path("refs", "heads", branch_name, fsep = "/")
)
rm(init_repo)

## ----store_data_1----------------------------------------------------------------
library(git2rdata)
repo <- repository(path)
fn <- write_vc(beaver1, "beaver", repo, sorting = "time", stage = TRUE)

## ----avoid_subsecond_commit, echo = FALSE----------------------------------------
Sys.sleep(1.2)

## ----commit_data_1---------------------------------------------------------------
status(repo)
cm1 <- commit(repo, message = "First commit")
cat(cm1$message)

## ----store_data_2----------------------------------------------------------------
fn <- write_vc(beaver2, "extra_beaver", repo, sorting = "time", stage = TRUE)
status(repo)

## ----avoid_subsecond_commit2, echo = FALSE---------------------------------------
Sys.sleep(1.2)

## --------------------------------------------------------------------------------
status(repo, ignored = TRUE)
fn <- write_vc(beaver2, "extra_beaver", repo, sorting = "time", stage = TRUE,
               force = TRUE)
status(repo)
cm2 <- commit(repo, message = "Second commit")

## ----avoid_subsecond_commit3, echo = FALSE---------------------------------------
Sys.sleep(1.2)

## ----store_data_3----------------------------------------------------------------
beaver1$beaver <- 1
beaver2$beaver <- 2
beaver <- rbind(beaver1, beaver2)
fn <- write_vc(beaver, "beaver", repo, sorting = c("beaver", "time"),
               strict = FALSE, stage = TRUE)
file.remove(list.files(path, "extra", full.names = TRUE))
status(repo)
cm3 <- commit(repo, message = "Third commit", all = TRUE)
status(repo)

## ----eval = FALSE----------------------------------------------------------------
# # load package
# library(git2rdata)
# # step 1: setup the repository and data path
# repo <- repository(".")
# data_path <- file.path("data", "beaver")
# # step 1b: sync the repository with the remote
# pull(repo = repo)
# # step 2: remove all existing data files
# rm_data(root = repo, path = data_path, stage = TRUE)
# 
# # step 3: write all relevant git2rdata objects to the data path
# beaver1$beaver <- 1
# beaver2$beaver <- 2
# body_temp <- rbind(beaver1, beaver2)
# fn <- write_vc(x = body_temp, file = file.path(data_path, "body_temperature"),
#                root = repo, sorting = c("beaver", "time"), stage = TRUE)
# 
# # step 4: remove any dangling metadata files
# prune_meta(root = repo, path = data_path, stage = TRUE)
# 
# # step 5: commit the changes
# cm <- commit(repo = repo, message = "import")
# # step 5b: sync the repository with the remote
# push(repo = repo)

## ----eval = FALSE----------------------------------------------------------------
# #' Import the beaver body temperature data
# #' @param path the root of the git repository
# #' @importFrom git2rdata repository pull rm_data write_vc prune_meta commit push
# #' @export
# import_body_temp <- function(path) {
#   # step 1: setup the repository and data path
#   repo <- repository(path)
#   data_path <- file.path("data", "beaver")
#   # step 1b: sync the repository with the remote
#   pull(repo = repo)
#   # step 2: remove all existing data files
#   rm_data(root = repo, path = data_path, stage = TRUE)
# 
#   # step 3: write all relevant git2rdata objects to the data path
#   beaver1$beaver <- 1
#   beaver2$beaver <- 2
#   body_temp <- rbind(beaver1, beaver2)
#   write_vc(x = body_temp, file = file.path(data_path, "body_temperature"),
#                  root = repo, sorting = c("beaver", "time"), stage = TRUE)
# 
#   # step 4: remove any dangling metadata files
#   prune_meta(root = repo, path = data_path, stage = TRUE)
# 
#   # step 5: commit the changes
#   commit(repo = repo, message = "import", session = TRUE)
#   # step 5b: sync the repository with the remote
#   push(object = repo)
# }

## ----standardized_analysis-------------------------------------------------------
analysis <- function(ds_name, repo) {
  ds <- read_vc(ds_name, repo)
  list(
    dataset = ds_name,
    repository = git2r::remote_url(repo),
    commit = recent_commit(ds_name, repo, data = TRUE),
    model = lm(temp ~ activ, data = ds)
  )
}
report <- function(x) {
  knitr::kable(
    coef(summary(x$model)),
    caption = sprintf("**dataset:** %s  \n**commit:** %s  \n**repository:** %s",
                      x$dataset, x$commit$commit, x$repository)
  )
}

## ----run_current_analyses, results = "asis"--------------------------------------
repo <- repository(path)
current <- lapply(list_data(repo), analysis, repo = repo)
names(current) <- list_data(repo)
result <- lapply(current, report)
junk <- lapply(result, print)

## ----run_previous_analyses, results = "asis"-------------------------------------
# checkout first commit
git2r::checkout(cm1)
# do analysis
previous <- lapply(list_data(repo), analysis, repo = repo)
names(previous) <- list_data(repo)
result <- lapply(previous, report)
junk <- lapply(result, print)
# checkout second commit
git2r::checkout(cm2)
# do analysis
previous <- lapply(list_data(repo), analysis, repo = repo)
names(previous) <- list_data(repo)
result <- lapply(previous, report)
junk <- lapply(result, print)

