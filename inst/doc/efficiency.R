## ----setup, include = FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(
  fig.height = 4, fig.width = 6,
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
inbo_colours <- c("#959B38", "#729BB7", "#E87837", "#BDDDD7", "#E4E517",
                  "#843860", "#C04384", "#C2C444", "#685457")
theme_inbo <- function(base_size = 12, base_family = "") {
  rect_bg <- "white"
  legend_bg <- "white"
  panel_bg <- "#F3F3F3"
  panel_grid <- "white"
  plot_bg <- "white"
  half_line <- base_size / 2
  ggplot2::theme(
    line = ggplot2::element_line(colour = "black", size = 0.5, linetype = 1,
                        lineend = "butt"),
    rect = ggplot2::element_rect(fill = rect_bg, colour = "black", size = 0.5,
                        linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain",
                        colour = "#843860", size = base_size, hjust = 0.5,
                        vjust = 0.5, angle = 0, lineheight = 0.9,
                        margin = ggplot2::margin(), debug = FALSE),
    axis.line = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1
    ),
    axis.text.x.top = NULL,
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1
    ),
    axis.text.y.right = NULL,
    axis.ticks = ggplot2::element_line(),
    axis.ticks.length = ggplot2::unit(0.15, "cm"),
    axis.title = ggplot2::element_text(colour = "black"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
    ),
    axis.title.x.top = NULL,
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line, l = 0.8 * half_line / 2),
      angle = 90
    ),
    axis.title.y.right = NULL,
    legend.background = ggplot2::element_rect(colour = NA, fill = legend_bg),
    legend.key = ggplot2::element_rect(fill = panel_bg, colour = NA),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.margin = NULL,
    legend.spacing = ggplot2::unit(0.2, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(
      size = ggplot2::rel(0.8), face = "bold", hjust = 0, colour = "black"
    ),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(
      t = half_line, r = half_line, b = half_line, l = half_line
    ),
    legend.box.background = ggplot2::element_rect(
      colour = NA, fill = legend_bg
    ),
    legend.box.spacing = ggplot2::unit(0.2, "cm"),
    panel.background = ggplot2::element_rect(fill = panel_bg, colour = NA),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = panel_grid),
    panel.grid.minor = ggplot2::element_line(colour = panel_grid, size = 0.25),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = "#8E9DA7", colour = NA),
    strip.text = ggplot2::element_text(
      size = ggplot2::rel(0.8), colour = "#F3F3F3"
    ),
    strip.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line, b = half_line)
    ),
    strip.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = half_line, l = half_line), angle = -90
    ),
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),
    strip.placement = "outside",
    plot.background = ggplot2::element_rect(colour = NA, fill = plot_bg),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, half_line, 0)
    ),
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1), margin = ggplot2::margin(0, 0, half_line, 0)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.6), margin = ggplot2::margin(0, 0, half_line, 0)
    ),
    plot.margin = ggplot2::margin(
      t = half_line, r = half_line, b = half_line, l = half_line
    ),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5
    ),
    plot.tag.position = "topleft",
    complete = TRUE
  )
}
theme_set(theme_inbo())
update_geom_defaults("line", list(colour = "#356196"))
update_geom_defaults("hline", list(colour = "#356196"))
update_geom_defaults("boxplot", list(colour = "#356196"))

## ----download_data, eval = system.file("efficiency", "airbag.rds", package = "git2rdata") == ""----
# airbag <- read.csv(
#   "https://vincentarelbundock.github.io/Rdatasets/csv/DAAG/nassCDS.csv"
# )
# airbag$dead <- airbag$dead == "dead"
# airbag$airbag <- airbag$airbag == "airbag"
# airbag$seatbelt <- airbag$seatbelt == "belted"
# airbag$dvcat <- as.ordered(airbag$dvcat)

## ----load_data, echo = FALSE--------------------------------------------------
if (system.file("efficiency", "airbag.rds", package = "git2rdata") == "") {
  saveRDS(airbag, file.path("..", "inst", "efficiency", "airbag.rds"))
} else {
  airbag <- readRDS(
    system.file("efficiency", "airbag.rds", package = "git2rdata")
  )
}

## ----data_structure-----------------------------------------------------------
str(airbag)

## ----set_tmp_dir--------------------------------------------------------------
library(git2rdata)
root <- tempfile("git2rdata-efficient")
dir.create(root)

## ----file_size----------------------------------------------------------------
write.table(airbag, file.path(root, "base_R.tsv"), sep = "\t")
base_size <- file.size(file.path(root, "base_R.tsv"))

saveRDS(airbag, file.path(root, "base_R.rds"))
rds_size <- file.size(file.path(root, "base_R.rds"))

fn <- write_vc(airbag, "airbag_optimize", root, sorting = "X")
optim_size <- sum(file.size(file.path(root, fn)))

fn <- write_vc(airbag, "airbag_verbose", root, sorting = "X", optimize = FALSE)
verbose_size <- sum(file.size(file.path(root, fn)))

## ----table_file_size, echo = FALSE--------------------------------------------
kable(
  data.frame(
    method = c("saveRDS()", "write_vc(), optimized", "write_vc(), verbose",
               "write.table()"),
    file_size = c(rds_size, optim_size, verbose_size, base_size) / 2 ^ 10,
    relative = c(rds_size, optim_size, verbose_size, base_size) / base_size
  ),
  caption = "Resulting file sizes (in kB) and file sizes relative to the size of
  write.table().",
  digits = 2
)

## ----factor_label_length, echo = FALSE, fig.cap = "Effect of the label length on the efficiency of storing factor optimized, assuming 1000 observations", warning = FALSE----
ratio <- function(label_length = 1:20, n_levels = 9, n_obs = 1000) {
  meta_length <- 63 + (5 + label_length + floor(log10(n_levels))) * n_levels
  optimized <- n_obs * mean(ceiling(log10(seq_len(n_levels) + 1)))
  verbose <- n_obs * label_length
  ifelse(
    62 ^ label_length >= n_levels,
    (optimized + meta_length) / (verbose + meta_length),
    NA
  )
}
lengths <- 1:50
f_ratio <- rbind(
  data.frame(
    label_length = lengths,
    levels = 1000,
    ratio = ratio(lengths, 1000, 1000)
  ),
  data.frame(
    label_length = lengths,
    levels = 100,
    ratio = ratio(lengths, 100, 1000)
  ),
  data.frame(
    label_length = lengths,
    levels = 10,
    ratio = ratio(lengths, 10, 1000)
  ),
  data.frame(
    label_length = lengths,
    levels = 3,
    ratio = ratio(lengths, 3, 1000)
  )
)
f_ratio$levels <- factor(f_ratio$levels, levels = c(1000, 100, 10, 3))
ggplot(f_ratio, aes(x = label_length, y = ratio, colour = levels)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line() +
  scale_x_continuous("label length (characters)") +
  scale_y_continuous(paste("optimized bytes", "verbose bytes", sep = " / "),
                     breaks = seq(0, 1.25, by = 0.25)) +
  scale_colour_manual("number of \nlevels", values = inbo_colours)

## ----factor_observations, echo = FALSE, fig.cap = "Effect of the number of observations on the efficiency of storing factor optimized assuming labels with 10 characters"----
n_obs <- 10 ^ seq(log10(1), log10(10000), length = 41)
f_ratio <- rbind(
  data.frame(
    observations = n_obs,
    levels = 3,
    ratio = sapply(n_obs, ratio, label_length = 10, n_levels = 3)
  ),
  data.frame(
    observations = n_obs,
    levels = 10,
    ratio = sapply(n_obs, ratio, label_length = 10, n_levels = 10)
  ),
  data.frame(
    observations = n_obs,
    levels = 100,
    ratio = sapply(n_obs, ratio, label_length = 10, n_levels = 100)
  ),
  data.frame(
    observations = n_obs,
    levels = 1000,
    ratio = sapply(n_obs, ratio, label_length = 10, n_levels = 1000)
  )
)
f_ratio$levels <- factor(f_ratio$levels, levels = c(1000, 100, 10, 3))
ggplot(f_ratio, aes(x = observations, y = ratio, colour = levels)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line() +
  scale_x_log10() +
  scale_y_continuous(paste("optimized bytes", "verbose bytes", sep = " / "),
                     breaks = seq(0, 1.25, by = 0.25)) +
  scale_colour_manual("number of \nlevels", values = inbo_colours)

## ----git_size, eval = system.file("efficiency", "git_size.rds", package = "git2rdata") == ""----
# library(git2r)
# tmp_repo <- function() {
#   root <- tempfile("git2rdata-efficient-git")
#   dir.create(root)
#   repo <- git2r::init(root)
#   git2r::config(repo, user.name = "me", user.email = "me@me.com")
#   return(repo)
# }
# commit_and_size <- function(repo, filename) {
#   add(repo, filename)
#   commit(repo, "test", session = TRUE)
#   git_size <- system(
#     sprintf("cd %s\ngit gc\ngit count-objects -v", dirname(repo$path)),
#     intern = TRUE
#   )
#   git_size <- git_size[grep("size-pack", git_size)]
#   as.integer(gsub(".*: (.*)", "\\1", git_size))
# }
# 
# repo_wt <- tmp_repo()
# repo_wts <- tmp_repo()
# repo_rds <- tmp_repo()
# repo_wvco <- tmp_repo()
# repo_wvcv <- tmp_repo()
# 
# repo_size <- replicate(
#   100, {
#     observed_subset <- rbinom(nrow(airbag), size = 1, prob = 0.9) == 1
#     this <- airbag[
#       sample(which(observed_subset)),
#       sample(ncol(airbag))
#     ]
#     this_sorted <- airbag[observed_subset, ]
#     fn_wt <- file.path(workdir(repo_wt), "base_R.tsv")
#     write.table(this, fn_wt, sep = "\t")
#     fn_wts <- file.path(workdir(repo_wts), "base_R.tsv")
#     write.table(this_sorted, fn_wts, sep = "\t")
#     fn_rds <- file.path(workdir(repo_rds), "base_R.rds")
#     saveRDS(this, fn_rds)
#     fn_wvco <- write_vc(this, "airbag_optimize", repo_wvco, sorting = "X")
#     fn_wvcv <- write_vc(
#       this, "airbag_verbose", repo_wvcv, sorting = "X", optimize = FALSE
#     )
#     c(
#       write.table = commit_and_size(repo_wt, fn_wt),
#       write.table.sorted = commit_and_size(repo_wts, fn_wts),
#       saveRDS = commit_and_size(repo_rds, fn_rds),
#       write_vc.optimized = commit_and_size(repo_wvco, fn_wvco),
#       write_vc.verbose = commit_and_size(repo_wvcv, fn_wvcv)
#     )
# })

## ----store_git_size, echo = FALSE---------------------------------------------
if (system.file("efficiency", "git_size.rds", package = "git2rdata") == "") {
  saveRDS(repo_size, file.path("..", "inst", "efficiency", "git_size.rds"))
} else {
  repo_size <- readRDS(
    system.file("efficiency", "git_size.rds", package = "git2rdata")
  )
}

## ----plot_git_size, echo = FALSE, fig.cap = "Size of the git history using the different storage methods."----
rs <- lapply(
  rownames(repo_size),
  function(x) {
    if (x == "saveRDS") {
      fun <- "saveRDS"
      optimized <- "yes"
    } else if (x == "write_vc.optimized") {
      fun <- "write_vc"
      optimized <- "yes"
    } else if (x == "write_vc.verbose") {
      fun <- "write_vc"
      optimized <- "no"
    } else if (x == "write.table") {
      fun <- "write.table"
      optimized <- "no"
    } else if (x == "write.table.sorted") {
      fun <- "write.table"
      optimized <- "yes"
    }
    data.frame(commit = seq_along(repo_size[x, ]), size = repo_size[x, ],
               rel_size = repo_size[x, ] / repo_size["write.table.sorted", ],
               fun = fun, optimized = optimized, stringsAsFactors = FALSE)
  }
)
rs <- do.call(rbind, rs)
rs$optimized <- factor(rs$optimized, levels = c("yes", "no"))
ggplot(
  rs, aes(x = commit, y = size / 2^10, colour = fun, linetype = optimized)
) +
  geom_line() +
  scale_y_continuous("repo size (in MiB)") +
  scale_colour_manual("function", values = inbo_colours)

## ----plot_rel_git_size, echo = FALSE, fig.cap = "Relative size of the git repository when compared to write.table()."----
ggplot(rs, aes(x = commit, y = rel_size, colour = fun, linetype = optimized)) +
  geom_line() +
  scale_y_continuous("size relative to sorted write.table()", breaks = 0:10) +
  scale_colour_manual("function", values = inbo_colours)

## ----get_file_timings, eval = system.file("efficiency", "file_timings.rds", package = "git2rdata") == ""----
# library(microbenchmark)
# mb <- microbenchmark(
#   write.table = write.table(airbag, file.path(root, "base_R.tsv"), sep = "\t"),
#   saveRDS = saveRDS(airbag, file.path(root, "base_R.rds")),
#   write_vc.optim = write_vc(airbag, "airbag_optimize", root, sorting = "X"),
#   write_vc.verbose = write_vc(airbag, "airbag_verbose", root, sorting = "X",
#                               optimize = FALSE)
# )
# mb$time <- mb$time / 1e6

## ----store_file_timings, echo = FALSE-----------------------------------------
if (
  system.file("efficiency", "file_timings.rds", package = "git2rdata") == ""
) {
  saveRDS(mb, file.path("..", "inst", "efficiency", "file_timings.rds"))
} else {
  mb <- readRDS(
    system.file("efficiency", "file_timings.rds", package = "git2rdata")
  )
}

## ----median_write, echo = FALSE-----------------------------------------------
median_time <- aggregate(time ~ expr, data = mb, FUN = median)
write_ratio <- 100 * median_time$time /
  median_time$time[median_time$expr == "write.table"]
names(write_ratio) <- median_time$expr

## ----plot_file_timings, echo = FALSE, fig.cap = "Boxplot of the write timings for the different methods."----
mb$expr <- reorder(mb$expr, mb$time, FUN = median)
levels(mb$expr) <- gsub("write_vc\\.", "write_vc\n", levels(mb$expr))
ggplot(mb, aes(x = expr, y = time)) +
  geom_boxplot() +
  scale_y_continuous("Time (in milliseconds)", limits = c(0, NA)) +
  theme(axis.title.x = ggplot2::element_blank())

## ----get_read_timings, eval = system.file("efficiency", "read_timings.rds", package = "git2rdata") == ""----
# mb <- microbenchmark(
#   read.table = read.table(file.path(root, "base_R.tsv"), header = TRUE,
#                           sep = "\t"),
#   readRDS = readRDS(file.path(root, "base_R.rds")),
#   read_vc.optim = read_vc("airbag_optimize", root),
#   read_vc.verbose = read_vc("airbag_verbose", root)
# )
# mb$time <- mb$time / 1e6

## ----store_read_timings, echo = FALSE-----------------------------------------
if (
  system.file("efficiency", "read_timings.rds", package = "git2rdata") == ""
) {
  saveRDS(mb, file.path("..", "inst", "efficiency", "read_timings.rds"))
} else {
  mb <- readRDS(
    system.file("efficiency", "read_timings.rds", package = "git2rdata")
  )
}

## ----median_read, echo = FALSE------------------------------------------------
median_time <- aggregate(time ~ expr, data = mb, FUN = median)
read_ratio <- 100 * median_time$time /
  median_time$time[median_time$expr == "read.table"]
names(read_ratio) <- median_time$expr

## ----plot_read_timings, echo = FALSE, fig.cap = "Boxplots for the read timings for the different methods."----
mb$expr <- factor(
  mb$expr,
  levels = c("readRDS", "read.table", "read_vc.optim", "read_vc.verbose")
)
levels(mb$expr) <- gsub("read_vc\\.", "read_vc\n", levels(mb$expr))
ggplot(mb, aes(x = expr, y = time)) +
  geom_boxplot() +
  scale_y_continuous("Time (in milliseconds)", limits = c(0, NA)) +
  theme(axis.title.x = ggplot2::element_blank())

