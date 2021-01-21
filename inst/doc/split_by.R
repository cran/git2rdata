## ----setup, include = FALSE------------------------------------------------------
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
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1,
                        lineend = "butt"),
    rect = element_rect(fill = rect_bg, colour = "black", size = 0.5,
                        linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "#843860", size = base_size, hjust = 0.5,
                        vjust = 0.5, angle = 0, lineheight = 0.9,
                        margin = margin(), debug = FALSE),
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(0.8)),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2),
                               vjust = 1),
    axis.text.x.top = NULL,
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2),
                               hjust = 1),
    axis.text.y.right = NULL,
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.15, "cm"),
    axis.title = element_text(colour = "black"),
    axis.title.x = element_text(
      margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
    ),
    axis.title.x.top = NULL,
    axis.title.y = element_text(
      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2),
      angle = 90
    ),
    axis.title.y.right = NULL,
    legend.background = element_rect(colour = NA, fill = legend_bg),
    legend.key = element_rect(fill = panel_bg, colour = NA),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.margin = NULL,
    legend.spacing = unit(0.2, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0,
                                colour = "black"),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(t = half_line, r = half_line, b = half_line,
                               l = half_line),
    legend.box.background = element_rect(colour = NA, fill = legend_bg),
    legend.box.spacing = unit(0.2, "cm"),
    panel.background = element_rect(fill = panel_bg, colour = NA),
    panel.border = element_blank(),
    panel.grid = element_line(colour = panel_grid),
    panel.grid.minor = element_line(colour = panel_grid, size = 0.25),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "#8E9DA7", colour = NA),
    strip.text = element_text(size = rel(0.8), colour = "#F3F3F3"),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(margin = margin(r = half_line, l = half_line),
                                angle = -90),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    strip.placement = "outside",
    plot.background = element_rect(colour = NA, fill = plot_bg),
    plot.title = element_text(size = rel(1.2),
                              margin = margin(0, 0, half_line, 0)),
    plot.subtitle = element_text(size = rel(1),
                                 margin = margin(0, 0, half_line, 0)),
    plot.caption = element_text(size = rel(0.6),
                                margin = margin(0, 0, half_line, 0)),
    plot.margin = margin(t = half_line, r = half_line, b = half_line,
                         l = half_line),
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    complete = TRUE
  )
}
theme_set(theme_inbo())
update_geom_defaults("line", list(colour = "#356196"))
update_geom_defaults("hline", list(colour = "#356196"))
update_geom_defaults("boxplot", list(colour = "#356196"))
update_geom_defaults("smooth", list(colour = "#356196"))

## ----ratio, fig.cap = "Storage space required using `split_by` relative to storing a single file.", echo = FALSE----
combinations <- expand.grid(
  a = c(0.25, 0.5, 1, 2, 4),
  b = seq(0, 1, length = 41),
  r = c(10, 100, 1000)
)
combinations$ratio <- with(
  combinations,
  (a * b + b + 1) / (a + 1 + 1 / r)
)
ggplot(combinations, aes(x = b, y = ratio, colour = factor(a))) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line() +
  facet_wrap(~ paste("r =", r)) +
  scale_x_continuous(
    expression(b~{"="}~N[s]~{"/"}~N),
    labels = function(x) {
      paste0(100 * x, "%")
    }
  ) +
  scale_y_continuous(
    "Relative amount of disk space",
    labels = function(x) {
      paste0(100 * x, "%")
    }
  ) +
  scale_colour_manual(
    "a = s / r",
    values = inbo_colours,
    labels = c("1/4", "1/2", "1", "2", "4")
  )

## ----load_data, echo = FALSE-----------------------------------------------------
airbag <- readRDS(
  system.file("efficiency", "airbag.rds", package = "git2rdata")
)

## ----set_tmp_dir-----------------------------------------------------------------
library(git2rdata)
root <- tempfile("git2rdata-split-by")
dir.create(root)

## ----get_write_timings, eval = system.file("split_by", "write_timings.rds", package = "git2rdata") == ""----
#  library(microbenchmark)
#  mb <- microbenchmark(
#    part_1 = write_vc(airbag, "part_1", root, sorting = "X"),
#    part_2 = write_vc(airbag, "part_2", root, sorting = "X", split_by = "airbag"),
#    part_3 = write_vc(airbag, "part_3", root, sorting = "X", split_by = "abcat"),
#    part_4 = write_vc(
#      airbag, "part_4", root, sorting = "X", split_by = c("airbag", "sex")
#    ),
#    part_5 = write_vc(airbag, "part_5", root, sorting = "X", split_by = "dvcat"),
#    part_6 = write_vc(
#      airbag, "part_6", root, sorting = "X", split_by = "yearacc"
#    ),
#    part_15 = write_vc(
#      airbag, "part_15", root, sorting = "X", split_by = c("dvcat", "abcat")
#    ),
#    part_45 = write_vc(
#      airbag, "part_45", root, sorting = "X", split_by = "yearVeh"
#    ),
#    part_270 = write_vc(
#      airbag, "part_270", root, sorting = "X", split_by = c("yearacc", "yearVeh")
#    )
#  )
#  mb$time <- mb$time / 1e6

## ----store_write_timings, echo = FALSE-------------------------------------------
if (system.file("split_by", "write_timings.rds", package = "git2rdata") == "") {
  dir.create(file.path("..", "inst", "split_by"), showWarnings = FALSE)
  saveRDS(mb, file.path("..", "inst", "split_by", "write_timings.rds"))
} else {
  mb <- readRDS(
    system.file("split_by", "write_timings.rds", package = "git2rdata")
  )
}

## ----plot_write_timings, echo = FALSE, fig.cap = "Boxplot of the write timings for different number of parts."----
mb$combinations <- as.integer(gsub("part_", "", levels(mb$expr)))[mb$expr]
ggplot(mb, aes(x = combinations, y = time)) +
  geom_boxplot(aes(group = combinations)) +
  scale_x_log10("Number of parts") +
  scale_y_log10("Time (in milliseconds)")

## ----get_read_timings, eval = system.file("split_by", "read_timings.rds", package = "git2rdata") == ""----
#  mb_r <- microbenchmark(
#    part_1 = read_vc("part_1", root),
#    part_2 = read_vc("part_2", root),
#    part_3 = read_vc("part_3", root),
#    part_4 = read_vc("part_4", root),
#    part_5 = read_vc("part_5", root),
#    part_6 = read_vc("part_6", root),
#    part_15 = read_vc("part_15", root),
#    part_45 = read_vc("part_45", root),
#    part_270 = read_vc("part_270", root)
#  )
#  mb_r$time <- mb_r$time / 1e6

## ----store_read_timings, echo = FALSE--------------------------------------------
if (system.file("split_by", "read_timings.rds", package = "git2rdata") == "") {
  saveRDS(mb_r, file.path("..", "inst", "split_by", "read_timings.rds"))
} else {
  mb_r <- readRDS(
    system.file("split_by", "read_timings.rds", package = "git2rdata")
  )
}

## ----plot_read_timings, echo = FALSE, fig.cap = "Boxplot of the read timings for the different number of parts."----
mb_r$combinations <- as.integer(gsub("part_", "", levels(mb_r$expr)))[mb_r$expr]
ggplot(mb_r, aes(x = combinations, y = time)) +
  geom_boxplot(aes(group = combinations)) +
  scale_x_log10("Number of parts") +
  scale_y_log10("Time (in milliseconds)")

