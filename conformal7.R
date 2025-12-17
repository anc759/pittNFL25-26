# ------------------------------------------------------------
# BOTH-only Pipeline (throw-aligned -> playerwise conformal -> curves + ranking -> plots)
#   - BOTH mode only (pre + post around throw)
#   - Uses player_name_position.csv for names/positions merge (you already do)
#   - Highlights Top-K players that survive plot filters
#   - Adds Top-K tables next to each plot
#   - Shades ranking windows:
#       PRE:  pre_rank_window  -> "ANTICIPATION"
#       POST: post_rank_window -> "REACTION"
#   - Runs for multiple positions (FS, SS, CB) and stores each plot object
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(patchwork)

# -----------------------------
# Paths
# -----------------------------
data.path <- "NFL26/data/"
utils.path <- "NFL26/script/utils/conformal_util2.R"

# -----------------------------
# Data
# -----------------------------
d.prob <- read.csv(file.path(data.path, "est_prob/probs_dec17.csv"))

player.name.position <- read.csv(file.path(data.path, "player_name_position.csv"))
d.prob <- d.prob %>% left_join(player.name.position, by = "nfl_id")

# Optional lookup csv (your util supports df OR path; you use path)
player_lookup_path <- file.path(data.path, "player_name.csv")
use_names <- TRUE

# Load utils
source(utils.path)

# -----------------------------
# Parameters (global)
# -----------------------------
mode <- "both"  # fixed

# Windows used for ranking
pre_rank_window  <- c(-10, 0)
post_rank_window <- c(0, 10)

# Windows shown in plots (extended view)
pre_plot_window  <- c(-15, 0)
post_plot_window <- c(0, 15)

# Top-K highlight
top_k <- 5

# Minimum plays required for a player to be eligible for conformal
min.play <- 25

# Consistency knobs
rank_min_points <- 8
min_points_plot <- 8

# Conformal knobs
alpha <- 0.1
calib_frac <- 0.5
min_per_trel <- 20

# Y-axis range (keep common scale across pre/post)
y_lim <- c(0, 0.5)

# -----------------------------
# Helper: Top-K table grob (used inside BOTH plot)
# -----------------------------
make_topk_table_grob <- function(rank_long, top_ids, use_names = TRUE, base_size = 8) {

  if (length(top_ids) == 0 || nrow(rank_long) == 0) {
    return(grid::textGrob("(no players)", gp = grid::gpar(fontsize = base_size)))
  }

  tab <- rank_long %>%
    filter(nfl_id %in% top_ids) %>%
    slice_head(n = length(top_ids)) %>%
    mutate(
      Rank   = row_number(),
      Player = if (use_names && "player_name" %in% names(.)) {
        ifelse(is.na(player_name) | player_name == "",
               as.character(nfl_id),
               as.character(player_name))
      } else {
        as.character(nfl_id)
      },
      Score = round(score, 3)
    ) %>%
    select(Rank, Player, Score)

  gridExtra::tableGrob(
    tab,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = base_size)
  )
}

# -----------------------------
# Helper: build the BOTH combined plot for one position
# -----------------------------
build_both_plot_for_position <- function(player.position.choice) {

  # Filter by position
  d_pos <- d.prob
  if (player.position.choice != "All") {
    d_pos <- d_pos %>% filter(player_position == player.position.choice)
  }

  # 1) Throw alignment (BOTH)
  dat_aligned <- prepare_throw_frames(
    data = d_pos,
    mode = "both"
  )

  # 2) Conformal
  cp_player <- conformal_by_trel_playerwise(
    data         = dat_aligned,
    alpha        = alpha,
    calib_frac   = calib_frac,
    min_plays    = min.play,
    min_per_trel = min_per_trel
  )

  # 3) Curves (with names attached if lookup provided)
  curves_all <- summarise_player_cp(
    cp_obj        = cp_player,
    return_rank   = FALSE,
    player_lookup = if (use_names) player_lookup_path else NULL
  )

  # Attach phase to curves
  phase_map <- cp_player$data %>% distinct(nfl_id, t_rel, phase)
  curves_all <- curves_all %>% left_join(phase_map, by = c("nfl_id", "t_rel"))

  # ---- PRE ranking (computed ONLY on pre rows)
  tmp_pre <- summarise_player_cp(
    cp_obj            = list(data = cp_player$data %>% filter(phase == "pre")),
    return_rank       = TRUE,
    rank_t_rel_range  = pre_rank_window,
    rank_top_k        = top_k,
    rank_min_points   = rank_min_points,
    rank_score_fun    = "mean",
    rank_return_names = use_names,
    player_lookup     = if (use_names) player_lookup_path else NULL
  )
  rank_pre_long <- tmp_pre$rank_long

  # ---- POST ranking (computed ONLY on post rows)
  tmp_post <- summarise_player_cp(
    cp_obj            = list(data = cp_player$data %>% filter(phase == "post")),
    return_rank       = TRUE,
    rank_t_rel_range  = post_rank_window,
    rank_top_k        = top_k,
    rank_min_points   = rank_min_points,
    rank_score_fun    = "mean",
    rank_return_names = use_names,
    player_lookup     = if (use_names) player_lookup_path else NULL
  )
  rank_post_long <- tmp_post$rank_long

  # ---- pick Top-K that survive plot filters (PRE)
  ids_pre_in_plot <- curves_all %>%
    filter(phase == "pre", t_rel >= pre_plot_window[1], t_rel <= pre_plot_window[2]) %>%
    count(nfl_id, name = "n_t") %>%
    filter(n_t >= min_points_plot) %>%
    pull(nfl_id)

  top_pre_ids <- rank_pre_long %>%
    filter(nfl_id %in% ids_pre_in_plot) %>%
    slice_head(n = top_k) %>%
    pull(nfl_id)

  # ---- pick Top-K that survive plot filters (POST)
  ids_post_in_plot <- curves_all %>%
    filter(phase == "post", t_rel >= post_plot_window[1], t_rel <= post_plot_window[2]) %>%
    count(nfl_id, name = "n_t") %>%
    filter(n_t >= min_points_plot) %>%
    pull(nfl_id)

  top_post_ids <- rank_post_long %>%
    filter(nfl_id %in% ids_post_in_plot) %>%
    slice_head(n = top_k) %>%
    pull(nfl_id)

  # ---- tables
  pre_table_plot  <- patchwork::wrap_elements(make_topk_table_grob(rank_pre_long,  top_pre_ids,  use_names = use_names))
  post_table_plot <- patchwork::wrap_elements(make_topk_table_grob(rank_post_long, top_post_ids, use_names = use_names))

  # ---- PRE plot + shaded rank window + label
  p_pre <- plot_players_lower_curves_labeled(
    player_cp_curves  = curves_all %>% filter(phase == "pre"),
    t_rel_range       = pre_plot_window,
    min_points        = min_points_plot,
    highlight_top_ids = top_pre_ids,
    n_labelled        = 0,
    label_highlight   = TRUE,
    player_lookup     = if (use_names) player_lookup_path else NULL,
    label_use_names   = use_names,
    title             = "Anticipatory Coverage Reliability"
  ) +
    coord_cartesian(ylim = y_lim) +
    annotate(
      "rect",
      xmin = pre_rank_window[1],
      xmax = pre_rank_window[2],
      ymin = -Inf,
      ymax = Inf,
      fill = "#4C72B0",
      alpha = 0.1
    ) +
    annotate(
      "text",
      x = pre_rank_window[2] - 0.5,
      y = y_lim[2] * 0.98,
      label = "ANTICIPATION",
      hjust = 1, vjust = 1,
      fontface = "bold",
      size = 4,
      color = "#4C72B0"
    )

  # ---- POST plot + shaded rank window + label
  p_post <- plot_players_lower_curves_labeled(
    player_cp_curves  = curves_all %>% filter(phase == "post"),
    t_rel_range       = post_plot_window,
    min_points        = min_points_plot,
    highlight_top_ids = top_post_ids,
    n_labelled        = 0,
    label_highlight   = TRUE,
    player_lookup     = if (use_names) player_lookup_path else NULL,
    label_use_names   = use_names,
    title             = "Reactive Coverage Reliability"
  ) +
    coord_cartesian(ylim = y_lim) +
    annotate(
      "rect",
      xmin = post_rank_window[1],
      xmax = post_rank_window[2],
      ymin = -Inf,
      ymax = Inf,
      fill = "#DD8452",
      alpha = 0.1
    ) +
    annotate(
      "text",
      x = post_rank_window[1] + 0.5,
      y = y_lim[2] * 0.98,
      label = "REACTION",
      hjust = 0, vjust = 1,
      fontface = "bold",
      size = 4,
      color = "#DD8452"
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )

  # ---- layout: [table | PRE]  and  [POST | table]
  combined <- (pre_table_plot + p_pre  + patchwork::plot_layout(widths = c(0.7, 1.3))) |
    (p_post + post_table_plot + patchwork::plot_layout(widths = c(1.3, 0.7)))


  # return everything you might want
  list(
    combined = combined,
    top_pre_ids = top_pre_ids,
    top_post_ids = top_post_ids,
    rank_pre_long = rank_pre_long,
    rank_post_long = rank_post_long
  )
}


add_position_header <- function(p, txt, color = "red") {
  header <- patchwork::wrap_elements(
    grid::textGrob(
      txt, x = 0.5, hjust = 0.5,
      gp = grid::gpar(col = color, fontface = "bold", fontsize = 14)
    )
  )
  header / p + patchwork::plot_layout(heights = c(0.15, 1))
}

add_red_panel_title <- function(p, txt) {
  p + plot_annotation(
    title = txt,
    theme = theme(
      plot.title = element_text(color = "red", face = "bold", hjust = 0.5, size = 14)
    )
  )
}


plots <- list()
source(utils.path)

# TOP panel (keep subplot titles)
# Y-axis range (keep common scale across pre/post)
source(utils.path)
y_lim <- c(0, 0.5)
plots[["CB"]] <- add_position_header(
  build_both_plot_for_position("CB")$combined,
  "Cornerback"
)

# Lower panels: remove subplot titles first, then add header
# Y-axis range (keep common scale across pre/post)
source(utils.path)
y_lim <- c(0, 0.3)
plots[["FS"]] <- add_position_header(
  build_both_plot_for_position("FS")$combined &
    theme(plot.title = element_blank()),
  "Field Safety"
)

# Y-axis range (keep common scale across pre/post)
source(utils.path)
y_lim <- c(0, 0.3)
plots[["SS"]] <- add_position_header(
  build_both_plot_for_position("SS")$combined &
    theme(plot.title = element_blank()),
  "Strong Safety"
)



# robust stacking
stacked <- patchwork::wrap_plots(
  plots[c("CB","FS","SS")],
  ncol = 1
) +
  patchwork::plot_annotation(
    title = "Reliable Ball Coverage Performance of Players across 2023 Season",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )


print(stacked)

