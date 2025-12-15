# ============================================================================
# NFL Big Data Bowl — plotting utilities
# Requires: dplyr, ggplot2; optional: ggrepel (for end labels)
# ============================================================================

#' Simple NFL field painter (full 120 x 53.3 yds)
#'
#' Draws an NFL field background (green turf, end zones, yard lines, hash marks)
#' spanning the full coordinate system [0, 120] x [0, 53.3]. This is a *static*
#' painter; zooming is typically handled by `coord_fixed()` in the caller.
#'
#' @return A `ggplot` object with the field drawn (no data layers).
#' @examples
#' p <- geom_nfl_field(); print(p)
geom_nfl_field <- function() {
  yard_marks <- seq(10, 110, by = 10)

  ggplot2::ggplot() +
    # Turf
    ggplot2::annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 53.3,
                      fill = "#1b5e20", alpha = 0.7) +
    # End zones
    ggplot2::annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 53.3,
                      fill = "#0d3d14", alpha = 0.8) +
    ggplot2::annotate("rect", xmin = 110, xmax = 120, ymin = 0, ymax = 53.3,
                      fill = "#0d3d14", alpha = 0.8) +
    # Yard lines (every 5 yds faint, 10 yds stronger)
    ggplot2::geom_segment(
      data = data.frame(x = seq(0, 120, by = 5)),
      ggplot2::aes(x = x, xend = x, y = 0, yend = 53.3),
      color = "white", linewidth = 0.2, alpha = 0.25
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = yard_marks),
      ggplot2::aes(x = x, xend = x, y = 0, yend = 53.3),
      color = "white", linewidth = 0.5, alpha = 0.7
    ) +
    # Hash marks (approximate placement)
    ggplot2::geom_segment(
      data = data.frame(x = seq(11, 109, by = 1)),
      ggplot2::aes(x = x, xend = x, y = 53.3 - 23/3, yend = 53.3 - 22.5/3),
      color = "white", linewidth = 0.2, alpha = 0.6
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = seq(11, 109, by = 1)),
      ggplot2::aes(x = x, xend = x, y = 23/3, yend = 22.5/3),
      color = "white", linewidth = 0.2, alpha = 0.6
    ) +
    ggplot2::coord_fixed(xlim = c(0, 120), ylim = c(0, 53.3), expand = FALSE) +
    ggplot2::theme_void()
}

#' Degrees → Radians
#' @param x Numeric vector in degrees.
#' @return Numeric vector in radians.
deg2rad <- function(x) x * pi / 180


#' Plot enhanced static player tracks (input + optional output)
#'
#' Draws player movement paths for a play (from `df_tracks`, the "input") and,
#' optionally, a post-throw trajectory for the targeted player (from `df_out`,
#' the "output" data). Supports:
#' - Offense/Defense toggling and contrasting/faceted styles
#' - Direction ticks (small dot + tiny forward line, no arrowheads)
#' - Speed/accel overlays (input only)
#' - Throw & landing markers (only when throw frame is known)
#' - Smart zoom to the play window in both X & Y (fills full rectangle)
#' - Matching output colors to input players (nfl_id join) when `color_by="player"`
#'
#' @param df_tracks Input tracking data (one row per player-frame).
#'   Required columns: `game_id, play_id, nfl_id, frame_id, x, y`.
#'   Optional: `player_name, player_side (Offense/Defense), player_role, dir, o,
#'   play_direction (left/right), s (speed), a (accel)`.
#' @param df_land Optional one-row (or distinct’d) data with `ball_land_x, ball_land_y`.
#' @param df_out Optional output trajectory data for targeted player(s).
#'   Required: `game_id, play_id, nfl_id, frame_id, x, y`.
#' @param show_mode `"both" | "input" | "output"`: which layers to render.
#' @param throw_frame `NULL` (infer last frame for single play), `NA` (unknown),
#'   or integer frame id.
#' @param tick_var `"none" | "dir" | "o"`: use movement heading (`dir`) or body
#'   orientation (`o`) to draw tiny forward lines (input only).
#' @param tick_every Integer; sample every Nth frame for ticks.
#' @param tick_len Numeric; tick forward length in yards.
#' @param tick_anchor `"tail" | "center"`; where to anchor the tiny line.
#' @param trail_style `"ghost_black" | "by_player"`: style for tick dots/lines.
#' @param end_tick_len Length of tiny line at final input position (default = `max(0.3, tick_len)`).
#' @param stat_map `"none" | "speed" | "accel"` overlays for input.
#' @param stat_aes `"color" | "size"` mapping for stat overlay (input).
#' @param color_by `"player" | "team_role" | "frame"` for input path colors.
#' @param team_side `"both" | "offense" | "defense"` filter for input players.
#' @param side_style `"contrast_basic" | "facet"` split style when `team_side="both"`.
#' @param show_end_labels Logical; uses `ggrepel` to label input final positions (offense).
#' @param flip_left Logical; mirror left-moving plays to standardize left→right.
#' @param out_color Color for output layer (used if not matching input colors).
#' @param out_linewidth Line width for output paths.
#' @param out_point_size Point size for output points.
#' @param out_match_input_colors Logical; if `TRUE` and `color_by="player"`,
#'   maps output colors to input `player_label` via `nfl_id`.
#' @param field_extent `"full" | "play_window"`; if `play_window`, zoom to snapped
#'   min/max x/y of the drawn data while still rendering the full field underneath.
#' @param yard_snap Horizontal snapping (yds) when computing the zoom window.
#' @param yard_margin Horizontal margin (yds) before snapping.
#' @param y_snap Vertical snapping (yds) when computing the zoom window.
#' @param y_margin Vertical margin (yds) before snapping.
#' @param title Optional title; if missing, a descriptive title is auto-generated.
#'
#' @return A `ggplot` object.
#' @examples
#' # tracks <- dplyr::filter(d.in,  game_id == gid, play_id == pid)
#' # outs   <- dplyr::filter(d.out, game_id == gid, play_id == pid)
#' # land   <- dplyr::distinct(tracks, game_id, play_id, ball_land_x, ball_land_y)
#' # p <- plot_play_tracks_enhanced(tracks, df_land = land, df_out = outs,
#' #       show_mode = "both", field_extent = "play_window")
#' # print(p)
plot_play_tracks_enhanced <- function(
    df_tracks,
    df_land = NULL,
    df_out  = NULL,                          # post-throw trajectory (target player)
    show_mode = c("both","input","output"),  # what to draw
    throw_frame = NULL,                      # NULL=infer if single play; NA=unknown
    # direction marks (INPUT only)
    tick_var     = c("none", "dir", "o"),
    tick_every   = 5,
    tick_len     = 0.7,
    tick_anchor  = c("tail","center"),
    trail_style  = c("ghost_black","by_player"),
    end_tick_len = NULL,                     # if NULL, uses max(0.3, tick_len)
    # stat overlays (INPUT only)
    stat_map   = c("none", "speed", "accel"),
    stat_aes   = c("color", "size"),
    # base coloring for INPUT paths
    color_by   = c("player", "team_role", "frame"),
    # INPUT side controls
    team_side  = c("both", "offense", "defense"),
    side_style = c("contrast_basic", "facet"),
    show_end_labels = TRUE,
    flip_left = FALSE,
    # OUTPUT styling
    out_color = "goldenrod3",
    out_linewidth = 1.2,
    out_point_size = 1.4,
    out_match_input_colors = TRUE,           # match d.out colors to input
    # FIELD extent controls (zoom box fills full rectangle)
    field_extent = c("full","play_window"),
    yard_snap    = 5,
    yard_margin  = 2,
    y_snap       = 1,
    y_margin     = 2,
    title = NULL
) {
  # ---------------------------- helpers -----------------------------
  show_mode    <- match.arg(show_mode)
  tick_var     <- match.arg(tick_var)
  tick_anchor  <- match.arg(tick_anchor)
  trail_style  <- match.arg(trail_style)
  stat_map     <- match.arg(stat_map)
  stat_aes     <- match.arg(stat_aes)
  color_by     <- match.arg(color_by)
  field_extent <- match.arg(field_extent)

  # Normalize UK spellings for team_side (so "defence"/"offence" also work)
  if (!missing(team_side) && length(team_side)) {
    team_side <- tolower(team_side)
    if (team_side %in% c("defence")) team_side <- "defense"
    if (team_side %in% c("offence")) team_side <- "offense"
  }
  team_side  <- match.arg(team_side)
  side_style <- match.arg(side_style)

  if (is.null(end_tick_len)) end_tick_len <- max(0.3, tick_len)
  has_cols <- function(x, cols) all(cols %in% names(x))
  clamp    <- function(x, lo, hi) pmax(lo, pmin(hi, x))

  # ------------------------ INPUT (df_tracks) ------------------------
  dat_in <- df_tracks %>%
    dplyr::mutate(
      player_label = ifelse(!is.na(player_name) & nzchar(player_name),
                            player_name, paste0("ID ", nfl_id))
    )
  gids <- unique(dat_in$game_id)
  pids <- unique(dat_in$play_id)
  frame_min <- if ("frame_id" %in% names(dat_in)) suppressWarnings(min(dat_in$frame_id, na.rm = TRUE)) else NA
  frame_max <- if ("frame_id" %in% names(dat_in)) suppressWarnings(max(dat_in$frame_id, na.rm = TRUE)) else NA

  # Capture nfl_id → label map BEFORE any side-filtering (used to color df_out)
  player_map <- dat_in %>% dplyr::distinct(nfl_id, player_label)

  # Optional horizontal flip (standardize all plays to L→R using input direction)
  play_moves_left <- has_cols(dat_in, "play_direction") && any(dat_in$play_direction == "left", na.rm = TRUE)
  if (flip_left && play_moves_left) {
    dat_in <- dat_in %>% dplyr::mutate(x = 120 - x)
    if (!is.null(df_land) && has_cols(df_land, c("ball_land_x","ball_land_y"))) {
      df_land <- df_land %>% dplyr::mutate(ball_land_x = 120 - ball_land_x)
    }
  }

  # Side filter for input (if available)
  if ("player_side" %in% names(dat_in)) {
    if (team_side == "offense") dat_in <- dplyr::filter(dat_in, player_side == "Offense")
    if (team_side == "defense") dat_in <- dplyr::filter(dat_in, player_side == "Defense")
  }
  has_side <- "player_side" %in% names(dat_in)
  off_in <- if (has_side) dplyr::filter(dat_in, player_side == "Offense") else dat_in
  def_in <- if (has_side) dplyr::filter(dat_in, player_side == "Defense") else dat_in[0, ]
  n_off <- length(unique(off_in$nfl_id)); n_def <- length(unique(def_in$nfl_id))

  # ---------------------- THROW handling (INPUT) ---------------------
  # NULL → infer last frame if a single play is present; NA → unknown; numeric → use it
  single_play_input <- (length(gids) == 1 && length(pids) == 1 && "frame_id" %in% names(df_tracks))
  if (is.null(throw_frame)) {
    throw_used <- if (single_play_input) {
      suppressWarnings(max(df_tracks$frame_id[df_tracks$game_id == gids & df_tracks$play_id == pids], na.rm = TRUE))
    } else NA_real_
    if (!is.finite(throw_used)) throw_used <- NA_real_
  } else if (length(throw_frame) == 1 && is.na(throw_frame)) {
    throw_used <- NA_real_
  } else {
    throw_used <- throw_frame
  }

  # Landing marker: show only if throw is known and landing coords exist
  land_txt <- NULL; show_landing <- FALSE
  if (!is.na(throw_used) && !is.null(df_land) && has_cols(df_land, c("ball_land_x","ball_land_y"))) {
    land_unique <- df_land %>% dplyr::distinct(ball_land_x, ball_land_y) %>% dplyr::slice(1)
    if (nrow(land_unique) == 1 && is.finite(land_unique$ball_land_x) && is.finite(land_unique$ball_land_y)) {
      land_txt <- sprintf("Ball land: (%.1f, %.1f)", land_unique$ball_land_x, land_unique$ball_land_y)
      show_landing <- TRUE
    }
  }

  # ------------------------ OUTPUT (df_out) --------------------------
  out_ok  <- (!is.null(df_out) && nrow(df_out) > 0 &&
                has_cols(df_out, c("game_id","play_id","nfl_id","frame_id","x","y")))
  dat_out <- if (out_ok) df_out %>% dplyr::filter(game_id %in% gids, play_id %in% pids) else df_out
  if (flip_left && play_moves_left && out_ok) {
    dat_out <- dat_out %>% dplyr::mutate(x = 120 - x)
  }

  # --------------------- FIELD extent (x & y limits) -----------------
  # We *always* draw the full field first, then zoom using coord_fixed(xlim, ylim)
  x_all <- numeric(0); y_all <- numeric(0)
  if (show_mode %in% c("both","input")) {
    if ("x" %in% names(dat_in)) x_all <- c(x_all, dat_in$x)
    if ("y" %in% names(dat_in)) y_all <- c(y_all, dat_in$y)
  }
  if (show_mode %in% c("both","output")) {
    if (out_ok && "x" %in% names(dat_out)) x_all <- c(x_all, dat_out$x)
    if (out_ok && "y" %in% names(dat_out)) y_all <- c(y_all, dat_out$y)
  }
  x_all <- x_all[is.finite(x_all)]; if (length(x_all) == 0) x_all <- c(0, 120)
  y_all <- y_all[is.finite(y_all)]; if (length(y_all) == 0) y_all <- c(0, 53.3)

  if (field_extent == "full") {
    xmin <- 0; xmax <- 120
    ymin <- 0; ymax <- 53.3
  } else {
    # X: crop to nearest yard bars around action (with margin)
    xmin_raw <- min(x_all, na.rm = TRUE) - yard_margin
    xmax_raw <- max(x_all, na.rm = TRUE) + yard_margin
    xmin <- clamp(floor(xmin_raw / yard_snap) * yard_snap, 0, 120)
    xmax <- clamp(ceiling(xmax_raw / yard_snap) * yard_snap, 0, 120)
    if (xmax < xmin + yard_snap) xmax <- clamp(xmin + yard_snap, 0, 120)

    # Y: crop to action (with vertical margin) and snap
    ymin_raw <- min(y_all, na.rm = TRUE) - y_margin
    ymax_raw <- max(y_all, na.rm = TRUE) + y_margin
    ymin <- clamp(floor(ymin_raw / y_snap) * y_snap, 0, 53.3)
    ymax <- clamp(ceiling(ymax_raw / y_snap) * y_snap, 0, 53.3)
    if (ymax < ymin + y_snap) ymax <- clamp(ymin + y_snap, 0, 53.3)
  }

  # ---------------------- Field painter (FULL) -----------------------
  # Paint full field; zoom window is applied via coord_fixed with (xmin..xmax, ymin..ymax)
  field_full <- function() {
    yard_marks5  <- seq(0, 120, by = 5)
    yard_marks10 <- seq(0, 120, by = 10)
    hash_x <- seq(11, 109, by = 1)

    ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 53.3,
                        fill = "#1b5e20", alpha = 0.7) +
      ggplot2::annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 53.3,
                        fill = "#0d3d14", alpha = 0.8) +
      ggplot2::annotate("rect", xmin = 110, xmax = 120, ymin = 0, ymax = 53.3,
                        fill = "#0d3d14", alpha = 0.8) +
      ggplot2::geom_segment(
        data = data.frame(x = yard_marks5),
        ggplot2::aes(x = x, xend = x, y = 0, yend = 53.3),
        color = "white", linewidth = 0.2, alpha = 0.25
      ) +
      ggplot2::geom_segment(
        data = data.frame(x = yard_marks10),
        ggplot2::aes(x = x, xend = x, y = 0, yend = 53.3),
        color = "white", linewidth = 0.5, alpha = 0.7
      ) +
      ggplot2::geom_segment(
        data = data.frame(x = hash_x),
        ggplot2::aes(x = x, xend = x, y = 53.3 - 23/3, yend = 53.3 - 22.5/3),
        color = "white", linewidth = 0.2, alpha = 0.6
      ) +
      ggplot2::geom_segment(
        data = data.frame(x = hash_x),
        ggplot2::aes(x = x, xend = x, y = 23/3, yend = 22.5/3),
        color = "white", linewidth = 0.2, alpha = 0.6
      ) +
      ggplot2::coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      ggplot2::theme_void()
  }

  # ---------------------- Title / Subtitle ---------------------------
  side_phrase <- switch(team_side,
                        "both"    = "Offense + Defense",
                        "offense" = "Offense",
                        "defense" = "Defense")
  what_phrase <- switch(show_mode,
                        "both"   = "Input + Output",
                        "input"  = "Input only",
                        "output" = "Output only")

  if (is.null(title) || !nzchar(title)) {
    id_phrase <-
      if (length(gids) == 1 && length(pids) == 1) {
        sprintf("Game %s • Play %s", as.character(gids), as.character(pids))
      } else {
        sprintf("Games %s • Plays %s",
                if (length(gids) <= 5) paste(gids, collapse = ",") else sprintf("(%d unique)", length(gids)),
                if (length(pids) <= 5) paste(pids, collapse = ",") else sprintf("(%d unique)", length(pids)))
      }
    vis_ids <- unique(dat_in$nfl_id)
    player_bit <- NULL
    if (length(vis_ids) == 1) {
      nm <- dat_in$player_label[match(vis_ids, dat_in$nfl_id)]
      player_bit <- sprintf(" • %s (%s)", nm[1], vis_ids[1])
    }
    title <- sprintf("%s — %s (%s)%s", id_phrase, side_phrase, what_phrase,
                     ifelse(is.null(player_bit), "", player_bit))
  }

  sub_parts <- c(
    if (is.finite(frame_min) && is.finite(frame_max) && show_mode != "output")
      sprintf("Input frames: %d–%d", frame_min, frame_max) else NULL,
    if (show_mode != "output") sprintf("Players: OFF %d, DEF %d", n_off, n_def) else NULL,
    sprintf("Show: %s", what_phrase),
    if (!is.na(throw_used) && show_mode != "output")
      sprintf("Throw frame: %d%s", as.integer(throw_used),
              if (is.null(throw_frame)) " (inferred)" else "") else
                if (show_mode != "output") "Throw frame: unknown",
    if (show_mode != "output") land_txt else NULL,
    if (tick_var != "none" && show_mode != "output")
      sprintf("Marks: %s, len=%.2f, every=%d, anchor=%s",
              tick_var, tick_len, tick_every, tick_anchor) else
                if (show_mode == "output") "Marks: (output only — none)",
    sprintf("Field: %s zoom [x:%g,%g; y:%g,%g]", field_extent, xmin, xmax, ymin, ymax)
  )
  subtitle_auto <- paste(Filter(Negate(is.null), sub_parts), collapse = " • ")

  # ------------------------- base field (FULL) -----------------------
  p <- field_full()

  # ====================== DRAW INPUT (if any) ========================
  if (show_mode %in% c("both","input") && nrow(dat_in) > 0) {

    # Input paths: either facet by side, or overlay with contrast
    if (team_side == "both" && side_style == "facet" && has_side) {
      dat_fac <- dat_in %>%
        dplyr::mutate(Side = factor(player_side, levels = c("Offense","Defense")))

      if (color_by == "player") {
        p <- p + ggplot2::geom_path(
          data  = dat_fac %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id, color = player_label),
          linewidth = 0.7, alpha = 0.9
        ) + ggplot2::guides(color = ggplot2::guide_legend(title = "Player"))

      } else if (color_by == "team_role") {
        dat_fac <- dat_fac %>%
          dplyr::mutate(role_grp = dplyr::coalesce(player_role, player_side, as.character(nfl_id)))

        p <- p + ggplot2::geom_path(
          data  = dat_fac %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id, color = role_grp),
          linewidth = 0.8, alpha = 0.9
        ) + ggplot2::guides(color = ggplot2::guide_legend(title = "Role"))

      } else { # color_by = "frame"
        p <- p + ggplot2::geom_path(
          data  = dat_fac %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id, color = as.numeric(frame_id)),
          linewidth = 0.8, alpha = 0.9
        ) + ggplot2::scale_color_viridis_c(name = "Frame")
      }

    } else {
      # Contrast overlay: offense colorful; defense light dashed
      if (nrow(off_in) > 0) {
        if (color_by == "player") {
          p <- p + ggplot2::geom_path(
            data  = off_in %>% dplyr::arrange(nfl_id, frame_id),
            ggplot2::aes(x = x, y = y, group = nfl_id, color = player_label),
            linewidth = 0.9, alpha = 0.95
          )
        } else if (color_by == "team_role") {
          off_in <- off_in %>%
            dplyr::mutate(role_grp = dplyr::coalesce(player_role, player_side, as.character(nfl_id)))

          p <- p + ggplot2::geom_path(
            data  = off_in %>% dplyr::arrange(nfl_id, frame_id),
            ggplot2::aes(x = x, y = y, group = nfl_id, color = role_grp),
            linewidth = 0.9, alpha = 0.95
          )
        } else { # frame
          p <- p + ggplot2::geom_path(
            data  = off_in %>% dplyr::arrange(nfl_id, frame_id),
            ggplot2::aes(x = x, y = y, group = nfl_id, color = as.numeric(frame_id)),
            linewidth = 0.9, alpha = 0.95
          ) + ggplot2::scale_color_viridis_c(name = "Frame")
        }
      }
      if (nrow(def_in) > 0) {
        p <- p + ggplot2::geom_path(
          data  = def_in %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id),
          linewidth = 0.7, alpha = 0.65, color = "gray80", linetype = "22"
        )
      }
    }

    # Optional stat overlays (INPUT only)
    if (stat_map != "none") {
      if (stat_map == "speed" && "s" %in% names(dat_in)) {
        if (stat_aes == "size") {
          p <- p + ggplot2::geom_point(
            data = dat_in, ggplot2::aes(x = x, y = y, size = s),
            alpha = 0.45, inherit.aes = FALSE
          ) + ggplot2::scale_size_continuous(name = "Speed (yd/s)", range = c(0.4, 2.2))
        } else {
          p <- p + ggplot2::geom_point(
            data = dat_in, ggplot2::aes(x = x, y = y, color = s),
            size = 0.9, alpha = 0.45, inherit.aes = FALSE
          ) + ggplot2::scale_color_viridis_c(name = "Speed (yd/s)")
        }
      } else if (stat_map == "accel" && "a" %in% names(dat_in)) {
        if (stat_aes == "size") {
          p <- p + ggplot2::geom_point(
            data = dat_in, ggplot2::aes(x = x, y = y, size = a),
            alpha = 0.45, inherit.aes = FALSE
          ) + ggplot2::scale_size_continuous(name = "Accel (yd/s^2)", range = c(0.4, 2.2))
        } else {
          p <- p + ggplot2::geom_point(
            data = dat_in, ggplot2::aes(x = x, y = y, color = a),
            size = 0.9, alpha = 0.45, inherit.aes = FALSE
          ) + ggplot2::scale_color_viridis_c(name = "Accel (yd/s^2)")
        }
      }
    }

    # Start/end markers (INPUT); labels use ggrepel if available
    if (nrow(off_in) > 0) {
      off_starts <- off_in %>% dplyr::group_by(nfl_id) %>%
        dplyr::slice_min(frame_id, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
      off_ends <- off_in %>% dplyr::group_by(nfl_id) %>%
        dplyr::slice_max(frame_id, n = 1, with_ties = FALSE) %>% dplyr::ungroup()

      p <- p +
        ggplot2::geom_point(data = off_starts, ggplot2::aes(x = x, y = y),
                            size = 2.2, shape = 21, fill = "white", color = "black") +
        ggplot2::geom_point(data = off_ends, ggplot2::aes(x = x, y = y),
                            size = 2.2, shape = 21, fill = "black", color = "white")

      if (show_end_labels && requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          data = off_ends, ggplot2::aes(x = x, y = y, label = player_label),
          size = 3, max.overlaps = 50, min.segment.length = 0
        )
      }
    }
    if (nrow(def_in) > 0) {
      def_starts <- def_in %>% dplyr::group_by(nfl_id) %>%
        dplyr::slice_min(frame_id, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
      def_ends <- def_in %>% dplyr::group_by(nfl_id) %>%
        dplyr::slice_max(frame_id, n = 1, with_ties = FALSE) %>% dplyr::ungroup()

      p <- p +
        ggplot2::geom_point(data = def_starts, ggplot2::aes(x = x, y = y),
                            size = 2, shape = 24, fill = "white", color = "gray60") +
        ggplot2::geom_point(data = def_ends, ggplot2::aes(x = x, y = y),
                            size = 2, shape = 24, fill = "gray60", color = "white")
    }

    # Direction ticks (INPUT only): dot + tiny forward line, no arrowheads
    if (tick_var != "none" && has_cols(dat_in, c("frame_id","x","y"))) {
      ang_col <- if (tick_var == "dir") "dir" else "o"
      if (ang_col %in% names(dat_in)) {
        make_marks <- function(df) {
          if (nrow(df) == 0) return(df)
          df %>%
            dplyr::arrange(nfl_id, frame_id) %>%
            dplyr::group_by(nfl_id) %>%
            dplyr::filter(dplyr::row_number() %% tick_every == 0) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              theta = deg2rad(.data[[ang_col]]),
              ux = cos(theta), uy = sin(theta),
              x0 = if (tick_anchor == "center") x - (tick_len/2)*ux else x,
              y0 = if (tick_anchor == "center") y - (tick_len/2)*uy else y,
              x1 = if (tick_anchor == "center") x + (tick_len/2)*ux else x + tick_len*ux,
              y1 = if (tick_anchor == "center") y + (tick_len/2)*uy else y + tick_len*uy
            )
        }
        off_marks <- make_marks(off_in)
        def_marks <- make_marks(def_in)

        if (nrow(off_marks) > 0) {
          if (trail_style == "by_player") {
            p <- p +
              ggplot2::geom_point(
                data = off_marks,
                ggplot2::aes(x = x, y = y, color = player_label, group = nfl_id),
                size = 1.4, alpha = 0.25
              ) +
              ggplot2::geom_segment(
                data = off_marks,
                ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1,
                             color = player_label, group = nfl_id),
                linewidth = 0.45, alpha = 0.40
              )
          } else { # ghost_black
            p <- p +
              ggplot2::geom_point(
                data = off_marks,
                ggplot2::aes(x = x, y = y, group = nfl_id),
                shape = 21, fill = "black", color = "black",
                size = 1.6, alpha = 0.22, stroke = 0.2
              ) +
              ggplot2::geom_segment(
                data = off_marks,
                ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, group = nfl_id),
                color = "black", linewidth = 0.40, alpha = 0.35
              )
          }
        }
        if (nrow(def_marks) > 0) {
          p <- p +
            ggplot2::geom_point(
              data = def_marks,
              ggplot2::aes(x = x, y = y, group = nfl_id),
              shape = 21, fill = "gray70", color = "gray70",
              size = 1.4, alpha = 0.20, stroke = 0.2
            ) +
            ggplot2::geom_segment(
              data = def_marks,
              ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, group = nfl_id),
              color = "gray70", linewidth = 0.35, alpha = 0.30
            )
        }
      }
    }

    # Tiny forward line at FINAL input position (one per player)
    ang_col_final <- if (tick_var == "dir") "dir" else if (tick_var == "o") "o" else NULL
    if (!is.null(ang_col_final) && ang_col_final %in% names(dat_in)) {
      end_marks <- function(df, len = end_tick_len) {
        if (nrow(df) == 0) return(df[0, ])
        df %>%
          dplyr::group_by(nfl_id) %>%
          dplyr::slice_max(frame_id, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            theta = deg2rad(.data[[ang_col_final]]),
            x0 = x, y0 = y,
            x1 = x + len*cos(theta),
            y1 = y + len*sin(theta)
          )
      }
      if (nrow(off_in) > 0) {
        if (trail_style == "by_player") {
          p <- p + ggplot2::geom_segment(
            data = end_marks(off_in),
            ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1,
                         color = player_label, group = nfl_id),
            linewidth = 0.55, alpha = 0.55
          )
        } else {
          p <- p + ggplot2::geom_segment(
            data = end_marks(off_in),
            ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, group = nfl_id),
            color = "black", linewidth = 0.55, alpha = 0.55
          )
        }
      }
      if (nrow(def_in) > 0) {
        p <- p + ggplot2::geom_segment(
          data = end_marks(def_in),
          ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, group = nfl_id),
          color = "gray60", linewidth = 0.5, alpha = 0.5
        )
      }
    }

    # Throw & landing (input only), but *only* when throw frame is known
    if (!is.na(throw_used)) {
      at_throw <- dat_in %>% dplyr::filter(frame_id == throw_used)
      if (nrow(at_throw) > 0) {
        p <- p + ggplot2::geom_point(
          data = at_throw, ggplot2::aes(x = x, y = y),
          inherit.aes = FALSE, size = 3.2, shape = 21,
          fill = "gold", color = "black", stroke = 0.6
        )
      }
      if (show_landing) {
        p <- p + ggplot2::geom_point(
          data = df_land, ggplot2::aes(x = ball_land_x, y = ball_land_y),
          inherit.aes = FALSE, size = 3.2, shape = 4, stroke = 1.2, color = "yellow"
        )
      }
    }
  }

  # ====================== DRAW OUTPUT (if any) =======================
  if (show_mode %in% c("both","output") && out_ok && nrow(dat_out) > 0) {
    if (out_match_input_colors && color_by == "player") {
      dat_out_col <- dat_out %>%
        dplyr::left_join(player_map, by = "nfl_id") %>%
        dplyr::mutate(player_label = dplyr::coalesce(player_label, paste0("ID ", nfl_id)))
      p <- p +
        ggplot2::geom_path(
          data = dat_out_col %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id, color = player_label),
          linewidth = out_linewidth, alpha = 0.95
        ) +
        ggplot2::geom_point(
          data = dat_out_col,
          ggplot2::aes(x = x, y = y, group = nfl_id, color = player_label),
          alpha = 0.95, size = out_point_size
        )
    } else {
      p <- p +
        ggplot2::geom_path(
          data = dat_out %>% dplyr::arrange(nfl_id, frame_id),
          ggplot2::aes(x = x, y = y, group = nfl_id),
          linewidth = out_linewidth, color = out_color, alpha = 0.95
        ) +
        ggplot2::geom_point(
          data = dat_out,
          ggplot2::aes(x = x, y = y, group = nfl_id),
          color = out_color, alpha = 0.9, size = out_point_size
        )
    }
  }

  # ------------------------- labels/theme ----------------------------
  p + ggplot2::labs(title = title, subtitle = subtitle_auto, x = NULL, y = NULL) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

## =============================================================================
# VORONOI — SIMPLE "WORKING" VERSION (geom_sf + manual fill)
# Add this AFTER your base ggplot so it sets the final coords.
# =============================================================================

# 1) Build Voronoi polygons for a given frame (clipped to field)
nfl_build_voronoi <- function(
    df_tracks,
    frame,
    field_xlim = c(0, 120),
    field_ylim = c(0, 53.3),
    team_side  = c("both","offense","defense")
){
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. install.packages('sf')")
  }
  team_side <- match.arg(team_side)

  # Filter points
  pts <- df_tracks[df_tracks$frame_id == frame, , drop = FALSE]
  if (nrow(pts) == 0) stop(sprintf("No rows in frame %s.", frame))
  if ("player_side" %in% names(pts)) {
    if (team_side == "offense") pts <- pts[pts$player_side == "Offense", , drop = FALSE]
    if (team_side == "defense") pts <- pts[pts$player_side == "Defense", , drop = FALSE]
  }
  if (nrow(pts) == 0) stop(sprintf("No points after team_side='%s' at frame %s.", team_side, frame))

  # Labels
  pts$player_label <- ifelse(!is.na(pts$player_name) & nzchar(pts$player_name),
                             pts$player_name, paste0("ID ", pts$nfl_id))

  # SF points (planar, no CRS)
  sf_pts <- sf::st_as_sf(pts, coords = c("x","y"), remove = FALSE, crs = sf::NA_crs_)

  # Field rectangle
  rect_mat <- matrix(
    c(field_xlim[1], field_ylim[1],
      field_xlim[1], field_ylim[2],
      field_xlim[2], field_ylim[2],
      field_xlim[2], field_ylim[1],
      field_xlim[1], field_ylim[1]), byrow = TRUE, ncol = 2
  )
  field_poly <- sf::st_sfc(sf::st_polygon(list(rect_mat)), crs = sf::NA_crs_)

  # Voronoi inside envelope -> polygons -> clip -> valid
  vor_raw   <- sf::st_voronoi(sf::st_union(sf_pts), envelope = field_poly)
  vor_polys <- sf::st_collection_extract(vor_raw, "POLYGON")
  vor_sf    <- sf::st_sf(geometry = vor_polys)
  vor_sf    <- suppressWarnings(sf::st_intersection(vor_sf, field_poly))
  vor_sf    <- suppressWarnings(sf::st_make_valid(vor_sf))

  # Map back each polygon to nearest generating point
  idx <- sf::st_nearest_feature(vor_sf, sf_pts)
  vor_sf <- sf::st_set_geometry(
    cbind(
      sf::st_drop_geometry(sf_pts[idx, c("nfl_id","player_label","player_side","player_role","frame_id")]),
      vor_sf
    ),
    sf::st_geometry(vor_sf)
  )
  vor_sf
}

# 2) Add Voronoi overlay (team OR player colors), using geom_sf + manual scale
#    IMPORTANT: This function **sets coord_sf**; add it LAST in your pipeline.
nfl_add_voronoi_sf <- function(
    p,
    vor_sf,
    fill_by      = c("team","player","none"),
    alpha        = 0.30,
    border_color = "white",
    border_size  = 0.3,
    # palettes
    team_colors  = c(Offense = "#1565C0", Defense = "#EF5350", Unknown = "#9E9E9E"),
    player_palette = NULL,
    # coord (match your field)
    xlim = c(0, 120),
    ylim = c(0, 53.3)
){
  fill_by <- match.arg(fill_by)

  # Base: unfilled polygons (if requested)
  if (fill_by == "none") {
    layer <- ggplot2::geom_sf(
      data  = vor_sf,
      fill  = scales::alpha("grey70", alpha),
      color = border_color, linewidth = border_size,
      inherit.aes = FALSE
    )
    return(
      p + layer +
        ggplot2::coord_sf(default_crs = sf::NA_crs_, expand = FALSE, xlim = xlim, ylim = ylim)
    )
  }

  if (fill_by == "team") {
    # normalize sides
    vor_sf$player_side <- as.character(vor_sf$player_side)
    vor_sf$player_side[is.na(vor_sf$player_side)] <- "Unknown"
    vor_sf$player_side[!vor_sf$player_side %in% c("Offense","Defense","Unknown")] <- "Unknown"

    # subset palette to present levels (but keep both in legend)
    present <- intersect(names(team_colors), unique(vor_sf$player_side))
    pal_use <- team_colors
    if (!all(unique(vor_sf$player_side) %in% names(pal_use))) {
      missing <- setdiff(unique(vor_sf$player_side), names(pal_use))
      pal_use <- c(pal_use, stats::setNames(rep("#9E9E9E", length(missing)), missing))
    }

    return(
      p +
        ggplot2::geom_sf(
          data = vor_sf,
          ggplot2::aes(fill = player_side),
          color = border_color, linewidth = border_size, alpha = alpha, inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_manual("Side", values = pal_use, drop = FALSE) +
        ggplot2::coord_sf(default_crs = sf::NA_crs_, expand = FALSE, xlim = xlim, ylim = ylim)
    )
  }

  # fill_by == "player"
  players <- sort(unique(vor_sf$player_label))
  cols <- if (is.null(player_palette)) {
    if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(players)) else grDevices::hcl.colors(length(players), "Set3")
  } else if (is.function(player_palette)) {
    player_palette(length(players))
  } else if (is.character(player_palette) && length(player_palette) == 1) {
    # interpret as hcl palette name
    tryCatch(grDevices::hcl.colors(length(players), palette = player_palette),
             error = function(...) grDevices::rainbow(length(players)))
  } else if (is.character(player_palette) && length(player_palette) >= length(players)) {
    player_palette[seq_along(players)]
  } else {
    grDevices::rainbow(length(players))
  }
  pal_players <- stats::setNames(cols, players)

  p +
    ggplot2::geom_sf(
      data = vor_sf,
      ggplot2::aes(fill = player_label),
      color = border_color, linewidth = border_size, alpha = alpha, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual("Player (Voronoi)", values = pal_players, drop = FALSE) +
    ggplot2::coord_sf(default_crs = sf::NA_crs_, expand = FALSE, xlim = xlim, ylim = ylim)
}

# 3) Convenience: build (for a frame) + overlay; optionally append " (frame X)" to title
nfl_add_voronoi <- function(
    p,
    df_tracks,
    frame,
    fill_by      = c("team","player","none"),
    team_side    = c("both","offense","defense"),
    alpha        = 0.30,
    border_color = "white",
    border_size  = 0.3,
    team_colors  = c(Offense = "#1565C0", Defense = "#EF5350", Unknown = "#9E9E9E"),
    player_palette = NULL,
    xlim = c(0, 120),
    ylim = c(0, 53.3),
    append_frame_in_title = TRUE
){
  fill_by   <- match.arg(fill_by)
  team_side <- match.arg(team_side)

  vor_sf <- nfl_build_voronoi(
    df_tracks  = df_tracks,
    frame      = frame,
    field_xlim = xlim,
    field_ylim = ylim,
    team_side  = team_side
  )

  # Append "(frame N)" once
  if (append_frame_in_title) {
    cur_title <- ggplot2::ggplot_build(p)$plot$labels$title
    if (is.null(cur_title) || !nzchar(cur_title)) cur_title <- ""
    tag <- sprintf(" (frame %d)", frame)
    if (!grepl("\\(frame \\d+\\)$", cur_title)) {
      p <- p + ggplot2::labs(title = paste0(cur_title, tag))
    }
  }

  nfl_add_voronoi_sf(
    p, vor_sf,
    fill_by        = fill_by,
    alpha          = alpha,
    border_color   = border_color,
    border_size    = border_size,
    team_colors    = team_colors,
    player_palette = player_palette,
    xlim = xlim, ylim = ylim
  )
}

speed_acceleration_direction <- function(
    data,
    fps        = 10,
    group_cols = c("game_id", "play_id", "nfl_id"),
    x_col      = "x",
    y_col      = "y",
    speed_col  = "s",      # name for speed column
    accel_col  = "a",      # name for acceleration column
    dir_col    = "dir"     # name for direction column (NEW)
) {
  
  # time step between frames
  dt <- 1 / fps
  
  # symbols for tidy evaluation
  x_sym      <- rlang::sym(x_col)
  y_sym      <- rlang::sym(y_col)
  speed_sym  <- rlang::sym(speed_col)
  accel_sym  <- rlang::sym(accel_col)
  dir_sym    <- rlang::sym(dir_col)
  
  data %>%
    # sort by group and frame so differences make sense
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "frame_id")))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      # frame-to-frame displacement in yards
      .dx = !!x_sym - dplyr::lag(!!x_sym),
      .dy = !!y_sym - dplyr::lag(!!y_sym),

      # speed (yards / second)
      .s_new = pmin(sqrt(.dx^2 + .dy^2) / dt, 18)
    ) %>%
  dplyr::mutate(

       # acceleration (yards / second^2) = diff(speed) / dt
      .a_new = (.s_new - dplyr::lag(.s_new)) / dt,

      # direction
      .dir_raw = atan2(.dx, .dy) * 180 / pi,
      .dir_new = (.dir_raw + 360) %% 360
    ) %>%
    
    # apply only if column is NA
    dplyr::mutate(
      !!speed_sym := dplyr::if_else(is.na(!!speed_sym), .s_new, !!speed_sym),
      !!accel_sym := dplyr::if_else(is.na(!!accel_sym), .a_new, !!accel_sym),
      !!dir_sym   := dplyr::if_else(is.na(!!dir_sym), .dir_new, !!dir_sym)
    ) %>%
  
  dplyr::select(-dplyr::starts_with(".")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    s = pmin(s, 25),
    a = pmax(pmin(a, 20), -20)
  )
}

# For computing closing speed
compute_closing_speed <- function(
    x1, y1, s1, dir1,
    x2, y2, s2, dir2
) {

  # Get vector components
  rad1 <- (90 - dir1) * pi / 180
  rad2 <- (90 - dir2) * pi / 180
  v1x <- s1 * cos(rad1)
  v1y <- s1 * sin(rad1)
  v2x <- s2 * cos(rad2)
  v2y <- s2 * sin(rad2)

  # Vector differences
  dvx <- v2x - v1x
  dvy <- v2y - v1y
  
  # Displacement
  dx <- x2 - x1
  dy <- y2 - y1
 
  distance <- sqrt(dx^2 + dy^2)
  if (distance == 0) {
    return(0)
  }
  
  ux <- dx / distance
  uy <- dy / distance
  
  s_rel <- (dvx * ux) + (dvy * uy)
  
  return(s_rel)
}

# Function for computing targeted reciever voronoi features
# focus_pt is either tr for targeted receiver or ball for ball landing location
compute_tr_voronoi <- function(
    df_tracks,
    focus_pt = "tr",         
    field_xlim = c(0, 120),
    field_ylim = c(0, 53.3)
) {

  # Define field
  rect_mat <- matrix(
    c(field_xlim[1], field_ylim[1],
      field_xlim[1], field_ylim[2],
      field_xlim[2], field_ylim[2],
      field_xlim[2], field_ylim[1],
      field_xlim[1], field_ylim[1]), 
    byrow = TRUE, ncol = 2
  )
  field_poly <- sf::st_sfc(sf::st_polygon(list(rect_mat)), crs = sf::NA_crs_)

  get_frame_voronoi_area <- function(curr_frame_data) {

    # If focus is targeted reciever, make that targeted row
    if (focus_pt == 'tr'){
      target_row <- curr_frame_data[curr_frame_data$player_role == 'Targeted Receiver', ]
    }
    # Otherwise, focus on ball landing location and add it to voronoi area calculation
    else {
      landing_x <- curr_frame_data$ball_land_x[1]
      landing_y <- curr_frame_data$ball_land_y[1]
      ball_landing_row <- curr_frame_data[1,]
      ball_landing_row$x <- landing_x
      ball_landing_row$y <- landing_y
      ball_landing_row$nfl_id <- -999L 
      ball_landing_row$player_role <- 'Ball Landing Spot'
      target_row <- ball_landing_row
      curr_frame_data <- rbind(curr_frame_data, ball_landing_row)
    }
    
    sf_pts <- sf::st_as_sf(curr_frame_data, coords = c("x", "y"), remove = FALSE, crs = sf::NA_crs_)
    vor_raw <- sf::st_voronoi(sf::st_union(sf_pts), envelope = field_poly)
    vor_polys <- sf::st_collection_extract(vor_raw, "POLYGON")
    vor_sf <- sf::st_sf(geometry = vor_polys)
    vor_sf <- suppressWarnings(sf::st_intersection(vor_sf, field_poly))
    idx <- sf::st_nearest_feature(vor_sf, sf_pts)
    target_index <- if (focus_pt == "tr") {
      which(curr_frame_data$player_role == "Targeted Receiver")
    } else {
      which(curr_frame_data$player_role == "Ball Landing Spot")
    }

    target_poly <- vor_sf[idx == target_index, ]

    if (focus_pt == "tr" && nrow(target_poly) > 0) {

      tr_x <- target_row$x
      play_dir <- target_row$play_direction

      if (play_dir == "right") {
        xmin <- max(field_xlim[1], tr_x - 5)
        xmax <- field_xlim[2]
      } else if (play_dir == "left") {
        xmin <- field_xlim[1]
        xmax <- min(field_xlim[2], tr_x + 5)
      } else {
        return(data.frame(voronoi_area = NA_real_))
      }
      
      forward_rect <- matrix(
        c(xmin, field_ylim[1],
          xmin, field_ylim[2],
          xmax, field_ylim[2],
          xmax, field_ylim[1],
          xmin, field_ylim[1]),
        byrow = TRUE, ncol = 2
      )

      forward_poly <- sf::st_sfc(
        sf::st_polygon(list(forward_rect)),
        crs = sf::NA_crs_
      )
      target_poly <- suppressWarnings(
        sf::st_intersection(target_poly, forward_poly)
      )
    }

    area_val <- sum(sf::st_area(target_poly))
    return(data.frame(voronoi_area = as.numeric(area_val)))
  }

  message('Computing Forward Voronoi areas, this may take a while')
  library(data.table)
  dt <- as.data.table(df_tracks)
  result_df <- dt[, get_frame_voronoi_area(.SD), by = .(game_id, play_id, frame_id)]

  return(result_df)
 }  

calculate_gaussian_density <- function(p_x, p_y, p_s, p_dir, t_x, t_y) {
  # Create scaling matrix
  max_speed <- 18
  influence_radius <- 3
  ratio <- p_s / max_speed
  sx <- max(influence_radius + (influence_radius * ratio), .000001)
  sy <- max(influence_radius - (influence_radius * ratio), .000001)
  val_x <- sx / 2
  val_y <- sy / 2
  S_mat <- matrix(c(val_x, 0, 0, val_y), 2, 2)

  # Make rotation matrix
  theta <- (p_dir-90) * (pi / 180)
  c_t <- cos(theta)
  s_t <- sin(theta)
  R_mat <- matrix(c(c_t, s_t, -s_t, c_t), 2, 2)
  
  # Make covariance matrix
  Cov <- R_mat %*% (S_mat %*% S_mat) %*% t(R_mat)
  
  # Compute mean of gaussian
  mu_x <- p_x + (0.5 * p_s * cos(theta))
  mu_y <- p_y + (0.5 * p_s * sin(theta))

  # Calculate density
  diff_vec <- c(t_x - mu_x, t_y - mu_y)
  det_cov  <- det(Cov)
  cov_inv <- solve(Cov)
  mahal <- t(diff_vec) %*% cov_inv %*% diff_vec
  density <- (1 / (2 * pi * sqrt(det_cov))) * exp(-0.5 * mahal)
  return(as.numeric(density))
}
calc_influence_v <- Vectorize(calculate_gaussian_density)

# For computing team influence on targeted reciever
compute_team_influence_on_tr <- function(df_tracks) {
  
  # Remove targeted receivers from influence calculation
  df_calc <- df_tracks %>%
    filter(player_role != "Targeted Receiver")

  # Compute influence for every player on targeted reciever
  df_calc <- df_calc %>%
    mutate(
      influence_value = calc_influence_v(x, y, s, dir, target_x, target_y)
    )

  # Compute team influence
  result <- df_calc %>%
    group_by(game_id, play_id, frame_id, player_side) %>%
    summarise(total_inf = sum(influence_value), .groups = "drop") %>%
    pivot_wider(
      names_from = player_side, 
      values_from = total_inf, 
      values_fill = 0
    )  %>%
    mutate(
    Offense  = ifelse(is.na(Offense), 0, Offense),
    Defense  = ifelse(is.na(Defense), 0, Defense),
    tr_net_influence = Offense - Defense
    )

  return(result)
}

# For computing team influence on ball landing location
compute_team_influence_on_ball_land <- function(df_tracks) {

  # Compute influence for every player on ball landing location
  df_calc <- df_tracks %>%
    mutate(
      influence_value = calc_influence_v(x, y, s, dir, ball_land_x, ball_land_y)
    )

  # Compute team influence
  result <- df_calc %>%
    group_by(game_id, play_id, frame_id, player_side) %>%
    summarise(total_inf = sum(influence_value), .groups = "drop") %>%
    pivot_wider(
      names_from = player_side, 
      values_from = total_inf, 
      values_fill = 0
    ) %>%
    mutate(
      Offense  = ifelse(is.na(Offense), 0, Offense),
      Defense  = ifelse(is.na(Defense), 0, Defense),
      ball_net_influence = Offense - Defense
    )
  return(result)
}

# Next a function for computing the offensive influence on the defender besides targeted reciever
compute_blocker_influence_on_defenders <- function(df_tracks) {
  
  # Find potential offensive blockers excluding targeted receiver
  blockers <- df_tracks %>%
    filter(
      player_side == 'Offense',
      player_role != 'Targeted Receiver'
    ) %>%
    select(game_id, play_id, frame_id, 
           blocker_nfl_id = nfl_id, 
           blocker_x = x, blocker_y = y, 
           blocker_s = s, blocker_dir = dir)

  # Grab defensive players that are involved later in play
  true_defender_ids <- df_tracks %>%
    filter(
      player_side == 'Defense',
      output == TRUE
    ) %>%
    distinct(game_id, play_id, nfl_id)
  defenders <- df_tracks %>%
    filter(player_side == "Defense") %>%
    semi_join(true_defender_ids, by = c("game_id", "play_id", "nfl_id")) %>%
    select(game_id, play_id, frame_id, 
           defender_nfl_id = nfl_id, 
           defender_x = x, defender_y = y)
  
  # Match blockers with defenders across all plays and frames
  pair_df <- defenders %>%
    inner_join(blockers, by = c("game_id", "play_id", "frame_id"), relationship = "many-to-many")

  # Calculate offense influence on each individual defender
  pair_df <- pair_df %>%
    mutate(
      influence_at_defender = calc_influence_v(
        p_x = blocker_x, p_y = blocker_y, p_s = blocker_s, p_dir = blocker_dir,
        t_x = defender_x, t_y = defender_y
      )
    )

  # Aggregate influence
  influence_summary <- pair_df %>%
    group_by(game_id, play_id, frame_id, defender_nfl_id, defender_x, defender_y) %>%
    summarise(
      blocker_influence = sum(influence_at_defender, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(influence_summary)
}



####################
# Code to plot field and probs basd on randomly sampled game and play id.
# Requires input and output gameplay data.
plot_field_and_probs <- function(df,
                                 features,
                                 response,
                                 fit,
                                 game_id = NULL,
                                 play_id = NULL){

  game_id_rand <- game_id
  play_id_rand <- play_id
  
  if (is.null(game_id) || is.null(play_id)) {
    # Select random game id and play id
    game_id_rand <- df %>% select(game_id) %>% unname() %>% unlist() %>%  as.vector() %>% unique()  %>% sample(1)
    play_id_rand <- df %>% filter(game_id == game_id_rand) %>% select(play_id) %>%
      unname() %>% unlist() %>% as.vector() %>% unique() %>% sample(1)
  }

  # Get field plot
  tracks <- df  %>% dplyr::filter(game_id == game_id_rand, play_id == play_id_rand, output == FALSE)
  outs   <- df %>% dplyr::filter(game_id == game_id_rand, play_id == play_id_rand, output == TRUE)
  land   <- tracks %>% dplyr::distinct(game_id == game_id_rand, play_id == play_id_rand, ball_land_x, ball_land_y)
  
  p_field <- plot_play_tracks_enhanced(
    df_tracks = tracks,
    df_land = land,
    df_out = outs,
    show_mode = "both",
    throw_frame = NULL,
    color_by = "player",
    team_side = "both",
    field_extent = "full"
  )
  
  # Grab ids of players
  players_in_play <- df %>% 
    dplyr::filter(game_id == game_id_rand, play_id == play_id_rand) %>%
    pull(nfl_id) %>% unique()

  plot_player_probs <- function(player_id) {
    df_sub <- df %>%
      dplyr::filter(
        nfl_id == player_id,
        game_id == game_id_rand,
        play_id == play_id_rand
      )

    X.sub <- df_sub[, features]
    preds <- predict_model(fit, X.sub)$prob
    player_name <- unique(df_sub$player_name)
    outcome <- unique(df_sub[, response])

    plot_df <- data.frame(frame_id = df_sub$frame_id,  pred = preds) %>% arrange(frame_id)
    split_frame <- max(df_sub$frame_id[df_sub$output == FALSE])
    
    # ggplot panel
    return(ggplot(plot_df, aes(x = frame_id, y = pred)) +
             geom_line() +
             geom_point(size = 1.8) +
             geom_vline(xintercept = split_frame, linetype = "dashed") +
             labs(
               title = paste0(player_name, " (", outcome, ")"),
               x = "Frame ID", y = "Pred Prob"
             ) +
             theme_minimal(base_size = 11) +
             scale_y_continuous(limits = c(0,1)))
  }

  prob_plots <- lapply(players_in_play, plot_player_probs)
  prob_plots <- prob_plots[!sapply(prob_plots, is.null)]  # remove empty
  prob_grid <- wrap_plots(prob_plots, ncol = 2)
  return(p_field / prob_grid + plot_layout(heights = c(1.5, 2)))
}



# Functions for filtering the data
#####################################

# Filter based on win probability
filter_by_wp <- function(meta, input, lower = 0.05, upper = 0.95) {
  meta_filtered <- meta %>%
    filter(
      pre_snap_home_team_win_probability > lower &
      pre_snap_home_team_win_probability < upper
    )

  input_filtered <- input %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))
 return(input_filtered)
}

# Remove plays that are near the end of a half
filter_near_end_of_half <- function(meta, input, min_time_left = 30) {
  meta_with_time <- meta %>%
    mutate(
      time_left_qtr = as.numeric(substr(game_clock, 1, 2)) * 60 +
                      as.numeric(substr(game_clock, 4, 5)),
      time_left_half = case_when(
        quarter %in% c(1, 3) ~ time_left_qtr + 900,
        TRUE ~ time_left_qtr
      )
    )
  meta_filtered <- meta_with_time %>%
    filter(time_left_half > min_time_left)
  input_filtered <- input %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))
  return(input_filtered)
}

# Functions for computing features
#####################################

#
#
# HELPER FUNCTIONS FOR PROCESSING ORIENTATION AND DIRECTION
#
#
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

principal_angle <- function(v1, v2) {
  # Calculate the norms (magnitudes) of the vectors
  # Note: R's norm() function is for matrices,
  # so we use sqrt(sum(v^2)) or sqrt(v %*% v)
  norm_v1 <- sqrt(sum(v1^2))
  norm_v2 <- sqrt(sum(v2^2))
  
  # Handle the case of zero vectors to avoid division by zero
  if (norm_v1 == 0 || norm_v2 == 0) {
    # The angle is undefined if one vector is zero,
    # but we return 0.0 as a convention.
    return(0.0)
  }
  
  # Calculate the dot product
  # In R, the dot product is v1 %*% v2
  dot_product <- v1 %*% v2
  
  # Calculate the cosine of the angle
  cos_theta <- dot_product / (norm_v1 * norm_v2)
  
  # We clip the value to the range [-1.0, 1.0] to avoid
  # numerical errors (e.g., getting 1.0000001) that
  # would cause acos() to return NaN.
  # We use pmin and pmax for element-wise min/max
  cos_theta_clipped <- pmin(pmax(cos_theta, -1.0), 1.0)
  
  # Calculate the angle in radians using the arccosine
  # acos() returns the angle
  angle_rad <- acos(cos_theta_clipped)
  # The result of acos() might be a 1x1 matrix,
  # so we convert it to a simple numeric value
  return(rad2deg(as.numeric(angle_rad)))
}

process_angle <- function(x, ball_land_x, angle) {
  
  # ifelse() is the vectorized version of if-else.
  # It will check the condition (x > ball_land_x) for every element
  # and return the corresponding element from either (angle + 180) or (angle).
  
  result <- ifelse(x > ball_land_x, angle + 180, angle)
  
  return(result)
  
  # take the difference between angles and make sure its 180 degree calculated
}

angle_difference <- function(input_angle, angle_to_ball) {
  
  # 1. Calculate the absolute difference for all elements at once
  return_angle <- abs(input_angle - angle_to_ball)
  
  # 2. Use ifelse() to check the condition for every element
  # If return_angle > 180, use (360 - return_angle)
  # Otherwise, just use return_angle
  result <- ifelse(return_angle > 180, 360 - return_angle, return_angle)
  
  return(result)
}

#
#
# HERE ARE THE FUNCTIONS TO ACTUALLY PROCESS THE O AND DIR VARIABLES FOR THE INPUT DATASET. JUST PUT IN THE INPUT DATASET AND IT RETURNS
# THE FIXED DIR AND 0 VARIABLES REPECTIVELY.
#
#

process_dir <- function(input){
  x_abs <- abs(input$x - input$ball_land_x)
  y_abs <- abs(input$y - input$ball_land_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$ball_land_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$dir, theta))
}

process_o <- function(input){
  x_abs <- abs(input$x - input$ball_land_x)
  y_abs <- abs(input$y - input$ball_land_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$ball_land_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$o, theta))
}
#two new helper functions for processing angle difference between player and targeted receiver
process_dir_tr <- function(input){
  x_abs <- abs(input$x - input$target_x)
  y_abs <- abs(input$y - input$target_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$target_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$dir, theta))
}

process_o_tr <- function(input){
  x_abs <- abs(input$x - input$target_x)
  y_abs <- abs(input$y - input$target_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$target_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$o, theta))
}

# For merging input and output data
merge_data <- function(input, output) {
  input_filtered <- input %>%
    select('game_id', 'play_id', 'nfl_id', 'play_direction', 'absolute_yardline_number',
           'player_name', 'player_side', 'player_role', 'num_frames_output',
           'ball_land_x','ball_land_y') %>%
    distinct()
  output_mod <- output %>%
    left_join(input_filtered, by = c("game_id", "play_id", "nfl_id"))
  output_mod <-  speed_acceleration(data = output_mod)
  output_mod <- output_mod %>% select(-dx, -dy)
  input$output <- FALSE
  output_mod$output <- TRUE
  df <- input %>% bind_rows(output_mod)
  return(df)
}

# Functions for processing the data
#####################################
merge_and_process_data <- function(meta, input, output) {

  # Merge dataframes
  # ----------------------------------
  input_filtered <- input %>%
    select('game_id', 'play_id', 'nfl_id', 'play_direction', 'absolute_yardline_number',
           'player_name', 'player_side', 'player_role', 'num_frames_output',
           'ball_land_x','ball_land_y') %>%
    distinct()
  output_mod <- output %>%
    left_join(input_filtered, by = c("game_id", "play_id", "nfl_id"))
  output_mod <-  speed_acceleration(data = output_mod)
  output_mod <- output_mod %>% select(-dx, -dy)

  # TODO: Add estimated direction to output

  # Remove input data that does not show up in output data
  input_mod <- input %>%
    semi_join(output, by = c("game_id", "play_id", "nfl_id"))

  # Combine data into a total dataframe
  input_mod$output <- FALSE
  output_mod$output <- TRUE
  df <- input_mod %>% bind_rows(output_mod)

    # Add missing features to output rows
  input_filtered <- input %>%
    select('game_id', 'play_id', 'nfl_id', 'play_direction', 'absolute_yardline_number',
           'player_name', 'player_side', 'player_role', 'num_frames_output',
           'ball_land_x','ball_land_y') %>%
    distinct()
  output_mod <- output %>%
    left_join(input_filtered, by = c("game_id", "play_id", "nfl_id"))
  output_mod <-  speed_acceleration(data = output_mod)
  output_mod <- output_mod %>% select(-dx, -dy)

  # TODO: Add estimated direction to output

  # Remove input data that does not show up in output data
  input_mod <- input %>%
    semi_join(output, by = c("game_id", "play_id", "nfl_id"))

  # Combine data into a total dataframe
  input_mod$output <- FALSE
  output_mod$output <- TRUE
  df <- input_mod %>% bind_rows(output_mod)

  # Filter plays
  # -----------------------------------------

  # Remove low win probability plays
  df  <- filter_by_wp(meta, df)

  # Remove plays that occur near end of half
  df  <- filter_near_end_of_half(meta, df)

  # Add outcomes
  # -------------------------------------------

  # Get targeted reciever locations
  target_locs <- df %>%
    filter(player_role == "Targeted Receiver") %>%
    select(game_id, play_id, frame_id, target_x = x, target_y = y) %>%
    distinct()

  # Get targeted reciever final locations
  target_final_locs <- df %>%
    filter(player_role == "Targeted Receiver") %>%
    group_by(game_id, play_id) %>%
    slice_max(frame_id) %>%
    ungroup() %>%
    select(game_id, play_id, target_final_x = x, target_final_y = y)

  # Compute outcomes
  outcomes <- df %>%
    group_by(game_id, play_id, nfl_id) %>%
    slice_max(frame_id) %>%
    ungroup() %>%
    left_join(target_final_locs, by = c("game_id", "play_id")) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      distToTargetFinal = sqrt((x - target_final_x)^2 + (y - target_final_y)^2),
      inBallCircle = distFromBallLand <= 2,
      inBallTRCircle = distFromBallLand <= 2 | distToTargetFinal <= 2
    ) %>% select(game_id, play_id, nfl_id, inBallCircle, inBallTRCircle)

  # Add Response
  df <- df %>%
    left_join(outcomes, by = c("game_id", "play_id", "nfl_id"))

  # Filter by Player Positions
  # ----------------------------------------------

  # Only consider specific player positions
  positions <- c("CB", "FS")
  df <- df %>% filter(player_position %in% positions)

  # Add Features
  # ----------------------------------------------

  # Add distance to ball and targeted reciever
  df <- df %>%
    left_join(target_locs, by = c("game_id", "play_id", "frame_id")) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      distToTarget = sqrt((x - target_x)^2 + (y - target_y)^2),
      )

  # Add features based on number of frames; frames_until_throw, frames_until_ball_land
  df <- df %>%
    group_by(game_id, play_id) %>%
    mutate(
      max_input_frame  = max(frame_id[output == FALSE]),
      max_output_frame = max(frame_id[output == TRUE]),
      frames_until_throw = ifelse(
        output == FALSE,
        max_input_frame - frame_id,
        0
      ),
      frames_until_ball_land = max_output_frame - frame_id)  %>%
    ungroup() %>%
    select(-max_input_frame, -max_output_frame)

  # Add relative direction
  df$corrected_dir <- process_dir(df)
  df$corrected_dir_tr <- process_dir(df)

  return(df)
}


# For plotting gif of probabilites and play. Needs df with prob column and play information.
generate_gif_of_probs_and_play <- function(df, GAMEID, PLAYID){

  example.play <- df %>%
    filter(game_id == GAMEID, play_id == PLAYID) %>%
    arrange(frame_id)

  prob_players <- example.play %>%
    filter(!is.na(prob)) %>%
    distinct(nfl_id, player_name)

  player_colors <- setNames(
    hue_pal()(nrow(prob_players)),
    prob_players$nfl_id
  )

  example.play <- example.play %>%
    left_join(
      data.frame(
        nfl_id = as.numeric(names(player_colors)),
        prob_color = as.character(player_colors)
      ),
      by = "nfl_id"
    )

  player_labels <- example.play %>%
    filter(!is.na(prob), frame_id == max(example.play$frame_id)) %>%
    distinct(nfl_id, player_name, prob_color, prob) %>%
    mutate(label_y = prob + seq_along(nfl_id) * 0.01)



  example.play <- example.play %>%
    mutate(
      prob_color = case_when(
        !is.na(prob_color) ~ prob_color,                                      # already has prob color
        player_side == "Defense" ~ "#002244",                                 # blue
        player_side == "Offense" ~ "#c60c30",                                 # red
        TRUE ~ "gray50"                                                      # fallback
      )
    )

  throw_frame <- min(example.play$frame_id[example.play$output == TRUE])

  ball_land <- example.play %>%
    distinct(ball_land_x, ball_land_y)

  xmin <- 0
  xmax <- 160 / 3

  ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)

  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax),
                         y = seq(10,110,1)) %>%
    filter(!(floor(y %% 5) == 0)) %>%
    filter(y < ymax, y > ymin)

  play.plot <- ggplot() +
    annotate("text", x = df.hash$x[df.hash$x < xmax/2],
             y = df.hash$y[df.hash$x < xmax/2], label = "_", hjust = 0) +
    annotate("text", x = df.hash$x[df.hash$x > xmax/2],
             y = df.hash$y[df.hash$x > xmax/2], label = "_", hjust = 1) +
    geom_segment(aes(x = xmin,
                     y = seq(max(10, ymin), min(ymax, 110), by = 5),
                     xend = xmax,
                     yend = seq(max(10, ymin), min(ymax, 110), by = 5)),
                 linewidth = 0.3) +
    annotate("rect",
             xmin = xmin, xmax = xmax,
             ymin = ymin, ymax = ymax,
             fill = NA, colour = "black") +
    geom_text(data = ball_land,
              aes(x = xmax - ball_land_y, y = ball_land_x),
              label = "X",
              size = 8,
              colour = "black") +
    geom_point(data = example.play,
               aes(x = xmax - y, y = x,
                   color = prob_color,
                   group = nfl_id),
               size = 4, alpha = 0.8) +
    scale_color_identity() +
    coord_fixed() +
    ylim(ymin, ymax) +
    theme_nothing() +
    labs(title = "Frame {frame_time}")

  prob.plot <- ggplot() +
    geom_line(data = example.play %>% filter(!is.na(prob)),
              aes(x = frame_id, y = prob,
                  group = nfl_id,
                  color = prob_color),
              linewidth = 1) +
    geom_vline(xintercept = throw_frame, linetype = "dashed", linewidth = 1) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_identity(
      guide = "legend",
      labels = setNames(player_labels$player_name, player_labels$prob_color),
      breaks = player_labels$prob_color
    ) +
    coord_cartesian(xlim = c(min(example.play$frame_id),
                             max(example.play$frame_id))) +
    labs(title = paste('Game ID: ', GAMEID, 'Play ID: ', PLAYID),
         x = "Frame",
         y = "Probability") +
    theme_minimal(base_size = 12) +
    theme(
      # The legend is now needed, so we move it
      legend.position = c(0.2, 0.8), # Place legend inside plot area (top-left-ish)
      legend.background = element_rect(fill = "white", color = "gray50"),
      plot.margin = unit(c(1, 4, 1, 1), "lines") # Increase right margin for labels
    )


  anim_field <- play.plot + 
    transition_time(frame_id) +
    ease_aes('linear') +
    theme(plot.title = element_text(hjust = 0.5)) # Center title
  anim_prob <- prob.plot + 
    transition_reveal(frame_id) + 
    ease_aes('linear')

  fps_val <- 10
  nframes_val <- length(unique(example.play$frame_id))*2

  field_gif <- animate(anim_field, 
                       nframes = nframes_val, 
                       fps = fps_val, 
                       width = 600, 
                       height = 600,
                       renderer = magick_renderer())
  prob_gif <- animate(anim_prob, 
                      nframes = nframes_val, 
                      fps = fps_val, 
                      width = 400, 
                      height = 600,
                      renderer = magick_renderer())

  combined_gif <- image_append(c(prob_gif[1], field_gif[1]))

  for(i in 2:nframes_val){
    combined <- image_append(c(prob_gif[i], field_gif[i]))
    combined_gif <- c(combined_gif, combined)
  }

  # Pause the animation at end
  frames_to_pause <- 30
  last_frame <- combined_gif[length(combined_gif)]
  for(i in 1:frames_to_pause){
    combined_gif <- c(combined_gif, last_frame)
  }

  image_write(combined_gif, format = "gif", path = paste(GAMEID,"_",PLAYID, ".gif", sep = ''))
}
