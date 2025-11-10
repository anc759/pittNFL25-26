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

# =============================================================================
# Voronoi overlay utilities (NFL field) — requires: sf
# =============================================================================

#' @title Build Voronoi polygons for a single frame (clipped to field)
#' @description
#' Computes Voronoi cells for all players in a given frame, then clips the
#' tessellation to the NFL field rectangle (default [0,120] x [0,53.3]).
#' Returns an `sf` polygon data frame with player attributes attached.
#'
#' @param df_tracks Data frame with at least: frame_id, nfl_id, x, y.
#'   Helpful extras: player_name, player_side ("Offense"/"Defense"), player_role.
#' @param frame Integer frame to compute the tessellation for.
#' @param field_xlim numeric length-2, default c(0,120)
#' @param field_ylim numeric length-2, default c(0,53.3)
#' @param team_side "both"|"offense"|"defense" (filter which points to include)
#' @return sf POLYGON data frame with columns:
#'   nfl_id, player_label, player_side, player_role, frame_id, geometry
#' @examples
#' # polys <- nfl_build_voronoi(tracks, frame = 18)
nfl_build_voronoi <- function(
    df_tracks,
    frame,
    field_xlim = c(0, 120),
    field_ylim = c(0, 53.3),
    team_side  = c("both","offense","defense")
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for Voronoi polygons. Install with install.packages('sf').")
  }
  team_side <- match.arg(team_side)

  # Filter to frame & (optionally) side
  pts <- df_tracks[df_tracks$frame_id == frame, , drop = FALSE]
  if (nrow(pts) == 0) stop(sprintf("No rows found in frame %s.", frame))
  if ("player_side" %in% names(pts)) {
    if (team_side == "offense") pts <- pts[pts$player_side == "Offense", , drop = FALSE]
    if (team_side == "defense") pts <- pts[pts$player_side == "Defense", , drop = FALSE]
  }
  if (nrow(pts) == 0) stop(sprintf("No player points after team_side='%s' at frame %s.", team_side, frame))

  pts$player_label <- ifelse(!is.na(pts$player_name) & nzchar(pts$player_name),
                             pts$player_name, paste0("ID ", pts$nfl_id))

  # sf points (planar yards; no CRS)
  sf_pts <- sf::st_as_sf(pts, coords = c("x","y"), remove = FALSE, crs = sf::NA_crs_)

  # Field rectangle polygon (for bounding & clipping)
  rect_mat <- matrix(
    c(field_xlim[1], field_ylim[1],
      field_xlim[1], field_ylim[2],
      field_xlim[2], field_ylim[2],
      field_xlim[2], field_ylim[1],
      field_xlim[1], field_ylim[1]), ncol = 2, byrow = TRUE
  )
  field_poly <- sf::st_sfc(sf::st_polygon(list(rect_mat)), crs = sf::NA_crs_)

  # Voronoi tessellation inside field envelope
  vor_raw   <- sf::st_voronoi(sf::st_union(sf_pts), envelope = field_poly)
  vor_polys <- sf::st_collection_extract(vor_raw, "POLYGON")

  # Turn the GEOMETRYCOLLECTION into an sf polygon layer
  vor_sf <- sf::st_sf(geometry = vor_polys)
  # Clip to field (ensures bounded cells)
  vor_sf <- suppressWarnings(sf::st_intersection(vor_sf, field_poly))

  # Map polygons back to generating points
  idx <- sf::st_nearest_feature(vor_sf, sf_pts)
  vor_sf <- sf::st_set_geometry(
    cbind(
      sf::st_drop_geometry(sf_pts[idx, c("nfl_id","player_label","player_side","player_role","frame_id")]),
      vor_sf
    ),
    sf::st_geometry(vor_sf)
  )

  dplyr::relocate(vor_sf, nfl_id, player_label, player_side, player_role, frame_id)
}

# ---- helper: convert sf polygon layer to plain data.frame for geom_polygon ----
nfl_sf_to_polygon_df <- function(vor_sf) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with install.packages('sf').")
  }
  vor_sf <- sf::st_cast(vor_sf, "POLYGON", warn = FALSE)
  vor_sf$poly_id <- seq_len(nrow(vor_sf))

  cc <- sf::st_coordinates(vor_sf)
  df <- as.data.frame(cc)
  names(df)[1:2] <- c("x","y")
  df$poly_id <- vor_sf$poly_id[df$L1]
  df$ring_id <- df$L2
  df$group   <- interaction(df$poly_id, df$ring_id, drop = TRUE)

  attrs <- sf::st_drop_geometry(vor_sf)
  df <- merge(
    df,
    attrs[, c("poly_id","nfl_id","player_label","player_side","player_role","frame_id")],
    by = "poly_id",
    all.x = TRUE,
    sort = FALSE
  )

  df <- df[order(df$poly_id, df$ring_id, ave(df$x, df$group, FUN = seq_along)), ]
  df
}

#' @title Voronoi overlay ggplot layer (no coord override)
#' @description
#' Returns a `ggplot2` layer using `geom_polygon` so it doesn't replace your
#' existing coordinate system (e.g., `coord_fixed`). Choose fill by team, player,
#' or constant.
#'
#' @param vor_sf The sf polygons from `nfl_build_voronoi()`.
#' @param fill_by "team"|"player"|"none"
#' @param alpha Polygon fill alpha (default 0.25).
#' @param border_color Outline color (default "white").
#' @param border_size Outline line width (default 0.3).
#' @return A `ggplot2` layer you can add with `+`.
#' @examples
#' # p + nfl_geom_voronoi_layer(vor_sf, fill_by="team")
nfl_geom_voronoi_layer <- function(
    vor_sf,
    fill_by        = c("team","player","none"),
    alpha          = 0.25,
    border_color   = "white",
    border_size    = 0.3,
    # palettes (edit to taste)
    team_colors    = c(Offense = "#004C97", Defense = "#FF6F61", Unknown = "#9E9E9E"),
    player_palette = NULL  # NULL => auto palette; or pass a vector/function/palette name
) {
  fill_by <- match.arg(fill_by)

  # Convert sf -> plain df for geom_polygon (keeps your coord system intact)
  vdf <- nfl_sf_to_polygon_df(vor_sf)

  # Helper: attach a vor_fill column (hex) and return layers using fill identity
  make_identity_layers <- function(df, fill_values, fill_key, legend_title) {
    # Map each category to a hex fill, attach as a column
    df$vor_key <- as.character(df[[fill_key]])
    df$vor_key[is.na(df$vor_key)] <- "Unknown"

    # Ensure we have a color for every key in the data
    keys_present <- unique(df$vor_key)
    pal <- fill_values
    # If any missing keys, assign a fallback grey
    if (!all(keys_present %in% names(pal))) {
      missing_keys <- setdiff(keys_present, names(pal))
      pal <- c(pal, setNames(rep("#9E9E9E", length(missing_keys)), missing_keys))
    }
    df$vor_fill <- pal[df$vor_key]

    # Build legend mapping (identity scale)
    layers <- list(
      ggplot2::geom_polygon(
        data = df,
        ggplot2::aes(x = x, y = y, group = group, fill = vor_fill),
        color = border_color, linewidth = border_size,
        alpha = alpha, inherit.aes = FALSE, show.legend = TRUE
      ),
      ggplot2::scale_fill_identity(
        name   = legend_title,
        guide  = "legend",
        labels = setNames(names(pal[keys_present]), pal[keys_present]),
        breaks = pal[keys_present]
      )
    )
    layers
  }

  if (fill_by == "team") {
    # Normalize side labels
    vdf$player_side <- as.character(vdf$player_side)
    vdf$player_side[is.na(vdf$player_side)] <- "Unknown"
    vdf$player_side <- ifelse(vdf$player_side %in% c("Offense","Defense","Unknown"),
                              vdf$player_side, "Unknown")
    return(make_identity_layers(
      df          = vdf,
      fill_values = team_colors,
      fill_key    = "player_side",
      legend_title= "Side"
    ))
  }

  if (fill_by == "player") {
    # Build player palette
    players <- sort(unique(vdf$player_label))
    if (is.null(player_palette)) {
      # Prefer nice qualitative palette if available
      if (requireNamespace("scales", quietly = TRUE)) {
        cols <- scales::hue_pal()(length(players))
      } else {
        cols <- grDevices::hcl.colors(length(players), "Set3")
      }
    } else if (is.function(player_palette)) {
      cols <- player_palette(length(players))
    } else if (is.character(player_palette) && length(player_palette) == 1) {
      # interpret as a named palette (e.g., "Set3")
      cols <- tryCatch(grDevices::hcl.colors(length(players), palette = player_palette),
                       error = function(...) grDevices::rainbow(length(players)))
    } else if (is.character(player_palette) && length(player_palette) >= length(players)) {
      cols <- player_palette[seq_along(players)]
    } else {
      cols <- grDevices::rainbow(length(players))
    }
    pal_players <- setNames(cols, players)

    return(make_identity_layers(
      df          = vdf,
      fill_values = pal_players,
      fill_key    = "player_label",
      legend_title= "Player (Voronoi)"
    ))
  }

  # No fill mapping — constant translucent grey with no legend
  list(
    ggplot2::geom_polygon(
      data  = vdf,
      ggplot2::aes(x = x, y = y, group = group),
      fill  = scales::alpha("#9E9E9E", alpha),
      color = border_color,
      linewidth = border_size,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}
