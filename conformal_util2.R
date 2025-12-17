# ================================================================
# 1) Prepare throw-aligned frames (PRE / POST / BOTH)
# ================================================================
# mode = "pre"  : output == FALSE, t_rel <= 0
# mode = "post" : output == TRUE,  t_rel >= 0
# mode = "both" : combine both with phase ∈ {pre, post}
#
# Convention:
#   t_rel = 0 is the throw
#   pre  -> ..., -2, -1, 0
#   post -> 0, 1, 2, ...
# ================================================================
prepare_throw_frames <- function(
    data,
    mode            = c("pre", "post", "both"),
    output_col      = "output",
    frame_col       = "frame_id",
    group_cols      = c("game_id", "play_id", "nfl_id"),
    outcome_col     = "inBallTRCircle",
    out_t_rel_col   = "t_rel",
    out_binary_col  = "inBallTRCircle",
    out_phase_col   = "phase"
) {
  mode <- match.arg(mode)

  stopifnot(
    output_col  %in% names(data),
    frame_col   %in% names(data),
    outcome_col %in% names(data)
  )

  make_pre <- function(d) {
    d <- d[d[[output_col]] == "FALSE", , drop = FALSE]
    if (nrow(d) == 0) return(NULL)

    d <- d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::mutate(
        throw_frame = max(.data[[frame_col]], na.rm = TRUE),
        !!out_t_rel_col := .data[[frame_col]] - throw_frame
      ) %>%
      dplyr::ungroup()

    d[[out_phase_col]] <- "pre"
    d
  }

  make_post <- function(d) {
    d <- d[d[[output_col]] == "TRUE", , drop = FALSE]
    if (nrow(d) == 0) return(NULL)

    # Align POST frames to the THROW frame:
    # t_rel = 0 at throw, then 1,2,... afterwards.
    # We estimate throw_frame as the last PRE frame for that (game, play, player).
    throw_tbl <- data %>%
      dplyr::filter(.data[[output_col]] == "FALSE") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        throw_frame = max(.data[[frame_col]], na.rm = TRUE),
        .groups = "drop"
      )

    d <- d %>%
      dplyr::left_join(throw_tbl, by = group_cols)

    if (any(is.na(d$throw_frame))) {
      stop("Some post-throw rows have no matching pre-throw throw_frame. Check group_cols / data completeness.")
    }

    d <- d %>%
      dplyr::mutate(
        !!out_t_rel_col := .data[[frame_col]] - .data$throw_frame
      )

    d[[out_phase_col]] <- "post"
    d
  }

  out <- switch(
    mode,
    pre  = make_pre(data),
    post = make_post(data),
    both = dplyr::bind_rows(make_pre(data), make_post(data))
  )

  if (is.null(out) || nrow(out) == 0) {
    stop("No rows produced for mode = ", mode)
  }

  # Coerce outcome to numeric 0/1
  out[[out_binary_col]] <- as.numeric(out[[outcome_col]])

  out
}

# ================================================================
# 2) Helper: split by PLAY
# ================================================================
nfl_split_calib_test_by_play <- function(
    data,
    play_cols   = c("game_id", "play_id"),
    calib_frac  = 0.5,
    seed        = 123
) {
  stopifnot(all(play_cols %in% names(data)))
  set.seed(seed)

  plays_tbl <- data %>% dplyr::distinct(dplyr::across(dplyr::all_of(play_cols)))
  n_plays   <- nrow(plays_tbl)
  if (n_plays < 2) return(list(calib = data[0, , drop = FALSE], test = data))

  n_calib <- max(1, floor(calib_frac * n_plays))
  calib_idx <- sample(seq_len(n_plays), size = n_calib, replace = FALSE)

  plays_tbl$.__is_calib <- FALSE
  plays_tbl$.__is_calib[calib_idx] <- TRUE

  tagged <- data %>% dplyr::left_join(plays_tbl, by = play_cols)

  list(
    calib = tagged %>% dplyr::filter(.__is_calib) %>% dplyr::select(-.__is_calib),
    test  = tagged %>% dplyr::filter(!.__is_calib) %>% dplyr::select(-.__is_calib)
  )
}

# ================================================================
# 3) Player-wise conformal bands by t_rel
#     (works for pre / post / both)
# ================================================================
conformal_by_trel_playerwise <- function(
    data,
    prob_col      = "prob",
    y_col         = "inBallTRCircle",
    t_rel_col     = "t_rel",
    player_col    = "nfl_id",
    play_cols     = c("game_id", "play_id"),
    alpha         = 0.1,
    calib_frac    = 0.5,
    min_plays     = 3,
    min_per_trel  = 20,
    seed          = 123
) {

  data[[y_col]] <- as.numeric(data[[y_col]])

  cp_quantile <- function(scores, alpha) {
    scores <- sort(scores[is.finite(scores)], na.last = NA)
    n <- length(scores)
    if (n == 0) return(NA_real_)
    k <- ceiling((n + 1) * (1 - alpha))
    k <- max(1, min(k, n))
    scores[k]
  }

  set.seed(seed)

  split_by_player <- split(data, data[[player_col]])

  per_player <- lapply(split_by_player, function(dj) {

    dj <- dj[is.finite(dj[[t_rel_col]]) & is.finite(dj[[prob_col]]), , drop = FALSE]
    if (nrow(dj) == 0) return(NULL)

    plays_tbl <- dj %>% dplyr::distinct(dplyr::across(dplyr::all_of(play_cols)))
    if (nrow(plays_tbl) < min_plays) return(NULL)

    sp <- nfl_split_calib_test_by_play(dj, play_cols, calib_frac, seed)
    calib <- sp$calib
    test  <- sp$test
    if (nrow(calib) == 0 || nrow(test) == 0) return(NULL)

    calib$score <- abs(calib[[y_col]] - calib[[prob_col]])
    q_global <- cp_quantile(calib$score, alpha)

    q_tbl <- calib %>%
      dplyr::group_by(.data[[t_rel_col]]) %>%
      dplyr::summarise(
        n_calib = dplyr::n(),
        q_trel  = ifelse(n_calib >= min_per_trel,
                         cp_quantile(score, alpha),
                         NA_real_),
        .groups = "drop"
      )

    test %>%
      dplyr::left_join(q_tbl, by = t_rel_col) %>%
      dplyr::mutate(
        q_eff = ifelse(is.na(q_trel), q_global, q_trel),
        L = pmax(0, .data[[prob_col]] - q_eff),
        U = pmin(1, .data[[prob_col]] + q_eff)
      )
  })

  data_all <- dplyr::bind_rows(per_player)
  if (nrow(data_all) == 0) stop("No players qualified for conformal bands.")

  list(
    data       = data_all,
    alpha      = alpha,
    player_col = player_col,
    t_rel_col  = t_rel_col
  )
}

# ================================================================
# 4) Summarise player curves over t_rel AND (optionally) produce
#    a ranking of players by their lower conformal curve over a window.
#
# MODIFICATION:
#   - player_lookup: either (a) a data.frame with nfl_id + player_name, OR
#                    (b) a path to a CSV with those columns.
#   - If provided, ranks can display names (rank_return_names = TRUE),
#     while ALWAYS keeping nfl_id in rank_long for safety.
# ================================================================
summarise_player_cp <- function(
    cp_obj,
    t_rel_col         = "t_rel",
    prob_col          = "prob",
    lower_col         = "L",
    upper_col         = "U",
    player_id_col     = "nfl_id",
    player_name_col   = "player_name",  # will be created via lookup if missing

    # ---- NEW: optional lookup (df or csv path) ----
    player_lookup     = NULL,           # df or "path/to/player_name.csv"
    lookup_id_col     = "nfl_id",
    lookup_name_col   = "player_name",

    # ---- ranking options ----
    return_rank       = FALSE,
    rank_t_rel_range  = c(-10, 0),
    rank_top_k        = 10,
    rank_min_points   = 8,
    rank_score_fun    = c("mean", "min", "p10"),
    rank_return_names = FALSE           # if TRUE and names available, Rank_* uses names
) {
  d <- cp_obj$data

  # ---- load / validate lookup (if provided) ----
  if (!is.null(player_lookup)) {
    if (is.character(player_lookup) && length(player_lookup) == 1) {
      # path to csv
      player_lookup <- read.csv(player_lookup, stringsAsFactors = FALSE)
    }
    stopifnot(is.data.frame(player_lookup))
    stopifnot(all(c(lookup_id_col, lookup_name_col) %in% names(player_lookup)))

    # attach names into d (do not overwrite nfl_id)
    if (!(player_name_col %in% names(d))) {
      # rename lookup columns to match expected in d
      lk <- player_lookup[, c(lookup_id_col, lookup_name_col)]
      names(lk) <- c(player_id_col, player_name_col)

      d <- d %>%
        dplyr::left_join(lk, by = player_id_col)
    }
  }

  # ---- curves (same as before) ----
  has_name <- !is.null(player_name_col) && player_name_col %in% names(d)
  group_vars <- if (has_name) c(player_id_col, player_name_col, t_rel_col) else c(player_id_col, t_rel_col)

  curves <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      mean_prob = mean(.data[[prob_col]],  na.rm = TRUE),
      mean_L    = mean(.data[[lower_col]], na.rm = TRUE),
      mean_U    = mean(.data[[upper_col]], na.rm = TRUE),
      n_points  = dplyr::n(),
      .groups   = "drop"
    )

  if (!return_rank) return(curves)

  # ---- ranking block ----
  rank_score_fun <- match.arg(rank_score_fun)

  stopifnot(length(rank_t_rel_range) == 2)
  dfw <- curves[curves[[t_rel_col]] >= rank_t_rel_range[1] & curves[[t_rel_col]] <= rank_t_rel_range[2], , drop = FALSE]
  if (nrow(dfw) == 0) stop("No rows left after rank_t_rel_range filtering (check window).")

  keep_ids <- dfw %>%
    dplyr::group_by(.data[[player_id_col]]) %>%
    dplyr::summarise(n_window = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n_window >= rank_min_points) %>%
    dplyr::pull(.data[[player_id_col]])
  dfw <- dfw[dfw[[player_id_col]] %in% keep_ids, , drop = FALSE]
  if (nrow(dfw) == 0) stop("No players left after rank_min_points filtering (lower rank_min_points).")

  rank_long <- dfw %>%
    dplyr::group_by(.data[[player_id_col]]) %>%
    dplyr::summarise(
      n_points_window = dplyr::n(),
      score = dplyr::case_when(
        rank_score_fun == "mean" ~ mean(mean_L, na.rm = TRUE),
        rank_score_fun == "min"  ~ min(mean_L,  na.rm = TRUE),
        rank_score_fun == "p10"  ~ as.numeric(stats::quantile(mean_L, probs = 0.10, na.rm = TRUE, type = 8)),
        TRUE ~ mean(mean_L, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(score))

  # attach name if available
  if (has_name) {
    nm <- curves %>% dplyr::distinct(.data[[player_id_col]], .data[[player_name_col]])
    rank_long <- rank_long %>% dplyr::left_join(nm, by = player_id_col)
  }

  top_tbl <- rank_long %>% dplyr::slice_head(n = rank_top_k)

  # What to display in Rank_* columns
  # (recommended: display name, but keep nfl_id in rank_long always)
  if (rank_return_names && has_name) {
    rank_vals <- as.character(top_tbl[[player_name_col]])
  } else {
    rank_vals <- as.character(top_tbl[[player_id_col]])
  }

  rank_wide <- as.data.frame(t(rank_vals), stringsAsFactors = FALSE)
  colnames(rank_wide) <- paste0("Rank_", seq_len(ncol(rank_wide)))

  if (ncol(rank_wide) < rank_top_k) {
    for (j in (ncol(rank_wide) + 1):rank_top_k) rank_wide[[paste0("Rank_", j)]] <- NA_character_
  }

  list(curves = curves, rank_wide = rank_wide, rank_long = rank_long)
}


# ================================================================
# 5) Plot lower conformal curves for many players.
#
# MODIFICATION:
#   - player_lookup: df or csv path (nfl_id -> player_name)
#   - Uses player_name for labels if available + requested
#   - Keeps grouping/highlighting by nfl_id ALWAYS
# ================================================================
plot_players_lower_curves_labeled <- function(
    player_cp_curves,
    n_labelled       = 10,
    sample_n_players = NULL,
    player_ids       = NULL,
    min_points       = 0,
    t_rel_range      = NULL,
    n_before         = NULL,
    player_id_col    = "nfl_id",
    player_name_col  = "player_name",  # will be created via lookup if missing

    # ---- NEW: optional lookup (df or csv path) ----
    player_lookup     = NULL,          # df or "path/to/player_name.csv"
    lookup_id_col     = "nfl_id",
    lookup_name_col   = "player_name",
    label_use_names   = TRUE,          # if TRUE and names available, labels use names

    lower_col_guess  = c("mean_L", "L"),
    title            = "Season worst-case (conformal lower curves)",

    # ---- highlight Top-K ----
    highlight_top_ids = NULL,
    highlight_lwd     = 1.4,
    base_lwd          = 0.6,
    highlight_alpha   = 0.95,
    base_alpha        = 0.25,
    label_highlight   = TRUE
) {
  lower_col_guess <- lower_col_guess[lower_col_guess %in% names(player_cp_curves)]
  if (length(lower_col_guess) == 0) stop("No lower-band column found (expected mean_L or L).")
  lower_col <- lower_col_guess[1]

  df <- player_cp_curves

  # ---- load / apply lookup (if provided) ----
  if (!is.null(player_lookup)) {
    if (is.character(player_lookup) && length(player_lookup) == 1) {
      player_lookup <- read.csv(player_lookup, stringsAsFactors = FALSE)
    }
    stopifnot(is.data.frame(player_lookup))
    stopifnot(all(c(lookup_id_col, lookup_name_col) %in% names(player_lookup)))

    if (!(player_name_col %in% names(df))) {
      lk <- player_lookup[, c(lookup_id_col, lookup_name_col)]
      names(lk) <- c(player_id_col, player_name_col)
      df <- df %>% dplyr::left_join(lk, by = player_id_col)
    }
  }

  # Windowing
  if (!is.null(n_before)) t_rel_range <- c(-abs(n_before), 0)
  if (!is.null(t_rel_range)) {
    stopifnot(length(t_rel_range) == 2)
    df <- df[df$t_rel >= t_rel_range[1] & df$t_rel <= t_rel_range[2], , drop = FALSE]
  }
  if (nrow(df) == 0) stop("No rows left after t_rel filtering.")

  # factor for discrete scales (grouping ALWAYS by id)
  df$player_id_factor <- factor(df[[player_id_col]])

  # ---- choose label text ----
  has_name <- (player_name_col %in% names(df)) && any(!is.na(df[[player_name_col]]))
  if (label_use_names && has_name) {
    df$player_label_plot <- as.character(df[[player_name_col]])
  } else {
    df$player_label_plot <- as.character(df[[player_id_col]])
  }

  # Explicit subset overrides sampling
  if (!is.null(player_ids)) {
    df <- df[df[[player_id_col]] %in% player_ids, , drop = FALSE]
  }

  # Require at least min_points timepoints
  if (min_points > 0) {
    keep_ids <- df %>%
      dplyr::group_by(.data[[player_id_col]]) %>%
      dplyr::summarise(n_t = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n_t >= min_points) %>%
      dplyr::pull(.data[[player_id_col]])
    df <- df[df[[player_id_col]] %in% keep_ids, , drop = FALSE]
  }
  if (nrow(df) == 0) stop("No rows left after min_points filtering.")

  # Random sample players (only if player_ids not specified)
  if (is.null(player_ids) && !is.null(sample_n_players)) {
    all_ids <- unique(df[[player_id_col]])
    df <- df[df[[player_id_col]] %in% sample(all_ids, size = min(sample_n_players, length(all_ids))), , drop = FALSE]
  }
  if (nrow(df) == 0) stop("No rows left after sampling/filtering.")

  # Highlight flag
  if (is.null(highlight_top_ids)) highlight_top_ids <- c()
  df$is_highlight <- df[[player_id_col]] %in% highlight_top_ids

  # Who to label (original behavior)
  ids_final <- unique(df[[player_id_col]])
  if (!is.null(player_ids)) {
    label_ids <- intersect(player_ids, ids_final)
  } else if (!is.null(sample_n_players)) {
    label_ids <- ids_final
  } else {
    label_ids <- sample(ids_final, size = min(n_labelled, length(ids_final)), replace = FALSE)
  }

  # endpoint rows
  df_label <- df %>%
    dplyr::filter(.data[[player_id_col]] %in% label_ids) %>%
    dplyr::group_by(.data[[player_id_col]]) %>%
    dplyr::slice_max(t_rel, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  df_label_hi <- df %>%
    dplyr::filter(.data[[player_id_col]] %in% highlight_top_ids) %>%
    dplyr::group_by(.data[[player_id_col]]) %>%
    dplyr::slice_max(t_rel, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Base plot: non-highlight faint + highlight bold
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df[df$is_highlight == FALSE, , drop = FALSE],
      ggplot2::aes(x = t_rel, y = .data[[lower_col]], group = player_id_factor),
      alpha = base_alpha, linewidth = base_lwd, color = "grey40"
    ) +
    ggplot2::geom_line(
      data = df[df$is_highlight == TRUE, , drop = FALSE],
      ggplot2::aes(x = t_rel, y = .data[[lower_col]], group = player_id_factor, color = player_id_factor),
      alpha = highlight_alpha, linewidth = highlight_lwd
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    ggplot2::labs(
      x = NULL, #"Frames relative to throw",
      y = "on success probability", #PLB on success probability",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title.y = ggplot2::element_text(hjust = 0.8)
    )

  # Labels for highlighted players
  if (label_highlight && nrow(df_label_hi) > 0 && requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_text_repel(
        data = df_label_hi,
        ggplot2::aes(
          x = t_rel,
          y = .data[[lower_col]],
          label = player_label_plot,
          color = player_id_factor
        ),
        size = 3.2,
        max.overlaps = Inf,
        min.segment.length = 0,
        box.padding = 0.25,
        point.padding = 0.15,
        show.legend = FALSE
      )
  }

  # Optional: label random subset too
  if (nrow(df_label) > 0 && requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_text_repel(
        data = df_label,
        ggplot2::aes(
          x = t_rel,
          y = .data[[lower_col]],
          label = player_label_plot
        ),
        size = 2.6,
        color = "grey30",
        max.overlaps = Inf,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.10,
        show.legend = FALSE
      )
  }

  p
}

