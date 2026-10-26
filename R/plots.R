# Drawing one chart panel
#
# panel_plot() draws the plotly chart for one panel: one y-axis, a line per
# area or category, optional confidence bands, and vertical markers for series
# breaks.


# ---- Colours and fonts ----

# Line colours, always used in this order
SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

# Greys for text, gridlines and the England line
INK <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
         grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb")

FONT <- "system-ui, -apple-system, 'Segoe UI', sans-serif"

# A fixed colour for each region (alphabetical), so a region keeps its colour
# whatever else is selected. England is grey ink rather than a colour.
org_colours <- function(orgs) {
  regions <- orgs |> filter(org_code != "ENG") |> pull(org_name) |> sort()
  c(setNames(SERIES_COLOURS[seq_along(regions)], regions), England = unname(INK["secondary"]))
}


# ---- Gaps and series breaks ----

months_index <- function(d) year_of(d) * 12L + month_of(d)

# Plotly draws a line through every point it is given. To break a line (at a
# series break, or where periods are missing), insert a blank point in the gap.
#   s           the rows for one line
#   break_dates dates where the series changes and lines must not join
#   step        expected months between points (1, 3 or 12)
add_gaps <- function(s, break_dates, step) {
  s <- arrange(s, period_end)
  if (nrow(s) < 2) return(s)
  segment <- findInterval(s$period_end, break_dates)
  gap_before <- diff(months_index(s$period_end)) > pmax(step, s$period_months[-1]) | diff(segment) != 0
  if (!any(gap_before)) return(s)

  blanks <- s[which(gap_before) + 1L, ] |>
    mutate(value = NA_real_, period_end = period_end - 1)
  if ("ci_lower" %in% names(blanks)) blanks <- mutate(blanks, ci_lower = NA_real_, ci_upper = NA_real_)
  bind_rows(s, blanks) |> arrange(period_end)
}

# A break marker sits at the start of the month where the new series begins
break_line_date <- function(period_end) as.Date(format(period_end, "%Y-%m-01"))


# ---- The chart ----

#   pd      rows for this panel, with series / colour / dash / hover columns
#   unit    y-axis title ("Count", "%", "per 1,000 ...")
#   breaks  rows of series_breaks.csv that apply to this panel
#   view    "month", "quarter" or "fy"
#   x_range first and last date of the chosen date range
panel_plot <- function(pd, unit, breaks, view, x_range, show_ci = FALSE) {
  # Fit the axis to the data shown (some measures only start in 2026)
  shown <- range(pd$period_end)
  x_range <- c(max(x_range[1], shown[1] - 45), min(x_range[2], shown[2] + 45))

  # Series breaks within the visible range. Only "break" rows split the lines;
  # "frequency" (monthly -> quarterly) and "note" rows are markers only.
  breaks <- breaks |>
    filter(break_line_date(period_end) > x_range[1], break_line_date(period_end) <= x_range[2])
  split_dates <- breaks |> filter(type == "break") |> pull(period_end) |> break_line_date() |> sort()
  # One marker per date, with the labels of breaks on the same date stacked
  markers <- breaks |>
    group_by(period_end) |>
    summarise(
      type = if (any(type == "break")) "break" else if (any(type == "frequency")) "frequency" else "note",
      label = paste(label, collapse = "<br>"),
      description = paste(description, collapse = " "),
      .groups = "drop"
    )

  # One trace per line, plus its confidence band if asked for
  p <- plot_ly()
  for (line_name in unique(pd$series)) {
    line_rows <- add_gaps(filter(pd, series == line_name), split_dates, VIEW_MONTHS[[view]])
    colour <- line_rows$colour[1]

    if (show_ci && "ci_lower" %in% names(line_rows) && any(!is.na(line_rows$ci_lower))) {
      # One band per unbroken stretch: a filled band can't skip over a gap
      stretch <- cumsum(is.na(line_rows$value) | is.na(line_rows$ci_lower))
      for (k in unique(stretch)) {
        band <- line_rows[stretch == k & !is.na(line_rows$ci_lower) & !is.na(line_rows$ci_upper), ]
        if (!nrow(band)) next
        p <- add_ribbons(p, data = band, x = ~period_end, ymin = ~ci_lower, ymax = ~ci_upper,
                         fillcolor = paste0(colour, "26"), line = list(width = 0),   # "26" = 15% opacity
                         hoverinfo = "skip", showlegend = FALSE, legendgroup = line_name)
      }
    }

    p <- add_trace(p, data = line_rows, x = ~period_end, y = ~value, type = "scatter", mode = "lines+markers",
                   name = line_name, legendgroup = line_name, text = ~hover, hoverinfo = "text",
                   line = list(color = colour, width = 2, dash = line_rows$dash[1]),
                   marker = list(color = colour, size = 8, line = list(color = INK[["surface"]], width = 2)))
  }

  # Vertical marker lines (dashed for breaks, dotted for notes) and their labels
  marker_x <- break_line_date(markers$period_end)
  shapes <- lapply(seq_len(nrow(markers)), function(i) list(
    type = "line", xref = "x", yref = "paper", y0 = 0, y1 = 1, x0 = marker_x[i], x1 = marker_x[i],
    line = list(color = INK[["muted"]], width = 1, dash = if (markers$type[i] == "note") "dot" else "dash")))
  # Labels in the right third of the chart sit to the left of their line so they aren't clipped
  on_right <- function(x) as.numeric(x - x_range[1]) > 0.66 * as.numeric(diff(x_range))
  # Labels close together (within a sixth of the axis, e.g. Oct 2022 and Apr
  # 2023) go on separate rows so they don't overlap
  label_row <- integer(nrow(markers))
  for (i in seq_len(nrow(markers))[-1]) {
    earlier <- seq_len(i - 1)
    near <- abs(as.numeric(marker_x[i] - marker_x[earlier])) < as.numeric(diff(x_range)) / 6
    if (any(near)) label_row[i] <- max(label_row[earlier][near]) + 1L
  }
  labels <- lapply(seq_len(nrow(markers)), function(i) list(
    x = marker_x[i], y = 1 - 0.07 * label_row[i], xref = "x", yref = "paper",
    text = markers$label[i], hovertext = markers$description[i], showarrow = FALSE,
    xanchor = if (on_right(marker_x[i])) "right" else "left", xshift = if (on_right(marker_x[i])) -4 else 4,
    yanchor = "top", font = list(size = 11, color = INK[["secondary"]])))

  # x-axis: quarters and years get one labelled tick per period ("Jan–Mar 2026")
  xaxis <- list(title = "", type = "date", range = x_range + c(-20, 20), gridcolor = INK[["grid"]],
                linecolor = INK[["axis"]], zeroline = FALSE, showspikes = TRUE, spikemode = "across",
                spikecolor = INK[["axis"]], spikethickness = 1, spikedash = "solid")
  ticks <- sort(unique(pd$period_end))
  if (view != "month" && length(ticks) <= 16) {
    xaxis <- c(xaxis, list(tickmode = "array", tickvals = ticks,
                           ticktext = vapply(ticks, function(t) period_label(t, view), "")))
  }

  layout(p,
    shapes = shapes, annotations = labels,
    font = list(family = FONT, color = INK[["primary"]], size = 12),
    paper_bgcolor = INK[["surface"]], plot_bgcolor = INK[["surface"]],
    xaxis = xaxis,
    yaxis = list(title = list(text = unit, font = list(size = 12, color = INK[["secondary"]])),
                 gridcolor = INK[["grid"]], zeroline = FALSE, tickformat = ",~r",
                 ticksuffix = if (startsWith(unit, "%")) "%" else ""),
    showlegend = length(unique(pd$series)) > 1,   # a single line is named by the panel title
    legend = list(orientation = "h", y = -0.15, font = list(size = 12)),
    hoverlabel = list(bgcolor = INK[["surface"]], bordercolor = INK[["axis"]],
                      font = list(family = FONT, size = 12, color = INK[["primary"]])),
    margin = list(t = 24, r = 12, b = 8, l = 8)
  ) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}
