##' Hour rose plot.
##' `phour()` plotted a 24-hour clock, indicating the active hours on the basis of a time column in a data frame. It is highly cumstomized on the basis of the `openair::windrose()` function`.
##'
##' @param my_df A data frame containing the fields ws and wd.
##' @param ws A character string of the name of the column representing the radium of the circular sectors in the diagram. It can be SBD payout, votes, comments, etc..
##' @param wd A character sring of the name of the column representing the hour of the day.
##' @param ws2 The user can supply a second set of data with which the first can be compared.
##' @param wd2 see ws2.
##' @param ws.int A numeric vector of the ws interval. Default is 2.
##' @param angle The hour spokes. Other potentially useful angle is 3 (hours).
##' @param type type determines how the data are split i.e. conditioned, and then plotted.  It can be 'season', 'year', 'weekday'...  The default is will produce a single plot using the entire data. It can also be a numeric or factor vector.
##' @param cols Colours for plotting.  'default', 'increment', 'heat', 'jet', 'hue' and user defined, such as c("yellow", "green", "blue", "black").
##' @param grid.line Grid line interval. NULL in default.It can also be a numeric value like 10, or a list like list(value = 10, lty = 5, col = "purple").
##' @param width The adjustment factor for width of payout intervals. For example, width = 1.5 will make the paddle width 1.5 times wider. For paddle = TRUE.
##' @param seg  The width of the segments. 0.5 will produce segments 0.5 * angle.
##' @param auto.text  A logical value of whether formatting the names and units automatically in the titles and axis labels
##' @param breaks A numeric vector of the number of break points for payouts. 4 by default, which generates the break points 2, 4, 6, 8 SBD for ws.int default of 2 SBD. It can also be c(0, 1, 10, 100), which breaks the data into segments <1, 1-10, 10-100, >100.
##' @param offset A numeric value (default 10) of the size of the 'hole' in the middle of the plot, expressed as a percentage of the polar axis scale.
##' @param paddle A logic value. TRUE means the 'paddle' style spokes, and FALSE means the 'wedge' style spokes.
##' @param key.header A character string of additional text above the scale key.
##' @param key.footer A character string of additional text below the scale key.
##' @param key.position A character string of the location of the scale key. 'top', 'right', 'bottom' and 'left'.
##' @param key Fine control of the scale key
##' @param dig.lab A numeric value of the signficant digits at which scientific number formatting is used in break point.
##' @param statistic A character string of the statistic to be applied.
##' - 'prop.count' (default) sizes bins according to the proportion of the frequency of the records,
##' - 'prop.mean' sizes bins according to their relative contribution to the mean,
##' - 'abs.count' provides the absolute count of records in each bin.
##' @param pollutant Alternative data series to be sampled.
##' @param annotate A logic value or a character string.
##' - TRUE: the percentage calm and mean values are printed in each panel together with a description of the statistic below the plot.
##' - " ": only the stastic is below the plot.
##' - Custom annotations may be added by setting value to c("annotation 1", "annotation 2").
##' @param border A character string of the border colour for shaded areas.
##' @param cust_labels A numeric vector displayed as the customed labels
##' @param ...
##'
#' @return A figure with the active hour rose
#' @export
#' @examples
#' phour(my_df, breaks = seq(0, 24, 1), angle = 15, key.footer = 'votes')
phour <- function(my_df, col_time = 'created',
                  ws = "ws", wd = "hour360",
                  ws2 = NA, wd2 = NA,
                  ws.int = 30, angle = 1, type = "default", cols = "default",
                  grid.line = NULL, width = 1, seg = 0.9, auto.text = TRUE,
                  breaks = 4, offset = 10, paddle = FALSE,
                  key.header = NULL, key.footer = "(SBD)",
                  key.position = "right", key = FALSE,
                  dig.lab = 5, statistic = "prop.count", pollutant = NULL,
                  cust_labels = c(0, 6, 12, 18), annotate = FALSE,
                  border = NA, quantile_line = TRUE,
                  ...)
{
  my_df <- as.data.frame(my_df)
  angle <- 15 * angle
  my_df$hour <- round(as.numeric(format(my_df[, col_time], '%H')) + as.numeric(format(my_df[, col_time], '%M'))/60, 1)
  my_df$hour360 <- my_df$hour * 360 / 24
  my_df$ws <- 1

  if (is.null(seg))
    seg <- 0.9
  if (length(cols) == 1 && cols == "greyscale") {
    lattice::trellis.par.set(list(strip.background = list(col = "white")))
    calm.col <- "black"
  }
  else {
    calm.col <- "forestgreen"
  }
  current.strip <- lattice::trellis.par.get("strip.background")
  on.exit(lattice::trellis.par.set("strip.background", current.strip))
  if (360/angle != round(360/angle)) {
    warning("In windRose(...):\n  angle will produce some spoke overlap",
            "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.",
            call. = FALSE)
  }
  if (angle < 3) {
    warning("In windRose(...):\n  angle too small", "\n  enforcing 'angle = 3'",
            call. = FALSE)
    angle <- 3
  }
  extra.args <- list(...)
  extra.args$xlab <- if ("xlab" %in% names(extra.args))
    openair::quickText(extra.args$xlab, auto.text)
  else openair::quickText("", auto.text)
  extra.args$ylab <- if ("ylab" %in% names(extra.args))
    openair::quickText(extra.args$ylab, auto.text)
  else openair::quickText("", auto.text)
  extra.args$main <- if ("main" %in% names(extra.args))
    openair::quickText(extra.args$main, auto.text)
  else openair::quickText("", auto.text)
  if (is.character(statistic)) {
    ok.stat <- c("prop.count", "prop.mean", "abs.count",
                 "frequency")
    if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
      warning("In windRose(...):\n  statistic unrecognised",
              "\n  enforcing statistic = 'prop.count'", call. = FALSE)
      statistic <- "prop.count"
    }
    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts (%)"
      stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                      3)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                      3)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "abs.count" | statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }
  if (is.list(statistic)) {
    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }
  vars <- c(wd, ws)
  diff <- FALSE
  rm.neg <- TRUE
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    my_df$ws <- my_df[, ws2] - my_df[, ws]
    my_df$wd <- my_df[, wd2] - my_df[, wd]
    id <- which(my_df$wd < 0)
    if (length(id) > 0)
      my_df$wd[id] <- my_df$wd[id] + 360
    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle))
      angle <- 10
    if (missing(offset))
      offset <- 20
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(min(my_df$ws, na.rm = TRUE),
                                  max(my_df$ws, na.rm = TRUE)))))
      breaks <- c(-1 * max.br, 0, max.br)
    }
    if (missing(cols))
      cols <- c("lightskyblue", "tomato")
    seg <- 1
  }
  if (any(type %in% openair:::dateTypes))
    vars <- c(vars, "date")
  if (!is.null(pollutant))
    vars <- c(vars, pollutant)
  my_df <- openair:::checkPrep(my_df, vars, type, remove.calm = FALSE,
                               remove.neg = rm.neg)
  my_df <- na.omit(my_df)
  if (is.null(pollutant))
    pollutant <- ws
  my_df$x <- my_df[, pollutant]
  my_df[, wd] <- angle * ceiling(my_df[, wd]/angle - 0.5)
  my_df[, wd][my_df[, wd] == 0] <- 360
  my_df[, wd][my_df[, ws] == 0] <- -999
  if (length(breaks) == 1)
    breaks <- 0:(breaks - 1) * ws.int
  if (max(breaks) < max(my_df$x, na.rm = TRUE))
    breaks <- c(breaks, max(my_df$x, na.rm = TRUE))
  if (min(breaks) > min(my_df$x, na.rm = TRUE))
    warning("Some values are below minimum break.")
  breaks <- unique(breaks)
  my_df$x <- cut(my_df$x, breaks = breaks, include.lowest = FALSE,
                 dig.lab = dig.lab)
  theLabels <- gsub("[(]|[)]|[[]|[]]", "", levels(my_df$x))
  theLabels <- gsub("[,]", " to ", theLabels)
  prepare.grid <- function(my_df) {
    if (all(is.na(my_df$x)))
      return()
    levels(my_df$x) <- c(paste("x", 1:length(theLabels),
                               sep = ""))
    all <- stat.fun(my_df[, wd])
    calm <- my_df[my_df[, wd] == -999, ][, pollutant]
    my_df <- my_df[my_df[, wd] != -999, ]
    calm <- stat.fun(calm)
    weights <- tapply(my_df[, pollutant], list(my_df[,
                                                     wd], my_df$x), stat.fun)
    if (stat.scale == "all") {
      calm <- calm/all
      weights <- weights/all
    }
    if (stat.scale == "panel") {
      temp <- stat.fun(stat.fun(weights)) + calm
      calm <- calm/temp
      weights <- weights/temp
    }
    weights[is.na(weights)] <- 0
    weights <- t(apply(weights, 1, cumsum))
    if (stat.scale == "all" | stat.scale == "panel") {
      weights <- weights * 100
      calm <- calm * 100
    }
    panel.fun <- stat.fun2(my_df[, pollutant])
    u <- mean(sin(2 * pi * my_df[, wd]/360))
    v <- mean(cos(2 * pi * my_df[, wd]/360))
    mean.wd <- atan2(u, v) * 360/2/pi
    if (all(is.na(mean.wd))) {
      mean.wd <- NA
    }
    else {
      if (mean.wd < 0)
        mean.wd <- mean.wd + 360
      if (mean.wd > 180)
        mean.wd <- mean.wd - 360
    }
    weights <- cbind(data.frame(weights), wd = as.numeric(row.names(weights)),
                     calm = calm, panel.fun = panel.fun, mean.wd = mean.wd)
    weights
  }
  if (paddle) {
    poly <- function(wd, len1, len2, width, colour, x.off = 0,
                     y.off = 0) {
      theta <- wd * pi/180
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      x1 <- len1 * sin(theta) - width * cos(theta) + x.off
      x2 <- len1 * sin(theta) + width * cos(theta) + x.off
      x3 <- len2 * sin(theta) - width * cos(theta) + x.off
      x4 <- len2 * sin(theta) + width * cos(theta) + x.off
      y1 <- len1 * cos(theta) + width * sin(theta) + y.off
      y2 <- len1 * cos(theta) - width * sin(theta) + y.off
      y3 <- len2 * cos(theta) + width * sin(theta) + y.off
      y4 <- len2 * cos(theta) - width * sin(theta) + y.off
      lattice::lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour,
                        border = border)
    }
  }
  else {
    poly <- function(wd, len1, len2, width, colour, x.off = 0,
                     y.off = 0) {
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      theta <- seq((wd - seg * angle/2), (wd + seg * angle/2),
                   length.out = (angle - 2) * 10)
      theta <- ifelse(theta < 1, 360 - theta, theta)
      theta <- theta * pi/180
      x1 <- len1 * sin(theta) + x.off
      x2 <- rev(len2 * sin(theta) + x.off)
      y1 <- len1 * cos(theta) + x.off
      y2 <- rev(len2 * cos(theta) + x.off)
      lattice::lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
    }
  }
  my_df <- openair::cutData(my_df, type, ...)
  results.grid <- plyr::ddply(.data = my_df,
                              .variables = type,
                              .fun = prepare.grid)
  results.grid$calm <- stat.labcalm(results.grid$calm)
  results.grid$mean.wd <- stat.labcalm(results.grid$mean.wd)
  strip.dat <- openair:::strip.fun(results.grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  if (length(theLabels) < length(cols)) {
    col <- cols[1:length(theLabels)]
  }
  else {
    col <- openair::openColours(cols, length(theLabels))
  }
  max.freq <- max(results.grid[, (length(type) + 1):(length(theLabels) + length(type))], na.rm = TRUE)
  off.set <- max.freq * (offset/100)
  box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(theLabels))^4
  box.widths <- box.widths * max.freq * angle/5
  legend <- list(col = col, space = key.position, auto.text = auto.text,
                 labels = theLabels,
                 footer = key.footer, header = key.header,
                 height = 0.6, width = 1.5, fit = "scale",
                 plot.style = if (paddle) "paddle" else "other")
  legend <- openair:::makeOpenKeyLegend(key, legend, "windRose")
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("x1 ~ wd | ", temp, sep = ""))
  mymax <- 2 * max.freq
  myby <- if (is.null(grid.line))
    pretty(c(0, mymax), 10)[2]
  else grid.line
  if (myby/mymax > 0.9)
    myby <- mymax * 0.9
  xyplot.args <- list(x = myform,
                      xlim = 1.03 * c(- max.freq - off.set, max.freq + off.set),
                      ylim = 1.03 * c(- max.freq - off.set, max.freq + off.set),
                      data = results.grid, type = "n",
                      sub = stat.lab, strip = strip, strip.left = strip.left,
                      as.table = TRUE, aspect = 1, par.strip.text = list(cex = 0.8),
                      scales = list(draw = FALSE), panel = function(x, y,
                                                                    subscripts, ...) {
                        lattice::panel.xyplot(x, y, ...)
                        angles <- seq(0, 2 * pi, length = 360)
                        sapply(seq(off.set, mymax, by = myby), function(x) lattice::llines(x *
                                                                                             sin(angles), x * cos(angles), col = "grey85",
                                                                                           lwd = 1))
                        subdata <- results.grid[subscripts, ]
                        upper <- max.freq + off.set
                        lar <- upper * 0.9
                        for (langle in seq(0, 2 * pi, pi/4)) lattice::larrows(0, 0, lar * cos(langle), lar * (sin(langle)), code = 3,length = 0, col = 'grey')
                        for (langle in seq(0, 2 * pi, pi/2)) lattice::larrows(0, 0, lar * cos(langle), lar * (sin(langle)), code = 3,length = 0)
                        if (quantile_line) {
                          for (langle in quantile(my_df[, wd], seq(0.25, 1, 0.25)) / 180 * pi) lattice::larrows(0, 0, lar * sin(langle), lar * (cos(langle)), code = 1,length = 0, col = 'red')
                        }


                        # larrows(-upper * 0.9, 0, upper * 0.9, 0, code = 3,
                        #         length = 0)
                        # larrows(0, -upper * 0.9, 0, upper * 0.9, code = 3,
                        #         length = 0)

                        lattice::ltext(0 * upper, upper * 0.95, cust_labels[1], cex = 1)
                        lattice::ltext(upper * 0.95, 0 * upper, cust_labels[2], cex = 1)
                        lattice::ltext(0 * upper, upper * -1 * 0.95, cust_labels[3],
                                       cex = 1)
                        lattice::ltext(upper * -1 * 0.95, 0 * upper, cust_labels[4],
                                       cex = 1)
                        if (nrow(subdata) > 0) {
                          for (i in 1:nrow(subdata)) {
                            with(subdata, {
                              for (j in 1:length(theLabels)) {
                                if (j == 1) {
                                  temp <- "poly(wd[i], 0, x1[i], width * box.widths[1], col[1])"
                                } else {
                                  temp <- paste("poly(wd[i], x", j - 1,
                                                "[i], x", j, "[i], width * box.widths[",
                                                j, "], col[", j, "])", sep = "")
                                }
                                eval(parse(text = temp))
                              }
                            })
                          }
                        }
                        lattice::ltext(seq((myby + off.set), mymax, myby) * sin(pi/4),
                                       seq((myby + off.set), mymax, myby) * cos(pi/4),
                                       paste(seq(myby, mymax, by = myby), stat.unit,
                                             sep = ""), cex = 0.7)
                        if (annotate) if (statistic != "prop.mean") {
                          if (!diff) {
                            lattice::ltext(max.freq + off.set, -max.freq - off.set,
                                           label = paste(stat.lab2, " = ", subdata$panel.fun[1],
                                                         "\ncalm = ", subdata$calm[1], stat.unit,
                                                         sep = ""), adj = c(1, 0), cex = 0.7, col = calm.col)
                          }
                          if (diff) {
                            lattice::ltext(max.freq + off.set, -max.freq - off.set,
                                           label = paste("mean ws = ", round(subdata$panel.fun[1],
                                                                             1), "\nmean wd = ", round(subdata$mean.wd[1],
                                                                                                       1), sep = ""), adj = c(1, 0), cex = 0.7,
                                           col = calm.col)
                          }
                        } else {
                          lattice::ltext(max.freq + off.set, -max.freq - off.set,
                                         label = paste(stat.lab2, " = ", subdata$panel.fun[1],
                                                       stat.unit, sep = ""), adj = c(1, 0), cex = 0.7,
                                         col = calm.col)
                        }
                      }, legend = legend)
  xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)
  plt <- do.call(lattice::xyplot, xyplot.args)
  if (length(type) == 1)
    plot(plt)
  else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  newdata <- results.grid
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}
