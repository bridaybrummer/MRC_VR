white <- "#FFFFFF"

circle_xy <- function(x, y, radius, n = 100) {
  theta <- seq(0, 2 * pi, length.out = n)
  list(x = x + radius * cos(theta), y = y + radius * sin(theta))
}

draw_circle <- function(x, y, radius, col = NA, border = white, lwd = 2) {
  coordinates <- circle_xy(x, y, radius)
  polygon(coordinates$x, coordinates$y, col = col, border = border, lwd = lwd)
}

ellipse_xy <- function(x, y, radius_x, radius_y, from = 0, to = 360, n = 100) {
  theta <- seq(from, to, length.out = n) * pi / 180
  list(x = x + radius_x * cos(theta), y = y + radius_y * sin(theta))
}

draw_ellipse <- function(x, y, radius_x, radius_y, col = NA,
                         border = white, lwd = 2) {
  coordinates <- ellipse_xy(x, y, radius_x, radius_y)
  polygon(coordinates$x, coordinates$y, col = col, border = border, lwd = lwd)
}

draw_arc <- function(x, y, radius, from, to, lwd = 2) {
  theta <- seq(from, to, length.out = 80) * pi / 180
  lines(x + radius * cos(theta), y + radius * sin(theta), col = white, lwd = lwd)
}

draw_arrow_head <- function(x, y, angle, size = 0.08) {
  direction <- angle * pi / 180
  side <- direction + c(2.55, -2.55)
  polygon(
    c(x, x + size * cos(side)),
    c(y, y + size * sin(side)),
    col = white,
    border = white
  )
}

make_segment <- function(angle, r_inner = 0.91, r_outer = 2.35,
                         width = 42, n = 120) {
  theta <- seq(angle - width / 2, angle + width / 2, length.out = n) * pi / 180
  list(
    x = c(r_inner * cos(theta), rev(r_outer * cos(theta))),
    y = c(r_inner * sin(theta), rev(r_outer * sin(theta)))
  )
}

draw_trend_icon <- function(x, y, scale = 1) {
  bar_x <- x + c(-0.34, -0.10, 0.14, 0.38) * scale
  heights <- c(0.22, 0.40, 0.29, 0.60) * scale
  for (i in seq_along(bar_x)) {
    rect(bar_x[i] - 0.075 * scale, y - 0.30 * scale,
         bar_x[i] + 0.075 * scale, y - 0.30 * scale + heights[i],
         col = white, border = white)
  }
  line_x <- x + c(-0.39, -0.13, 0.10, 0.35, 0.54) * scale
  line_y <- y + c(0.15, 0.37, 0.20, 0.45, 0.69) * scale
  lines(line_x, line_y, col = white, lwd = 4 * scale,
        lend = "round", ljoin = "round")
  points(line_x[-length(line_x)], line_y[-length(line_y)],
         pch = 16, col = white, cex = 0.8 * scale)
  draw_arrow_head(tail(line_x, 1), tail(line_y, 1), 48, 0.16 * scale)
}

draw_germ <- function(x, y, radius) {
  draw_circle(x, y, radius, col = white, border = white, lwd = 1)
  theta <- seq(0, 2 * pi, length.out = 9)[-9]
  x1 <- x + radius * cos(theta)
  y1 <- y + radius * sin(theta)
  x2 <- x + radius * 1.45 * cos(theta)
  y2 <- y + radius * 1.45 * sin(theta)
  segments(x1, y1, x2, y2, col = white, lwd = 1.4)
  points(x2, y2, pch = 16, col = white, cex = 0.22)
}

draw_surveillance_icon <- function(x, y, scale = 1) {
  draw_circle(x - 0.09 * scale, y + 0.08 * scale, 0.38 * scale,
              border = white, lwd = 5 * scale)
  segments(x + 0.18 * scale, y - 0.19 * scale,
           x + 0.49 * scale, y - 0.50 * scale,
           col = white, lwd = 9 * scale, lend = "round")
  draw_germ(x - 0.16 * scale, y + 0.20 * scale, 0.055 * scale)
  draw_germ(x + 0.08 * scale, y + 0.13 * scale, 0.075 * scale)
  draw_germ(x - 0.08 * scale, y - 0.10 * scale, 0.060 * scale)
  draw_germ(x + 0.02 * scale, y + 0.36 * scale, 0.035 * scale)
}

draw_health_planning_icon <- function(x, y, scale = 1) {
  background <- "#777777"

  rect(x - 0.52 * scale, y - 0.30 * scale,
    x - 0.24 * scale, y + 0.25 * scale, col = white, border = white)
  rect(x - 0.24 * scale, y - 0.30 * scale,
    x + 0.25 * scale, y + 0.55 * scale, col = white, border = white)
  rect(x + 0.25 * scale, y - 0.30 * scale,
    x + 0.52 * scale, y + 0.18 * scale, col = white, border = white)

  rect(x - 0.055 * scale, y + 0.23 * scale,
    x + 0.075 * scale, y + 0.47 * scale, col = background, border = NA)
  rect(x - 0.12 * scale, y + 0.29 * scale,
    x + 0.14 * scale, y + 0.41 * scale, col = background, border = NA)

  window_positions <- rbind(
    c(-0.39, 0.05), c(-0.39, -0.13),
    c(0.00, 0.05), c(0.00, -0.13),
    c(0.39, 0.00), c(0.39, -0.16)
  )
  for (i in seq_len(nrow(window_positions))) {
    window_x <- x + window_positions[i, 1] * scale
    window_y <- y + window_positions[i, 2] * scale
    rect(window_x - 0.05 * scale, window_y - 0.055 * scale,
      window_x + 0.05 * scale, window_y + 0.055 * scale,
      col = background, border = NA)
  }

  person_x <- x + c(-0.23, 0.10, 0.40) * scale
  person_y <- y + c(-0.28, -0.31, -0.32) * scale
  person_radius <- c(0.13, 0.11, 0.10) * scale
  for (i in seq_along(person_x)) {
    draw_circle(person_x[i], person_y[i], person_radius[i],
       col = background, border = white, lwd = 3 * scale)
    shoulder <- ellipse_xy(person_x[i], person_y[i] - 0.25 * scale,
            person_radius[i] * 1.55, 0.22 * scale, 10, 170)
    lines(shoulder$x, shoulder$y, col = white, lwd = 3 * scale,
    lend = "round")
  }
}

draw_target_icon <- function(x, y, scale = 1) {
  for (radius in c(0.48, 0.34, 0.19) * scale) {
    draw_circle(x - 0.05 * scale, y - 0.03 * scale, radius,
                border = white, lwd = 5 * scale)
  }
  segments(x - 0.01 * scale, y + 0.01 * scale,
           x + 0.48 * scale, y + 0.50 * scale,
           col = white, lwd = 6 * scale, lend = "round")
  draw_arrow_head(x + 0.55 * scale, y + 0.57 * scale, 45, 0.20 * scale)
  polygon(
    x + c(0.31, 0.51, 0.44, 0.24) * scale,
    y + c(0.37, 0.57, 0.67, 0.47) * scale,
    col = white, border = white
  )
}

draw_policy_icon <- function(x, y, scale = 1) {
  polygon(
    x + c(-0.54, 0, 0.54) * scale,
    y + c(0.20, 0.48, 0.20) * scale,
    col = white, border = white
  )
  rect(x - 0.57 * scale, y + 0.10 * scale,
       x + 0.57 * scale, y + 0.20 * scale, col = white, border = white)
  for (column_x in x + c(-0.38, -0.13, 0.13, 0.38) * scale) {
    rect(column_x - 0.055 * scale, y - 0.32 * scale,
         column_x + 0.055 * scale, y + 0.09 * scale,
         col = white, border = white)
  }
  rect(x - 0.58 * scale, y - 0.43 * scale,
       x + 0.58 * scale, y - 0.31 * scale, col = white, border = white)
  segments(x, y + 0.48 * scale, x, y + 0.76 * scale,
           col = white, lwd = 3 * scale)
  polygon(
    x + c(0.01, 0.25, 0.01) * scale,
    y + c(0.75, 0.67, 0.59) * scale,
    col = white, border = white
  )
}

draw_microscope_icon <- function(x, y, scale = 1) {
  background <- "#777777"

  polygon(
    x + c(-0.38, -0.12, -0.02, -0.28) * scale,
    y + c(0.55, 0.45, 0.60, 0.70) * scale,
    col = white, border = white
  )
  polygon(
    x + c(-0.32, -0.13, 0.15, -0.05) * scale,
    y + c(0.40, 0.31, -0.12, -0.22) * scale,
    col = white, border = white
  )
  rect(x - 0.39 * scale, y + 0.17 * scale,
       x - 0.12 * scale, y + 0.27 * scale, col = white, border = white)

  draw_circle(x + 0.17 * scale, y + 0.04 * scale, 0.17 * scale,
              col = white, border = white, lwd = 1)
  draw_circle(x + 0.17 * scale, y + 0.04 * scale, 0.07 * scale,
              col = background, border = background, lwd = 1)

  arm_outer <- ellipse_xy(x + 0.05 * scale, y - 0.10 * scale,
                          0.39 * scale, 0.46 * scale, 242, 72)
  lines(arm_outer$x, arm_outer$y, col = white, lwd = 9 * scale,
        lend = "round")

  polygon(
    x + c(-0.29, 0.28, 0.36, -0.35) * scale,
    y + c(-0.25, -0.25, -0.15, -0.15) * scale,
    col = white, border = white
  )
  rect(x - 0.30 * scale, y - 0.37 * scale,
       x + 0.32 * scale, y - 0.27 * scale, col = white, border = white)
  polygon(
    x + c(-0.50, 0.43, 0.55, -0.60) * scale,
    y + c(-0.55, -0.55, -0.43, -0.43) * scale,
    col = white, border = white
  )
}

draw_research_icon <- function(x, y, scale = 1) {
  rect(x - 0.52 * scale, y - 0.30 * scale,
       x + 0.52 * scale, y + 0.40 * scale,
       col = white, border = white)
  rect(x - 0.42 * scale, y - 0.20 * scale,
       x + 0.42 * scale, y + 0.30 * scale,
       col = "#EAD7B2", border = NA)
  polygon(
    x + c(-0.60, 0.60, 0.52, -0.52) * scale,
    y + c(-0.43, -0.43, -0.31, -0.31) * scale,
    col = white, border = white
  )
  for (i in 1:3) {
    rect(x + (0.08 + 0.17 * i) * scale, y - 0.13 * scale,
         x + (0.18 + 0.17 * i) * scale, y + (-0.04 + 0.13 * i) * scale,
         col = white, border = white)
  }
  draw_circle(x - 0.20 * scale, y + 0.05 * scale, 0.19 * scale,
              col = white, border = white, lwd = 1)
  polygon(
    x + c(-0.20, -0.20, -0.01) * scale,
    y + c(0.05, 0.24, 0.05) * scale,
    col = "#EAD7B2", border = "#EAD7B2"
  )
}

draw_database_icon <- function(x, y, scale = 1) {
  background <- "#666666"
  database_x <- x - 0.10 * scale
  rect(database_x - 0.34 * scale, y - 0.28 * scale,
       database_x + 0.34 * scale, y + 0.35 * scale,
       col = white, border = white)
  draw_ellipse(database_x, y + 0.35 * scale,
               0.34 * scale, 0.12 * scale, col = white, border = white)
  draw_ellipse(database_x, y - 0.28 * scale,
               0.34 * scale, 0.12 * scale, col = white, border = white)
  for (ellipse_y in y + c(0.14, -0.07) * scale) {
    coordinates <- ellipse_xy(database_x, ellipse_y,
                              0.34 * scale, 0.11 * scale, 180, 360)
    lines(coordinates$x, coordinates$y, col = background, lwd = 3 * scale)
  }

  draw_circle(x + 0.28 * scale, y - 0.16 * scale, 0.25 * scale,
              col = background, border = background, lwd = 1)
  draw_arc(x + 0.28 * scale, y - 0.16 * scale, 0.22 * scale, 35, 285,
           lwd = 5 * scale)
  draw_arrow_head(x + 0.39 * scale, y - 0.36 * scale, -68, 0.15 * scale)
}

draw_mortality_figure <- function(filename, width = 10, height = 10, dpi = 300) {
  labels <- c(
    "MONITOR TRENDS\nIN MORTALITY",
    "SUPPORT\nSURVEILLANCE\nSYSTEMS",
    "GUIDE HEALTH PLANNING\nAND RESOURCE\nALLOCATION",
    "EVALUATE EFFECTIVENESS\nOF HEALTH\nINTERVENTIONS AND\nPROGRAMMES",
    "INFORM PUBLIC HEALTH\nPOLICY AND\nDECISION-MAKING",
    "IDENTIFY\nPRIORITIES FOR\nRESEARCH",
    "CONTRIBUTE TO\nRESEARCH\nPROJECTS",
    "MAINTAIN AND\nUPDATE NATIONAL\nDATABASES"
  )
  angles <- c(90, 45, 0, -45, -90, -135, 180, 135)
  colours <- c(
    "#E88919", "#E5A044", "#777777", "#F0DFC0",
    "#E88919", "#777777", "#F0DFC0", "#666666"
  )
  icon_functions <- list(
    draw_trend_icon, draw_surveillance_icon, draw_health_planning_icon,
    draw_target_icon, draw_policy_icon, draw_microscope_icon,
    draw_research_icon, draw_database_icon
  )
  icon_x <- c(0.00, 1.20, 1.72, 1.12, 0.00, -1.12, -1.72, -1.35)
  icon_y <- c(1.76, 1.28, 0.27, -1.12, -1.72, -1.14, 0.25, 1.27)
  label_x <- c(0.00, 1.39, 1.46, 1.06, 0.00, -1.06, -1.46, -1.34)
  label_y <- c(1.18, 0.80, -0.31, -1.60, -1.15, -1.59, -0.31, 0.78)
  label_cex <- c(0.88, 0.82, 0.74, 0.64, 0.74, 0.73, 0.78, 0.78)

  png(filename, width = width, height = height, units = "in", res = dpi,
      bg = "white", type = "cairo")
  on.exit(dev.off(), add = TRUE)

  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot(NA, NA, xlim = c(-2.48, 2.48), ylim = c(-2.48, 2.48),
       asp = 1, axes = FALSE, xlab = "", ylab = "", bty = "n")

  for (i in seq_along(angles)) {
    segment <- make_segment(angles[i])
    polygon(segment$x, segment$y, col = colours[i], border = white, lwd = 5)
  }

  draw_circle(0, 0, 0.87, col = "#626262", border = white, lwd = 5)
  text(0, 0, "USES OF\nMORTALITY\nDATA", col = white, font = 2,
       family = "sans", cex = 1.65, lheight = 0.88)

  for (i in seq_along(angles)) {
    radians <- angles[i] * pi / 180
    icon_functions[[i]](icon_x[i], icon_y[i], scale = 0.52)

    text(label_x[i], label_y[i], labels[i], col = white, font = 2,
         family = "sans", cex = label_cex[i], lheight = 0.88)

    number_radius <- 2.31
    number_x <- number_radius * cos(radians)
    number_y <- number_radius * sin(radians)
    draw_circle(number_x, number_y, 0.13, col = colours[i],
                border = white, lwd = 2.5)
    text(number_x, number_y, i, col = white, font = 2,
         family = "sans", cex = 0.90)
  }

  invisible(filename)
}