library(magick)

generate_shape_with_gradient <- function(type, width, height, transparency) {
  if (type == "rectangle") {
    shape <- image_blank(width = width, height = height, color = "none")
    gradient_colors <- sample(colors, 2)
    shape <- image_draw(shape)
    rect(0, 0, width, height, col = gradient_colors[1], border = NA)
    for (i in seq(0, height, by = 1)) {
      color <- adjustcolor(gradient_colors[1], alpha.f = transparency * (1 - i / height))
      rect(0, i, width, i + 1, col = color, border = NA)
    }
    dev.off()
  } else if (type == "circle") {
    shape <- image_blank(width = width, height = height, color = "none")
    shape <- image_draw(shape)
    gradient_colors <- sample(colors, 2)
    symbols(x = width / 2, y = height / 2, circles = min(width, height) / 2,
            bg = gradient_colors[1], inches = FALSE, add = TRUE)
    for (i in seq(0, min(width, height) / 2, by = 1)) {
      color <- adjustcolor(gradient_colors[1], alpha.f = transparency * (1 - i / (min(width, height) / 2)))
      symbols(x = width / 2, y = height / 2, circles = i,
              bg = color, inches = FALSE, add = TRUE)
    }
    dev.off()
  }
  return(shape)
}

create_gradient_background <- function(width, height) {
  gradient <- image_blank(width = width, height = height, color = "white")
  gradient <- image_draw(gradient)
  rect(0, 0, width, height, col = "lightblue", border = NA)
  for (i in seq(0, height, by = 10)) {
    color <- adjustcolor("lightblue", alpha.f = 1 - i / height)
    rect(0, i, width, i + 10, col = color, border = NA)
  }
  dev.off()
  return(gradient)
}

create_shadow <- function(shape, offset_x = 5, offset_y = 5) {
  shadow <- image_transparent(shape, color = "black")
  shadow <- image_composite(shadow, shape, offset = sprintf("+%d+%d", offset_x, offset_y))
  return(shadow)
}

create_star <- function(x, y, size) {
  star <- image_blank(width = size, height = size, color = "none")
  star <- image_draw(star)
  stars <- list(
    list(x = 0.5, y = 0.1), list(x = 0.1, y = 0.4), list(x = 0.9, y = 0.4),
    list(x = 0.2, y = 0.9), list(x = 0.5, y = 0.6), list(x = 0.8, y = 0.9)
  )
  for (point in stars) {
    points(x = point$x * size, y = point$y * size, pch = 8, col = "yellow", cex = 1.5)
  }
  dev.off()
  return(star)
}

add_pattern <- function(shape, width, height) {
  for (j in seq(1, width, by = 10)) {
    line_color <- sample(colors, 1)
    shape <- image_draw(shape)
    segments(x0 = j, y0 = 0, x1 = j, y1 = height, col = line_color, lwd = 2)
    segments(x0 = 0, y0 = j, x1 = width, y1 = j, col = line_color, lwd = 2)
    dev.off()
  }
  return(shape)
}

canvas_width <- 800
canvas_height <- 800
num_shapes <- 150
shape_types <- c("rectangle", "circle")
colors <- c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#FF69B4", "#8A2BE2", "#00FF7F", "#FF4500")

canvas <- create_gradient_background(canvas_width, canvas_height)

set.seed(42)
radius <- 300
for (i in 1:num_shapes) {
  angle <- runif(1, min = 0, max = 2 * pi)
  x <- canvas_width / 2 + radius * cos(angle)
  y <- canvas_height / 2 + radius * sin(angle)
  width <- runif(1, min = 50, max = 100)
  height <- runif(1, min = 50, max = 100)
  shape_type <- sample(shape_types, 1)
  transparency <- runif(1, min = 0.3, max = 1.0)
  rotation_angle <- runif(1, min = 0, max = 360)

  shape <- generate_shape_with_gradient(shape_type, width, height, transparency)
  shape <- add_pattern(shape, width, height)
  shadow <- create_shadow(shape)

  canvas <- image_composite(canvas, shadow, offset = sprintf("+%d+%d", as.integer(x), as.integer(y)))
  canvas <- image_composite(canvas, shape, offset = sprintf("+%d+%d", as.integer(x), as.integer(y)))

  star_size <- sample(5:15, 1)
  star <- create_star(as.integer(x) + sample(-10:10, 1), as.integer(y) + sample(-10:10, 1), star_size)
  canvas <- image_composite(canvas, star, offset = sprintf("+%d+%d", as.integer(x), as.integer(y)))
}

for (i in 1:(num_shapes - 1)) {
  angle1 <- runif(1, min = 0, max = 2 * pi)
  angle2 <- runif(1, min = 0, max = 2 * pi)
  x1 <- canvas_width / 2 + radius * cos(angle1)
  y1 <- canvas_height / 2 + radius * sin(angle1)
  x2 <- canvas_width / 2 + radius * cos(angle2)
  y2 <- canvas_height / 2 + radius * sin(angle2)

  line_color <- sample(colors, 1)
  canvas <- image_draw(canvas)
  segments(x0 = x1, y0 = y1, x1 = x2, y1 = y2, col = line_color, lwd = 1)
  dev.off()
}

save_path <- "enhanced_art.png"

image_write(canvas, path = save_path, format = "png")

browseURL(save_path)

cat("Bild wurde gespeichert unter:", save_path, "\n")
