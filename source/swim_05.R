
# set up ------------------------------------------------------------------

name <- "swim"
version <- 05

# define common helper functions & core tools
source(here::here("source", "common.R"), echo = FALSE)
source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)


# this isn't needed... can pass bg to png via safe_gif
if (FALSE) {

  # overwrite the core draw method
  method(draw, sketch) <- function(object, xlim = NULL, ylim = NULL, bg = "white", ...) {

    # set default axis limits
    if (is.null(xlim)) {
      xlim <- c(
        min(purrr::map_dbl(object@shapes, \(s, id) min(s@points@x))),
        max(purrr::map_dbl(object@shapes, \(s, id) max(s@points@x)))
      )
    }
    if (is.null(ylim)) {
      ylim <- c(
        min(purrr::map_dbl(object@shapes, \(s) min(s@points@y))),
        max(purrr::map_dbl(object@shapes, \(s) max(s@points@y)))
      )
    }

    # plotting area is a single viewport with equal-axis scaling
    x_width <- xlim[2] - xlim[1]
    y_width <- ylim[2] - ylim[1]
    vp <- grid::viewport(
      xscale = xlim,
      yscale = ylim,
      width  = grid::unit(min(1, x_width / y_width), "snpc"),
      height = grid::unit(min(1, y_width / x_width), "snpc")
    )

    # draw the grobs
    grid::grid.newpage()
    grid::grid.draw(grid::rectGrob(gp = grid::gpar(fill = bg))) # set bg

    for(s in object@shapes) {
      grob <- grid::polygonGrob(
        x = s@points@x,
        y = s@points@y,
        gp = grid::gpar(
          col = s@style@color,
          fill = s@style@fill,
          lwd = s@style@linewidth
        ),
        vp = vp,
        default.units = "native"
      )
      grid::grid.draw(grob)
    }
  }

}

make_art <- function(seed, name, version) {

  # specify the output path and message the user
  output <- output_path(name, version, seed, format = "gif")
  message("generating art at ", output)

  set.seed(seed)

  # set up palettes
  palettes <- c("palette_01.csv", "palette_02.csv", "palette_02.csv") |>
    purrr::map(
      \(x) here::here("source", x) |>
        readr::read_csv(show_col_types = FALSE)
    ) |>
    dplyr::bind_rows()

  # select a palette
  row <- sample.int(nrow(palettes), 1)
  palette <- unlist(palettes[row, ])
  palette <- sample(palette)

  # parameters that affect all bezier objects
  pull_1 <- runif(1, min = -.1, max = .1) * .75
  pull_2 <- runif(1, min = 0, max = .2) * .75
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = 1, max = 3)
  max_arc <- runif(1, min = 1, max = 2) * pi/96
  r_len <- pi/12

  # boring lissajous parameters
  A <- 1 # width of the curve
  B <- 1 # height of the curve

  # fun lissajous parameters
  a <- 1 + rbinom(1, size = 20, prob = .5)
  b <- 1 + rbinom(1, size = 20, prob = .5)
  delta <- pi/2

  # rescale width
  width_scale <- width_scale * (1 + sqrt(abs(a - b))) * 1.2

  make_drawables <- function(rot) {
    set.seed(seed)
    n_ribbons <- 50L
    values <- tibble::tibble(
      theta_1 = runif(n_ribbons, min = 0, max = r_len) + rot,
      theta_2 = theta_1 + runif(n_ribbons, min = 0, max = max_arc),
      x = A * sin(a * theta_1 + delta),
      y = B * sin(b * theta_1),
      xend = A * sin(a * theta_2 + delta),
      yend = B * sin(b * theta_2),
      xctr_1 = (1 - pull_1) * (x + xend)/2 + pull_1 * x_mid,
      yctr_1 = (1 - pull_1) * (y + yend)/2 + pull_1 * y_mid,
      xctr_2 = (x + xend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
      yctr_2 = (y + yend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
      width = width_scale * runif(n_ribbons, min = .01, max = .2),
      smooth = 6L,
      n = 100L,
      fill = sample(palette, n_ribbons, replace = TRUE),
      color = fill
    )
    values <- dplyr::select(values, -theta_1, -theta_2)
    purrr::pmap(values, bezier_ribbon)
  }

  gifski::save_gif(
    expr = {
      for(rot in seq(0, 2*pi, length.out = 500)) {
        drawables <- make_drawables(rot)
        drawables |>
          sketch() |>
          draw(
            xlim = c(-2, 2),
            ylim = c(-2, 2),
            bg = palette[1]
          )
      }
    },
    gif_file = output,
    delay = .1,
    width = 800,
    height = 800,
    bg = palette[1]
  )

}

for(s in 510:599) make_art(s, name, version)

