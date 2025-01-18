
# set up ------------------------------------------------------------------

name <- "swim"
version <- 08

# define common helper functions & core tools
source(here::here("source", "common.R"), echo = FALSE)
source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)


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
  pull_1 <- runif(1, min = -.1, max = .1) * .5
  pull_2 <- runif(1, min = 0, max = .2) * .5
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = 1, max = 3)
  max_arc <- runif(1, min = 1, max = 2) * pi/18
  r_len <- pi/12

  # boring lissajous parameters
  A <- 1 # width of the curve
  B <- 1 # height of the curve

  # fun lissajous parameters
  a_1 <- 1 + rbinom(1, size = 20, prob = .1)
  b_1 <- a_1 # 1 + rbinom(1, size = 20, prob = .5)
  a_2 <- 1 + rbinom(1, size = 20, prob = .1)
  b_2 <- a_2 # 1 + rbinom(1, size = 20, prob = .5)
  delta <- pi/2

  direction <- sample(c(-1, 1), size = 1, replace = 2)
  #offset <- runif(1, min = -pi/2, max = pi/2)
  #inner_scale <- runif(1, min = .4, max = .8)

  make_drawables <- function(rot, a, b, A = 1, B = 1) {

    # rescale width
    width_scale <- width_scale * (1 + sqrt(abs(a - b))) * 1.2

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
      width = width_scale * runif(n_ribbons, min = .1, max = .4),
      smooth = 6L,
      n = 100L,
      fill = sample(palette, n_ribbons, replace = TRUE),
      color = fill
    )
    values <- dplyr::select(values, -theta_1, -theta_2)
    purrr::pmap(values, bezier_ribbon)
  }

  pb <- progress::progress_bar$new(
    format = "  rendering [:bar] :percent eta: :eta",
    total = 500,
    clear = FALSE,
    width= 60
  )

  gifski::save_gif(
    expr = {
      for(rot in seq(0, 2*pi, length.out = 300)) {
        drawables <- c(
          make_drawables(direction[1] * rot, a_1, b_1)
          #make_drawables(offset + direction[1] * rot, a_1, b_1, A = inner_scale, B = inner_scale)
        )
        drawables |>
          sketch() |>
          draw(
            xlim = c(-2, 2),
            ylim = c(-2, 2),
            bg = palette[1]
          )
        pb$tick()
      }
    },
    gif_file = output,
    delay = .1,
    width = 800,
    height = 800,
    bg = palette[1]
  )

}

for(s in 800:899) make_art(s, name, version)

