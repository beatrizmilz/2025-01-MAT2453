library(ggplot2)

plot_line <- function(function_name,
                      x_start = -3,
                      x_end = 3,
                      y_start = NULL,
                      y_end = NULL,
                      step = 1,
                      cor = "red") {
  range_total <- x_start:x_end

  breaks_range_x <- seq(x_start, x_end, by = step)




  grafico <- ggplot(data.frame(x = range_total)) +
    aes(x = x) +
    coord_fixed(expand = FALSE, ylim = c(y_start, y_end)) +
    geom_hline(yintercept = 0, col = "black") +
    geom_vline(xintercept = 0, col = "black") +
    stat_function(fun = function_name, color = cor, n = 1000)  +
    scale_x_continuous(breaks = breaks_range_x) +
    theme_minimal() +
    labs(x = "", y = "")



  if (!is.null(y_start)) {
    breaks_range_y <- seq(y_start, y_end, by = step)

    grafico <-  grafico + scale_y_continuous(breaks = breaks_range_y)
  }

  grafico
}
