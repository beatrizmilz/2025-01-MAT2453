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


circulo_unitario <- function(theta = pi/4, label_theta = TRUE){

# Dados para o círculo unitário
circle_all <- seq(0, 2*pi, length.out = 300)
circle <- data.frame(x = cos(circle_all), y = sin(circle_all))

# Dados para o arco que vai de 0 a theta
theta_seq <- seq(0, theta, length.out = 100)
theta_arc <- data.frame(x = cos(theta_seq), y = sin(theta_seq))

theta_arc_tail <- dplyr::slice_tail(theta_arc, n = 1)


# Construindo o gráfico
grafico <- ggplot() +
  # Linhas que dividem os 4 quadrantes (eixos x e y)
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = 0, color = "gray50") +

  # Não permitir distorções nas escalas
  coord_fixed() +
  # Desenha o círculo unitário
  geom_path(data = circle, aes(x = x, y = y), color = "gray50") +

    geom_segment(aes(x = 0, y = 0, xend = theta_arc_tail$x, yend = theta_arc_tail$y), color = "black", size = 1) +


  # Destaca o arco correspondente ao ângulo theta com uma linha grossa
  geom_path(data = theta_arc, aes(x = x, y = y), color = "red", linewidth = 1.5) +
  theme_minimal()

if (label_theta) {
  grafico <- grafico +
    # Anotação do ângulo (usando expressão para mostrar theta)
    annotate(
      "text",
      x = 1.1,
      y = 0.1,
      label = expression(theta),
      size = 6,
      color = "red"
    )


}

  grafico

}


