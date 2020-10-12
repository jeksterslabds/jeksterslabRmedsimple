#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Type I Error Rates
#'
#' @family plotting functions
#' @keywords plot
#' @import ggplot2
#' @param data Dataframe of results.
#' @param alpha_level Numeric.
#'   Available values are `0.05`, `0.01`, `0.001`.
#' @param criteria Character string.
#'   Available values are `"serlin"`, `"liberal"`, `"moderate"`, `"strict"`.
#' @export
plot_type1 <- function(data,
                       alpha_level = 0.05,
                       criteria = "serlin") {
  theta_label <- zero_hit_95 <- zero_hit_99 <- zero_hit_99.9 <- Method <- NULL
  # only zero effect sizes
  data <- data[which(data$theta == 0), ]
  data$theta_label <- factor(
    data$theta_label,
    levels = c(
      "0.00(0.00,0.00)",
      "0.00(0.00,0.14)",
      "0.00(0.14,0.00)",
      "0.00(0.00,0.36)",
      "0.00(0.36,0.00)",
      "0.00(0.00,0.51)",
      "0.00(0.51,0.00)"
    )
  )
  if (criteria == "serlin") {
    ll <- mean(data$serlin_ll_95)
    ul <- mean(data$serlin_ul_95)
    alpha_level <- 0.05
  }
  if (criteria == "liberal") {
    if (alpha_level == 0.001) {
      ll <- mean(data$liberal_ll_99.9)
      ul <- mean(data$liberal_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$liberal_ll_99)
      ul <- mean(data$liberal_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$liberal_ll_95)
      ul <- mean(data$liberal_ul_95)
    }
  }
  if (criteria == "moderate") {
    if (alpha_level == 0.001) {
      ll <- mean(data$moderate_ll_99.9)
      ul <- mean(data$moderate_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$moderate_ll_99)
      ul <- mean(data$moderate_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$moderate_ll_95)
      ul <- mean(data$moderate_ul_95)
    }
  }
  if (criteria == "strict") {
    if (alpha_level == 0.001) {
      ll <- mean(data$strict_ll_99.9)
      ul <- mean(data$strict_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$strict_ll_99)
      ul <- mean(data$strict_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$strict_ll_95)
      ul <- mean(data$strict_ul_95)
    }
  }
  if (alpha_level == 0.001) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = 1 - zero_hit_99.9,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.01) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = 1 - zero_hit_99,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.05) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = 1 - zero_hit_95,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  ylab <- paste0(
    "Type I Error [",
    ll,
    " \u003c ",
    alpha_level,
    " \u003c ",
    ul,
    "]"
  )
  p + geom_hline(
    yintercept = alpha_level,
    alpha = 0.5
  ) +
    geom_hline(
      yintercept = ll,
      alpha = 0.5
    ) +
    geom_hline(
      yintercept = ul,
      alpha = 0.5
    ) +
    annotate(
      geom = "rect",
      fill = "grey",
      alpha = 0.75,
      xmin = -Inf,
      xmax = Inf,
      ymin = ll,
      ymax = ul
    ) +
    geom_point(
      size = 0.75
    ) +
    geom_line() +
    facet_grid(
      factor(
        taudot_label,
        levels = rev(
          levels(
            taudot_label
          )
        )
      ) ~ n_label
    ) +
    xlab("\u03b1\u03b2(\u03b1, \u03b2)") +
    ylab(ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = -90
      )
    )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Power
#'
#' @family plotting functions
#' @keywords plot
#' @import ggplot2
#' @inheritParams plot_type1
#' @export
plot_power <- function(data,
                       alpha_level = 0.05) {
  theta_label <- power_95 <- power_99 <- power_99.9 <- Method <- NULL
  # only non-zero effect sizes
  data <- data[which(data$theta > 0), ]
  power_99.9 <- power_99 <- power_95 <- NULL
  if (alpha_level == 0.001) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = power_99.9,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.01) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = power_99,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.05) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = power_95,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  ylab <- "Power"
  p + geom_hline(
    yintercept = 0.80,
    alpha = 0.5
  ) +
    geom_point(
      size = 0.75
    ) +
    geom_line() +
    facet_grid(
      factor(
        taudot_label,
        levels = rev(
          levels(
            taudot_label
          )
        )
      ) ~ n_label
    ) +
    scale_y_continuous(
      breaks = c(
        0.00,
        0.20,
        0.40,
        0.60,
        0.80
      )
    ) +
    xlab("\u03b1\u03b2(\u03b1, \u03b2)") +
    ylab(ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = -90
      )
    )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Miss Rates for Non-Zero Effect Sizes
#'
#' @family plotting functions
#' @keywords plot
#' @import ggplot2
#' @inheritParams plot_type1
#' @export
plot_miss <- function(data,
                      alpha_level = 0.05,
                      criteria = "serlin") {
  theta_label <- theta_miss_95 <- theta_miss_99 <- theta_miss_99.9 <- Method <- NULL
  # only non-zero effect sizes
  data <- data[which(data$theta > 0), ]
  if (criteria == "serlin") {
    ll <- mean(data$serlin_ll_95)
    ul <- mean(data$serlin_ul_95)
    alpha_level <- 0.05
  }
  if (criteria == "liberal") {
    if (alpha_level == 0.001) {
      ll <- mean(data$liberal_ll_99.9)
      ul <- mean(data$liberal_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$liberal_ll_99)
      ul <- mean(data$liberal_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$liberal_ll_95)
      ul <- mean(data$liberal_ul_95)
    }
  }
  if (criteria == "moderate") {
    if (alpha_level == 0.001) {
      ll <- mean(data$moderate_ll_99.9)
      ul <- mean(data$moderate_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$moderate_ll_99)
      ul <- mean(data$moderate_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$moderate_ll_95)
      ul <- mean(data$moderate_ul_95)
    }
  }
  if (criteria == "strict") {
    if (alpha_level == 0.001) {
      ll <- mean(data$strict_ll_99.9)
      ul <- mean(data$strict_ul_99.9)
    }
    if (alpha_level == 0.01) {
      ll <- mean(data$strict_ll_99)
      ul <- mean(data$strict_ul_99)
    }
    if (alpha_level == 0.05) {
      ll <- mean(data$strict_ll_95)
      ul <- mean(data$strict_ul_95)
    }
  }
  if (alpha_level == 0.001) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = theta_miss_99.9,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.01) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = theta_miss_99,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  if (alpha_level == 0.05) {
    p <- ggplot(
      data = data,
      aes(
        x = theta_label,
        y = theta_miss_95,
        color = Method,
        group = Method,
        linetype = Method
      )
    )
  }
  ylab <- paste0(
    "Miss Rate [",
    ll,
    " \u003c ",
    alpha_level,
    " \u003c ",
    ul,
    "]"
  )
  p + geom_hline(
    yintercept = alpha_level,
    alpha = 0.5
  ) +
    geom_hline(
      yintercept = ll,
      alpha = 0.5
    ) +
    geom_hline(
      yintercept = ul,
      alpha = 0.5
    ) +
    annotate(
      geom = "rect",
      fill = "grey",
      alpha = 0.75,
      xmin = -Inf,
      xmax = Inf,
      ymin = ll,
      ymax = ul
    ) +
    geom_point(
      size = 0.75
    ) +
    geom_line() +
    facet_grid(
      factor(
        taudot_label,
        levels = rev(
          levels(
            taudot_label
          )
        )
      ) ~
      n_label
    ) +
    xlab("\u03b1\u03b2(\u03b1, \u03b2)") +
    ylab(ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = -90
      )
    )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Kurtosis
#'
#' @family plotting functions
#' @keywords plot
#' @import ggplot2
#' @inheritParams plot_type1
#' @inheritParams label
#' @export
plot_kurt <- function(data,
                      std = FALSE) {
  alphahatbetahat_kurt <- alphahatprimebetahatprime_kurt <- taudot_label <- NULL
  if (std) {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatprimebetahatprime_kurt,
        group = 1
      )
    )
  } else {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatbetahat_kurt,
        group = 1
      )
    )
  }
  p + geom_point(
    color = "red",
    size = 0.75
  ) +
    geom_path(
      color = "red",
      size = 0.75
    ) +
    facet_grid(
      factor(
        alpha_label,
        levels = rev(
          levels(
            alpha_label
          )
        )
      ) +
        factor(
          beta_label,
          levels = rev(
            levels(
              beta_label
            )
          )
        ) ~
      n_label,
      scales = "free"
    ) +
    ylab(
      "Excess Kurtosis (y-axis scales vary)"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = -90
      )
    )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Bias
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams plot_kurt
#' @export
plot_bias <- function(data,
                      std = FALSE) {
  alphahatbetahat_bias <- alphahatprimebetahatprime_bias <- taudot_label <- NULL
  if (std) {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatprimebetahatprime_bias,
        group = 1
      )
    )
  } else {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatbetahat_bias,
        group = 1
      )
    )
  }
  p + geom_hline(
    yintercept = 0,
    alpha = 0.5
  ) + geom_point(
    color = "red",
    size = 0.75
  ) +
    geom_path(
      color = "red",
      size = 0.75
    ) +
    facet_grid(
      factor(
        alpha_label,
        levels = rev(
          levels(
            alpha_label
          )
        )
      ) +
        factor(
          beta_label,
          levels = rev(
            levels(
              beta_label
            )
          )
        ) ~
      n_label
    ) +
    ylab("Bias") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = -90
      )
    )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Plot Root Mean Square Error
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams plot_kurt
#' @export
plot_rmse <- function(data,
                      std = FALSE) {
  alphahatbetahat_rmse <- alphahatprimebetahatprime_rmse <- taudot_label <- NULL
  if (std) {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatprimebetahatprime_rmse,
        group = 1
      )
    )
  } else {
    p <- ggplot(
      data = data,
      aes(
        x = taudot_label,
        y = alphahatbetahat_rmse,
        group = 1
      )
    )
  }
  p + geom_hline(
    yintercept = 0,
    alpha = 0.5
  ) + geom_point(
    color = "red",
    size = 0.75
  ) +
    geom_path(
      color = "red",
      size = 0.75
    ) +
    facet_grid(
      factor(
        alpha_label,
        levels = rev(
          levels(
            alpha_label
          )
        )
      ) +
        factor(
          beta_label,
          levels = rev(
            levels(
              beta_label
            )
          )
        ) ~
      n_label
    ) +
    ylab("Root Mean Square Error") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = -90
      )
    )
}
