#' Generate a visual profile of returns across metrics like rolling SD, rolling corr, and cumulative returns
#'
#' This function takes a long dataframe and outputs several plotly graphs for standard deviation, correlation, and cumulative return
#' using the slider and plotly libraries. DF columns names be date, symbol, value
#'
#' @param long_df A long dataframe `data.frame`
#' @param window the window for rolling measurements `numeric`
#' @param ret_type the return type to calculate cumulative returns from `character`
#' @import dplyr
#' @import tidyr
#' @import plotly
#' @import slider
#' @export retprofile
#' @author Travis Nowak
#' @examples
#' \dontrun{set.seed(123)
#' dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
#' tickers <- c("TICKER_A", "TICKER_B")
#' data <- data.frame(
#'   date = rep(dates, each = length(tickrs)),
#'   ticker = rep(tickers, times = length(dates)),
#'   return = rnorm(length(dates) * length(tickers), mean = 0, sd = 0.04)
#'   )
#' retprofile(data, window = 30, ret_type = 'log')}


retprofile <- function(long_df, window, ret_type = 'log'){

  # First we use our function to generate a long df to use
  # Then let's create a function which visualizes three mtrics:
  ## 1) Rolling standard deviation
  # Convert to wide
  df_sd <- long_df %>% tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)


  # Generate rolling standard deviation df
  df_sd <- df_sd %>%
    mutate(across(-date, ~slider::slide_dbl(.x, sd, .before = window))
    )

  df_sd <- df_sd %>%
    tidyr::pivot_longer(-date, names_to = "ticker", values_to = "rollingsd")

  sd_plot <- plotly::plot_ly(
    df_sd,
    x = ~date,
    y = ~rollingsd,
    color = ~ticker,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    plotly::layout(
      title = "Standard Deviation of Stock Returns",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Standard Deviation"),
      legend = list(title = list(text = "Ticker"))
    )

  # Correlation
  # First we use our function to generate a long df to use
  # Then let's create a function which visualizes three mtrics:

  # Convert to wide
  df_corr <- long_df %>% tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)


  # Need to convert colnames to something generic
  colnames(df_corr) <- c("date", "ticker1", "ticker2")

  # Generate rolling standard deviation df
  df_corr <- df_corr %>%
    dplyr::mutate(corr = slider::slide2_dbl(.x = df_corr$ticker1, .y = df_corr$ticker2, .f = ~cor(.x, .y), .before = window, .complete = TRUE)) %>% select(-ticker1, -ticker2)

  corr_plot <- plotly::plot_ly(
    df_corr,
    x = ~date,
    y = ~corr,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    plotly::layout(
      title = "Correlation of Stock Returns",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Correlation"))

  if(ret_type == 'log'){
    # For log returns:
    # Convert to wide
    df_cum <- long_df %>% tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)

    # Replace each column with the cumulative returns
    df_cum <- df_cum %>%
      dplyr::mutate(across(2:3, ~ exp(cumsum(.x))-1, .names = "{.col}"))

    df_cum <- df_cum %>%
      tidyr::pivot_longer(-date, names_to = "ticker", values_to = "return")


  } else if(ret_type == 'absolute'){

    # for abs returns:
    # Convert to wide
    df_cum <- long_df %>% tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)


    df_cum <- df_cum %>%
      dplyr::mutate(
        across(2:3, ~ cumsum(.x))
      )

    df_cum <- df_cum %>%
      tidyr::pivot_longer(-date, names_to = "ticker", values_to = "return")

  } else if(ret_type == 'relative'){

    # for abs returns:
    # Convert to wide
    df_cum <- long_df %>% tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)


    df_cum <- df_cum %>%
      dplyr::mutate(
        across(2:3, ~ cumprod(1 + .x) - 1)
      )

    df_cum <- df_cum %>%
      tidyr::pivot_longer(-date, names_to = "ticker", values_to = "return")

  }

  cum_plot <- plotly::plot_ly(
    df_cum,
    x = ~date,
    y = ~return,
    color = ~ticker,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    plotly::layout(
      title = "Cumulative Absolute Returns of Stocks",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Cumulative Return"),
      legend = list(title = list(text = "Ticker"))
    )

  dashboard <- plotly::subplot(
    sd_plot, cum_plot, corr_plot, nrows = 3) %>%
    plotly::layout(title = list(text = "Return Dashboard"),
                   plot_bgcolor='#e5ecf6',
                   xaxis = list(
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
                   yaxis = list(
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'))

  return(dashboard)
}




