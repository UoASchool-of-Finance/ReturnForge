#' Create a tibble or zoo object of returns using tq_get
#'
#' This function takes ticker(s), start date, return type, and output type as inputs and generates either a tibble or zoo object
#' using the tidyquant, tidy, and zoo libraries.
#'
#' @param tickers A list of yahoo finance tickers. `list`
#' @param start_date the first observation date. `character`
#' @param ret_type log (default), 'rel', or 'abs'. `character`
#' @param output 'tibble' (default), 'zoo'. `character`
#' @param long TRUE (default) to return a long tibble FALSE for wide. Zoo objects are wide. `character`
#' @return Long `tibble` (default), Wide `tibble`, `Zoo` object.
#' @export genrets
#' @author Mitch Greer
#' @examples
#' tickers = c('SPY', 'QQQ')
#' rets_long <- genrets(tickers, '2020-01-01', ret_type = 'log', output = 'tibble', long = TRUE)
#'

genrets <- function(tickers, start_date, ret_type = 'log', output = 'tibble', long = TRUE){

  prices <- tidyquant::tq_get(
    x = tickers,
    get = 'stock.prices',
    from = as.Date(start_date) - 1
  ) %>%
    dplyr::select(date, symbol, adjusted) %>%
    dplyr::group_by(symbol)

  if(ret_type == 'log'){

    rets <- prices %>%
      dplyr::mutate(
        return = log(adjusted / dplyr::lag(adjusted))
      ) %>%
      tidyr::drop_na()%>%
      dplyr::select(-adjusted)

  }else if(ret_type == 'rel'){

    rets <- prices %>%
      dplyr::mutate(
        return = (adjusted / dplyr::lag(adjusted)) - 1
      ) %>%
      tidyr::drop_na() %>%
      dplyr::select(-adjusted)

  }else if(ret_type == 'abs'){

    rets <- prices %>%
      dplyr::mutate(
        return = adjusted - dplyr::lag(adjusted)
      ) %>%
      tidyr::drop_na() %>%
      dplyr::select(-adjusted)

  }

  if(output == 'tibble'){

    if(long == TRUE){

      return(rets)

    }else if(long == FALSE){

      tmp <-
        rets %>%
        tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)

      return(tmp)
    }

  }else if(output == zoo){

    tmp <- rets %>%
      tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = return)

    tmp <- zoo::zoo(tmp[,-1], order.by = tmp$date)

    return(tmp)
  }


}
