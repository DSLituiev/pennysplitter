library(dplyr)
library(glue)
library(magrittr)
library(tidyselect)

#' Calculate Securities Gain / Loss and Hold period using FIFO method
#' @param data_gr_sec data.frame. The first item to paste
#' @param ... symbols. Grouping variables, normally would include Asset / Security / Token column
#' @param quantity character or function. Name of quantity column
#' @param price character or function. Name of price column
#' @param date_col character or function. Name of date / timestamp column
#' @param transaction_type character or function. Name of transaction type column (with values like "buy", "sell", "send")
#' @return tibble with gains / losses
#' @examples
#' data <- read_csv("data/toy-coinbase-transactions.csv")
#' data %>% fifo_sheet(Asset)
#' @export
#' @importFrom base paste

fifo_sheet <- function(data_gr_sec, 
                                 ...,
                                 quantity = find_col_quantity,
                                 price = find_col_price,
                                 date_col = find_col_date,
                                 transaction_type = find_col_transaction,
                                 verbose=T){
 

  expr <- as.list(substitute(list(...)))[-1L]
  class(expr) <- "expression"
  if (length(expr) > 0){
    data_gr_sec <- data_gr_sec %>% ungroup()
    data_gr_sec <- eval(substitute(group_by(data_gr_sec, ...)))
  } else if (data_gr_sec %>% attr("groups") %>% is.null()){
    message("wrong arguments: either a groupped object must be provided:
            data %>% group_by(Ticker) %>% fifo_sheet()
or groups (Ticker/Token/Coin/`Security Name` variable) must be specified as:
            data %>% fifo_sheet(Ticker)
          ")
    throw("wrong arguments")
  }
  group_vars <- data_gr_sec %>% attr("groups") %>% select(-.rows) %>% names()
  assertthat::assert_that(length(group_vars)>0)
  # message(glue::glue("group_vars: {length(group_vars)}\t{dput(group_vars)}"))
  
  if (is.function(quantity))          {quantity     <- quantity(data_gr_sec %>% ungroup()); 
                                        if(length(quantity)!=1) throw("cannot locate column for `quantity`");  message(glue("quantity: {quantity}\n"))}
  if (is.function(price))             {price        <- data_gr_sec %>% ungroup() %>% price();  
                                        if(length(price)!=1) throw("cannot locate column for `price`"); message(glue("price: {price}\n"))}
  if (is.function(date_col))          {date_col     <- data_gr_sec %>% ungroup() %>% date_col(); 
                                        if(length(date_col)!=1) throw("cannot locate column for `date/timestamp`"); message(glue("date_col: {date_col}\n"))}
  if (is.function(transaction_type))  {transaction_type <- data_gr_sec %>% ungroup() %>% transaction_type(); 
                                        if(length(transaction_type)!=1) throw("cannot locate column for `transaction type`"); message(glue("transaction_type: {transaction_type}\n"))}
  
  data_gr_sec %>%
    arrange(!!rlang::sym(date_col)) %>% 
    do(fifo_security_long(., group_vars = group_vars,
                               quantity = quantity,
                               price = price,
                               date_col = date_col,
                               transaction_type=transaction_type,
                               verbose=verbose)) %>% ungroup() %>% 
    relocate(starts_with("Date"), Asset, Cost, Revenue, Profit, hold_days)
}

find_col_price       <- function(data) data %>% select(starts_with(c("Spot", "Price")) & where(is.numeric)) %>% colnames()
find_col_date        <- function(data) data %>% select(starts_with(c("time", "date"))) %>% colnames()
find_col_quantity    <- function(data) data %>% select(starts_with(c("quantity"))) %>% colnames()
find_col_transaction <- function(data) data %>% select(ends_with(c("type"))&starts_with("transaction") ) %>% colnames()


fifo_security_fractional <- function(buy_gr, sell_gr, 
                                          #transaction_type="transaction_type",
                                          security_name="Ticker",
                                          suffix = c("_buy", "_sold")){
  # 
  sell_gr <- sell_gr %>% #select(-!!rlang::sym(transaction_type)) %>% 
    rename_at(dplyr::vars(tidyselect::everything()), #vars(-security_name),
              ~paste0(., suffix[2]))

  total_sold <- sell_gr %>% with(sum(Quantity_sold))
  
  remaining_asset_purchases <- buy_gr %>% #select(-!!rlang::sym(transaction_type)) %>% 
    rename_at(dplyr::vars(tidyselect::everything()),
              #vars(-security_name), 
              ~paste0(., suffix[1])) %>% 
    mutate(Cum_Quantity_buy = cumsum(Quantity_buy))
  
  total_bought <- remaining_asset_purchases %>% with(sum(Quantity_buy))
  
  if (total_sold==0){
    message("no sold; skipping")
    return(tibble())
  }
  if (total_sold>total_bought) warning(glue("sold {total_sold} units > bought {total_bought} units"))
  if ((nrow(remaining_asset_purchases)==0)&&(total_sold>0)){
    warning(glue("{total_sold} units sold but none bought"))
    }
  
  sold_asset_purchases <- tibble()
  # line <- 1
  for (line in 1:nrow(sell_gr)){
    if (length(remaining_asset_purchases)==0){
      message(glue("exhausted all buy transactions while {(nrow(sell_gr)-line)} sell transactions remain unmatched"))
      break
    }
    last_idx <- remaining_asset_purchases  %>% 
      with(which.max(Cum_Quantity_buy>=sell_gr[line,"Quantity_sold",T]))
    
    # if (length(remaining_asset_purchases)==0){
    #   message(print(sell_gr[line,]))
    # }
    if (last_idx > nrow(remaining_asset_purchases)){
      message("mismatch at row {line}" %>% glue())
      print(sell_gr[line,])
      print(remaining_asset_purchases)
    }
    
    profit_per_sale <-
      remaining_asset_purchases %>% head(last_idx) %>%
      cross_join(sell_gr[line,],
                 # by=join_by(!!rlang::sym(security_name))
                 ) %>% 
      mutate(Cum_Quantity_buy_per_sale = pmin(Cum_Quantity_buy, Quantity_sold),
             Quantity_buy_per_sale = diff(c(0, Cum_Quantity_buy_per_sale)),
             Cost = (Quantity_buy_per_sale*Price_buy),
             Revenue = (Quantity_buy_per_sale*Price_sold),
             Profit = Revenue - Cost
             )  %>% 
      mutate(Quantity_buy_remaining = Quantity_buy-Quantity_buy_per_sale)

    sold_asset_purchases_ <- profit_per_sale %>% filter(Revenue>0) %>% 
        mutate(Quantity_buy = Quantity_buy_per_sale) %>% select(-Quantity_buy_per_sale)
    
    sold_asset_purchases <- bind_rows(sold_asset_purchases, sold_asset_purchases_)
    
    remaining_asset_purchases <- 
      bind_rows(profit_per_sale %>% filter(Quantity_buy_remaining>0) %>% 
                  mutate(Quantity_buy=Quantity_buy_remaining) %>% 
                  select(-Quantity_buy_remaining, -Cost, -Revenue, -Profit, #-TransactionID_buy,
                         -ends_with(c("_sold", "_per_sale"))),
        remaining_asset_purchases %>% tail(-(last_idx))
      ) %>% mutate(Cum_Quantity_buy = cumsum(Quantity_buy))
  }
  
  sold_asset_purchases %>% 
    mutate(hold_days = difftime(Date_sold, Date_buy, units = "day") %>% as.integer()) %>% 
    select(-starts_with("Cum"), -Quantity_buy_remaining,-Quantity_sold) %>% 
    rename(Quantity = Quantity_buy)
}

fifo_security_long <- function(data_gr_sec, 
                                    transaction_type = "transaction_type",
                                    quantity = "Quantity",
                                    price = "Price",
                                    date_col = "Date",
                                    group_vars="Ticker",
                                    verbose=T){
  # message(glue("data_gr_sec: {class(data_gr_sec)}"))
  transaction_type_ <- rlang::sym(transaction_type)
  missing <- setdiff(c(price, quantity, group_vars, transaction_type),
         colnames(data_gr_sec))
  
  if (length(missing))  assertthat::assert_that(assertthat::has_name(data_gr_sec, missing))

  if (quantity != "Quantity")  data_gr_sec <- data_gr_sec %>% rename(Quantity = {quantity})
  if (price != "Price")    data_gr_sec <- data_gr_sec %>% rename(Price = {price})
  if (price != "Date")    data_gr_sec <- data_gr_sec %>% rename(Date = {date_col})

  data_gr_sec <- data_gr_sec %>% mutate(Quantity = abs(`Quantity`))
  
  if (verbose) message(data_gr_sec %>% distinct(!!rlang::sym(group_vars)))
  # browser()
  sell_gr <- data_gr_sec %>%
    dplyr::filter(tolower(!!rlang::sym(transaction_type))=="sell") %>% 
    dplyr::select(-!!rlang::sym(transaction_type)) %>% 
    dplyr::select(.data =., -one_of(group_vars)) %>%
    mutate(`Quantity`=abs(`Quantity`))
  
  buy_gr <- data_gr_sec %>% 
    filter(tolower(!!rlang::sym(transaction_type))=="buy") %>% 
    dplyr::select(-!!rlang::sym(transaction_type)) %>% 
    dplyr::select(-one_of(group_vars))
  
  remaining <- data_gr_sec %>% 
    filter(tolower(!!rlang::sym(transaction_type))!="buy",
           tolower(!!rlang::sym(transaction_type))!="sell") %>% 
    group_by({{transaction_type_}}) %>% count()
  
  n_remaining <- sum(remaining$n)
  
  if (n_remaining>0) {
    message(glue("ignoring {n_remaining} rows with transaction types other than 'buy' and 'sell':"))

    remaining %>%
      dplyr::mutate(res = paste0(!!rlang::sym(transaction_type), " -- ", n)) %>%
      mutate(message(res))
      # summarise(res = paste(res, sep = "; ")) %>%
      # message(.[1,1,T])
    # 
    }
  
  fifo_security_fractional(buy_gr, sell_gr,
     # transaction_type=transaction_type
  )
}



