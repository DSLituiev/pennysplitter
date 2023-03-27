#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(pennysplitter)

buy = data.frame(TransactionID = c(1:11),
                 Ticker=c(rep('MSFT',4), rep('AMZN',4), rep('DOCU',3)),
                 Date=c(rep('01-01-2020',2),rep('01-14-2020',2),
                        rep('01-01-2020',1), rep('01-02-2020',1), rep('01-05-2020',1), rep('01-14-2020',1),
                        '01-01-2020','03-15-2020','04-06-2020'),
                 Price=c(100,102,102,107,
                         2000, 2004, 2010,2011,
                         197,182,167),
                 Quantity=c(8,10,5,5, 
                            2,7,5,1,
                            12,15,15)) %>% 
  mutate(Date=as.Date(Date, "%m-%d-%Y",)) %>% as_tibble()

sell = data.frame(TransactionID=100+c(1:8),
                  Ticker=c('MSFT','MSFT',
                           'AMZN','AMZN','AMZN',
                           'DOCU','DOCU','DOCU'),
                  Date=c('01-07-2020','01-20-2020',
                         '01-06-2020','01-13-2020', '01-30-2020',
                         '01-15-2020','04-10-2020','04-20-2020'),
                  Price=c(97,110,
                          2100,2050,2020,
                          210,205,225),
                  Quantity=c(9,12,
                             3,8,4,
                             5,5,3)) %>% 
  mutate(Date=as.Date(Date, "%m-%d-%Y",)) %>% as_tibble()


buy %>% filter(Ticker=="AMZN")
sell %>% filter(Ticker=="AMZN")


#' https://stackoverflow.com/a/67676092/1716733
#' by @AnilGoyal
calculate_fifo_security_discrete <- function(buy, sell){
    
    buy_per_unit <- buy %>% uncount(Quantity) %>%
      group_by(Ticker) %>%
      mutate(Cost = cumsum(Price), 
             Total_Quantity = row_number())
    
    sell_per_unit <- sell %>%
      group_by(Ticker) %>%
      mutate(Total_Quantity = cumsum(Quantity)) %>% 
      ungroup()
    
    right_join(buy_per_unit, sell_per_unit, 
               by = c('Ticker', 'Total_Quantity'),
               suffix=c("_buy", "_sold")) %>%
    mutate(CP = diff(c(0, Cost))) %>%  # `Cost` is coming from the `buy` table
    group_by(TransactionID_sold, Ticker, Total_Quantity) %>%
    summarise(CP = sum(CP),
              SP = sum(Price_sold * Quantity),
              Quantity = Quantity,
              Profit = SP - CP,
              Period = difftime(Date_sold, Date_buy,
                                units="days"),
              .groups="drop") %>%
      arrange(Ticker) %>% relocate(TransactionID_sold, Ticker, Quantity, Total_Quantity)
}

#' Calculation from two tables
calculate_fifo_security_discrete(buy, sell)

#' Calculation from a long table
data <- bind_rows(buy %>% mutate(transaction_type="buy"), 
                  sell%>% mutate(transaction_type="sell")) %>% 
  arrange(Ticker) %>% 
  arrange(Date)


data %>% arrange(Ticker, Date) %>% 
  mutate(Cost = Price*Quantity) %>% 
  mutate(Cost = Cost*if_else(transaction_type=="sell", -1,1),
         Quantity = Quantity*if_else(transaction_type=="sell", -1,1),
         ) %>% 
  mutate(quantity_cumul = cumsum(Quantity),
    cost_cumul = cumsum(Cost) ) %>% 
  filter(Ticker=="AMZN")


calculate_fifo_security_discrete(buy %>% filter(Ticker=="AMZN") ,
                                 sell %>% filter(Ticker=="AMZN") )

calculate_fifo_security_discrete(
    data %>% filter(Ticker=="AMZN") %>% filter(transaction_type=="buy") %>% select(-transaction_type),
    data %>% filter(Ticker=="AMZN") %>%filter(transaction_type=="sell")%>% select(-transaction_type)
                                 )

buy_gr = data %>% filter(Ticker=="AMZN") %>% filter(transaction_type=="buy") %>% select(-transaction_type)
sell_gr = data %>% filter(Ticker=="AMZN") %>%filter(transaction_type=="sell")%>% select(-transaction_type)

fifo_security_fractional(
  data %>% filter(Ticker=="AMZN") %>% filter(transaction_type=="buy") %>% select(-transaction_type),
  data %>% filter(Ticker=="AMZN") %>%filter(transaction_type=="sell")%>% select(-transaction_type),
)

data %>% filter(Ticker=="AMZN") %>%  calculate_fifo_security_long(group_vars="Ticker")
 

data %>% calculate_fifo_sheet(Ticker)

data %>% group_by(Ticker) %>% calculate_fifo_sheet()

data %>% group_by(Ticker) %>% do(calculate_fifo_security_long(., group_vars = "Ticker")) %>% ungroup()


data %>% calculate_fifo_sheet(Ticker) %>% 
  group_by(Ticker) %>% summarise(across(one_of(c("Quantity", "Profit")), sum ))

calculate_fifo_security_discrete(
  data %>% filter(transaction_type=="buy") %>% select(-transaction_type),
  data %>% filter(transaction_type=="sell")%>% select(-transaction_type)
) %>% 
  group_by(Ticker) %>% summarise(across(one_of(c("Quantity", "Profit")), sum ))
