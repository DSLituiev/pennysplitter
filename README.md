# Pennysplitter: The R Package for Crypto Tax Calculation
> Disclaimer: Pennysplitter is not intended to provide tax advice, and is not a substitute for professional tax advice. Always consult a tax professional before making decisions related to your taxes.

As a crypto investor with transactions on multiple exchanges, I experienced firsthand the "crypto fallout" trying to make my crypto taxes work on one of the biggest tax software provider.

If you're a crypto investor looking for a tool to help with tax accounting, Pennysplitter can help. This R package is designed to make it easy to calculate your taxes using the FIFO (first-in, first-out) method, which is commonly used for calculating gains and losses. The package will generate a report that includes your tax gain/loss and holding period, giving you the information you need to file your taxes accurately.

Please note that Pennysplitter is not a substitute for professional tax advice. While it can help simplify the tax accounting process, you should always consult a tax professional before making any decisions related to your taxes. Additionally, the information provided by Pennysplitter should not be relied upon as tax advice or relied upon for accuracy.

# Current functionality

```R
    library(tidyverse)
    library(pennysplitter)

    transactions <- read_csv("coinbase-transactions.csv")
   
    gains <- transactions %>%
      mutate( # make sure to include fees / commission
        actual_price = abs(`Total (inclusive of fees and/or spread)` / `Quantity Transacted`)
      ) %>%
      select(-Notes, -Subtotal, -`Total (inclusive of fees and/or spread)`) %>% 
      fifo_sheet(Asset,
                 transaction_type="Transaction Type",
                 quantity="Quantity Transacted",
                 date="Timestamp",
                 price="actual_price") %>% 
      filter(Date_sold >= "2022-01-01")
     
    names_gain <- c(
      "Currency Name"="Asset",
      "Purchase Date"="Date_buy",
      "Cost Basis" = "Cost",
      "Date Sold" = "Date_sold",
      "Proceeds" = "Revenue",
      "Quantity" ="Quantity"
    )

    cat("total gains/losses by hold period")
    gains %>% 
      filter(Date_sold >= "2022-01-01") %>% 
      mutate(hold_term = if_else(hold_days>=365, "long", "short")) %>% 
      group_by(Asset, hold_term) %>% 
      summarise(Profit=sum(Profit)) %>% 
      group_by(hold_term) %>% summarise(Profit=sum(Profit))


    # Currency Name, Purchase Date, Cost Basis, Date Sold, Proceeds.
    gains %>% rename(!!names_gain) %>% select(as.character(names(names_gain))) %>% 
      mutate(`Purchase Date` = as.Date(`Purchase Date`),
             `Date Sold` = as.Date(`Date Sold`)
             ) %>% 
      mutate(across(where(is.numeric) & -starts_with("Quant") , ~round(.x, 2))) %>%
      filter(round(Proceeds,3)!=0.0) %>% 
      write_csv("gains.csv")
```
