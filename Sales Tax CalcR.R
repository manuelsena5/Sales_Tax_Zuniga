library(tidyverse)
library(lubridate)
library(openxlsx)
library(emayili)



path <- "C:/Users/manue/Desktop/M+C Bookkeeping/Tax Clients/Javier Zuniga/Sales Taxes/"

year <- year(now())

quarter <- quarter(now())-1

#To get the sales report downloaded from PayPal

paypal_report <- read.csv("Download.csv")


#Selecting the Columns Need for Calculation

paypal_report2 <- paypal_report[,1:20]

#To add a Gross column with Gross sales including Shipping amount

paypal_report2$Gross <- as.numeric(paypal_report2$Gross)

#To filter just for the sales and no expenses.

paypal_report2 <-  filter(paypal_report2, Type == "Website Payment" | Type == "PayPal Here Payment")


#To convert NA observations in zeros for shipping and handling and Sales tax Columns
paypal_report2 <- replace_na(paypal_report2, list(Shipping.and.Handling.Amount = 0, Sales.Tax = 0, Gross = 0))


#Calculation

if(paypal_report2$Shipping.and.Handling.Amount > 0){
  (paypal_report2$Shipping.and.Handling.Amount * 0.825) + paypal_report2$Sales.Tax
} else {
  paypal_report2$Sales.Tax
}


paypal_report2$Calculated.tax <- ifelse(paypal_report2$Shipping.and.Handling.Amount > 0, 
       (paypal_report2$Shipping.and.Handling.Amount * 0.0825) + paypal_report2$Sales.Tax,
      paypal_report2$Sales.Tax)


paypal_report2$Taxable.Sales <- ifelse(paypal_report2$Calculated.tax == 0, 0, 
                                      paypal_report2$Gross - paypal_report2$Calculated.tax)


paypal_report2$All.Sales <- ifelse(paypal_report2$Calculated.tax == 0, paypal_report2$Gross, 
                                      paypal_report2$Taxable.Sales)




tax.total <- sum(paypal_report2$Taxable.Sales)
sales.total <- sum(paypal_report2$All.Sales)
Tax <- round(tax.total * 0.0825, 2)

# Taxable and Gross Sales for the State of Texas



tax.total
sales.total
Tax
             


#to write excel file to M+C Bookkeeping documents folder with two worksheets


wbs <- list("Download" = paypal_report, "calculations" = paypal_report2)

write.xlsx(wbs, paste0(path, year,"/Q",quarter, " ", year,".xlsx")) 


#to Send Email to Javier

email <- envelope() %>%
  from("sena@mpluscbookkeeping.com") %>%
  to("rusticthingz@yahoo.com") %>%
  subject("Quarterly Texas Sales Tax") %>%
  text("Javier,
       
       I have completed the franchise tax report due this month with the Texas Comptroller. Whenever you have time please log in and submit the payment.
       This quarter the tax due is $ {{Tax}}. There may be a small discount if paid by the 20th when is due. Please let me know if you have any questions or concerns.
       
       Respectfully,
       
       Manuel Sena")

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = Sys.getenv("GMAIL_USERNAME"),
               password = Sys.getenv("GMAIL_PASSWORD"))


smtp(email, verbose = TRUE)



