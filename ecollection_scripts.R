# Load Package and Libraries

# Library for data manipulation and Visualization
library(tidyverse)
library(gmodels)
library(forcats)
library(purrr)
library(lubridate)
library(tidyquant)

# Library for reading and loading data
library(readr)
library(readxl)
library(writexl)
library(stringr)
library(stringi)
library(fs)

# Library for Machine Learning Packages
library(parsnip)
library(class)
library(rpart)
library(rpart.plot)
library(ggrepel)
library(broom)
library(umap)
library(rsample)
library(yardstick)
library(gmodels)

#Create Data Repository Folder ----
dir_create("00_Data")

#Load Data ----
ecol_pymt_tbl <- read_excel("00_Data/ecollection_payments.xlsx", sheet = 1)
write_rds(ecol_pymt_tbl, path = "00_Data/ecol_pymt_tbl.rds")
write_rds(ecol_class, path = "00_Data/ecol_class.rds")


#Select the relevant Columns ----

ecol_pay_tbl <- ecol_pymt_tbl %>%
    select(-c(eclctn_pmt_ref_num, 
              account_id, 
              pmtmthd_trn_id, 
              pmtmthd_bnk_trn_num, 
              agency_id, account_id,
              pmt_ref_info, dlvry_req_id, 
              dlvry_error_code, dlvry_retry_count, 
              calc_fees, calc_vat_amt, vat_number, last_batch_id, 
              last_job_run_id, job_id, migrationdate, 
              bill_cycle, cust_id, cust_id_type, recon_req_id, abc))

# Remove Colums not needed
ecol_pay_tbl <- ecol_pay_tbl %>% 
                select(-c(recon_date_hijri, ismigrated, 
                          as_of_date, bill_number))


#Renaming and Transforming Columns ----

ecol_pay_tbl <- ecol_pay_tbl %>% 
    # Changing the column names to Pascal Case
  
    set_names(. %>% str_to_title()) %>% 
    
    # Changing Column Data Type
  
    mutate(Amount = as.numeric(Cur_amt)) %>% 
    select(-Cur_amt) %>% 
    mutate(Partner_code = as_factor(Partner_code)) %>% 
    mutate(Pmtmthd_bnk_id = as_factor(Pmtmthd_bnk_id)) %>% 
    mutate(Pmtmthd_pmt_channel = as_factor(Pmtmthd_pmt_channel)) %>% 
    mutate(Pmt_dt = ymd_hms(Pmt_dt) %>% as.Date()) %>% 
    mutate(Source_Channel = as_factor(Source_channel)) %>% 
    mutate(Pmt_method = as_factor(Pmt_method)) %>% 
    mutate(Dlvry_status = as_factor(Dlvry_status)) %>% 
    mutate(Last_update_date = ymd_hms(Last_update_date)) %>% 
    mutate(State = as_factor(State)) %>% 
    mutate(Creation_date = ymd_hms(Creation_date)) %>% 
    mutate(Send_flag = as_factor(Send_flag)) %>% 
    mutate(Send_dlvry_state = as_factor(Send_dlvry_state))


# Encoding the right Payment Channel Description

ecol_pay_tbl <- ecol_pay_tbl %>% 
    mutate(Pmtmthd_pmt_channel = case_when(
        Pmtmthd_pmt_channel == "BTELLER" ~ "Bank Teller",
        Pmtmthd_pmt_channel == "IVR" ~ "Interactive Voice Response",
        Pmtmthd_pmt_channel == "CAM" ~ "Common Area Maintenance",
        Pmtmthd_pmt_channel == "CORP" ~ "Corporate Payment",
        Pmtmthd_pmt_channel == "PDA" ~ "Payment Distribution Agencies",
        Pmtmthd_pmt_channel == "POS" ~ "Point of Sales",
        TRUE ~ as.character(Pmtmthd_pmt_channel)
    ))


# Changing the Payment Channel column to a Factor (Categorical) Data Type

ecol_pay_tbl <- ecol_pay_tbl %>% 
    mutate(Pmtmthd_pmt_channel = as_factor(Pmtmthd_pmt_channel))


# Encoding the full Government Agency Description 

ecol_pay_tbl <- ecol_pay_tbl %>% 
    mutate(Partner_code = case_when(
        Partner_code == "DMMR" ~ "Ministry of Mineral Resources",
        Partner_code == "GEPP" ~ "eProcurement Portal",
        Partner_code == "GPM" ~ "Gouf Province Municipal",
        Partner_code == "MOE" ~ "Ministry of Education",
        Partner_code == "MOF" ~ "Ministry of Finance",
        Partner_code == "NBPM" ~ "Northern Boarder Municipal",
        Partner_code == "PRC" ~ "Premium Residency Center",
        Partner_code == "PTA" ~ "Public Transport Agency",
        Partner_code == "REGA" ~ "Real Estate Government Agency",
        Partner_code == "SAIP" ~ "Saudi Intellectual Property",
        Partner_code == "SEDA" ~ "Saudi Export Development Agency",
        Partner_code == "SIO" ~ "Saudi Irrigation Organization"
    ))


# Creating the Duration Column ----

ecol_pay_tbl <- ecol_pay_tbl %>% 
  select(-Main_agency_id, -Rvsrl_dt) %>% 
  mutate(Send_flag = case_when(
    Send_flag == "Y" ~ "Yes",
    Send_flag == "N" ~ "No"
  )) %>%
  # Removing some date columns we don't need
  
  select(-Recon_dt, -Settl_dt, Dlvry_ts, -Send_dlvry_ts) %>% 
  
  # Creating a duration column from the creation date and last updated date for payment
  
  mutate(duration = round(interval(Creation_date, Last_update_date)/ddays(1), digits = 2)) %>%
  mutate(duration_minutes = round(interval(Creation_date, Last_update_date)/dminutes(1), digits = 2))
  


# Data Visualization of Payment Channel Amount with the Violin Plot ----

ecol_pay_tbl %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove missing data NA's
    # Plot the violin plot
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_jitter(width = 0.1, alpha = 0.1) +
    geom_violin(width = 0.8, alpha = 0.5) +
    coord_flip() +
    #scale_y_continuous(labels = scales::comma) +
    scale_y_log10(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    facet_wrap(~Dlvry_status) +
    labs(x = "Payment Channel",
          y = "Payment Amount",
          title = "POS, ATM, Internet and IVR are the major channels for payments less than 1 Halal")


# Data Visualization of None Delivered Payments
ecol_pay_tbl %>%
    filter(Dlvry_status == "Not Delivered") %>% # Filter down to None Delivered Payments
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove missing data NA's
    group_by(Pmtmthd_pmt_channel, Partner_code) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ungroup() %>% 
    # Ploting the bar chart
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_col() +
    theme_tq() +
    scale_fill_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Payment Channel",
         y = "Payment Amount",
         title = "None Delivered Payments Across Channels"
         )
 
# Ploting the Non Delivered payment by the Enttities
ecol_pay_tbl %>%
    filter(Dlvry_status == "Not Delivered") %>% # Filter down to none delivered payments
    filter(!is.na(Pmtmthd_pmt_channel)) %>%  # Remove NA's from channels 
    group_by(Pmtmthd_pmt_channel, Partner_code) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ungroup() %>% 
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_col() +
    # Disecting and Faceting by Entities
    facet_wrap(~Partner_code, ncol = 3) +
    theme_tq() +
    scale_fill_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Payment Channel",
         y = "Payment Amount",
         title = "None Delivered Payments Across Entities")

# Count of Non Delivered Payment By Entities
ecol_pay_tbl %>%
    filter(Dlvry_status == "Not Delivered") %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% 
    count(Pmtmthd_pmt_channel, Partner_code) %>% 
    ggplot(aes(Pmtmthd_pmt_channel, n)) +
    geom_col() +
    theme_tq() +
    scale_fill_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Payment Channel",
         y = "Payment Amount",
         title = "Number of None Delivered Payments Across Channels")

# 10 Years Entity Performance by Payment
ecol_pay_tbl %>% 
    ggplot(aes(Partner_code, Amount)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Entity",
         y = "Payment Amount",
         title = "Ministry of Finance Leads")

# Analyzing Entity Performance 2016, 2017, 2018 and 2019
ecol_pay_tbl %>% 
    mutate(Year = year(Pmt_dt)) %>% # Create a Year Column from the Pmt_dt column
    filter(Year %in% c("2016","2017","2018", "2019")) %>% # Filter the Year to 2016 - 2019
    group_by(Partner_code, Year) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ggplot(aes(Partner_code, Amount)) +
    geom_col() +
    # Faceting by the Year Column
    facet_wrap(~Year) +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Entity",
         y = "Payment Amount",
         title = "MOF Declined in 2018 and MOE Increased in 2019")

# Analyzing the 2016 - 2019 Entity Performance by Volume of Payment
ecol_pay_tbl %>% 
    mutate(Year = year(Pmt_dt)) %>% 
    filter(Year %in% c("2016","2017","2018", "2019")) %>%
    count(Partner_code, Year) %>% 
    ggplot(aes(Partner_code, n)) +
    geom_col() +
    facet_wrap(~Year) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Entity",
         y = "Volume of Payments",
         title = "Gouf Province Municipal have the highest number of ePayments")


# Hypothesis Test to know if the difference between Ministry of Mineral Resources and MOF
# is Statistically significant or it happened by chance. calcuating the P-Value.

# Creating a dataframe for only Ministry of Mineral Resources and MOF
p_val_col <- ecol_pay_tbl %>% 
    filter(Partner_code %in% c("Ministry of Mineral Resources","Ministry of Finance"))

# Creating a Dataframe for only Ministry of Mineral Resources
moe_mean <- p_val_col %>% 
    filter(Partner_code == "Ministry of Mineral Resources")

# Calculating the mean payment for the Ministry of Mineral Resources
mean(moe_mean$Amount, na.rm = TRUE)

# Craeting a Dataframe for the Ministry of Finance
mof_mean <- p_val_col %>% 
    filter(Partner_code == "Ministry of Finance")

# Calculating the Mean Payment for MOF
mean(mof_mean$Amount, na.rm = TRUE)

# Calculating the difference in Mean between Ministry of Mineral Resources and MOF
57208.34 - 13528.72

# A T-Test Calculation for the P-Value between Difference in Mean for Ministry of Mienral Resources
# and MOF
t.test(Amount ~ Partner_code, p_val_col, mu = 43679.62)


detect_outliers <- function(x) {
  
  if (missing(x)) stop("The argument x needs a vector.")
  
  if (!is.numeric(x)) stop("The argument x must be numeric.")
  
  data_tbl <- tibble(data = x)
  
  limits_tbl <- data_tbl %>%
    summarise(
      quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE),
      quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE),
      iqr         = IQR(data, na.rm = TRUE),
      limit_lo    = quantile_lo - 1.5 * iqr,
      limit_hi    = quantile_hi + 1.5 * iqr
    )
  
  output_tbl <- data_tbl %>%
    mutate(outlier = case_when(
      data < limits_tbl$limit_lo ~ TRUE,
      data > limits_tbl$limit_hi ~ TRUE,
      TRUE ~ FALSE
    ))
  
  return(output_tbl$outlier)
  
}

ecol_outlier_tbl <- ecol_pay_tbl %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  group_by(Pmtmthd_pmt_channel) %>% 
  mutate(outlier = detect_outliers(Amount)) %>% 
  ungroup()


# Visualizing Outliers with Box Plot of Payment and Channels
ecol_pay_tbl %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% 
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_boxplot() +
    coord_flip() +
    # ggrepel::geom_label_repel(aes(label = Partner_code), 
    #                         color = "red", 
    #                         size = 3,
    #                         data = . %>%
    #                           filter(outlier)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Payment Amount",
         y = "Payment Channels",
         title = "BTELLER, CORP, INTERNET and ATM Contains Outliers")


# Creating a dataframe for Payment above SAR 500K which are outliers from the previous boxplot
above_500k <- ecol_pay_tbl %>% 
    filter(Amount > 500000)


# Box Plot of Payment Channel Performance above SAR 500K
above_500k %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    # Plot the Box Plot
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_boxplot() +
    # Flip the X and Y axis of the Plot
    coord_flip() +
    scale_y_continuous(labels = scales::comma)

# Plot the Violin Plot to see distribution of Payments above SAR 500K across channels
above_500k %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_jitter(width = 0.1) +
    geom_violin(width = 0.8, alpha = 0.5) +
    coord_flip() +
    scale_y_log10(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    labs(y = "Payment Amount",
         x = "Payment Channel",
         title = "Payment Amount above SAR 500K Across Channels")
    
# Violin Plot of Payments above SAR 500K for 2018 and 2019 across Channels
above_500k %>% 
    mutate(Year = year(Pmt_dt)) %>% # Creat a Year Column form the Pmt_dt column
    filter(Year == 2018 | Year == 2019) %>% # Filter for only 2018 and 2019 Year
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    # Visualization
    ggplot(aes(Pmtmthd_pmt_channel, Amount)) +
    geom_jitter(width = 0.1) +
    geom_violin(width = 0.8, alpha = 0.5) +
    coord_flip() +
    scale_y_log10(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    facet_wrap(~Year) +
    labs(x = "Payment Channels",
         y = "Payment Amount",
         title = "Payment Amount Above SAR 500K Across Channel")


# Creating a Yearly trend analysis of Payments across channel
ecol_pay_tbl %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # remove NA's
    mutate(pymt_date = floor_date(Pmt_dt, "year") %>% ymd()) %>% # Create a yearly trend
    group_by(pymt_date, Pmtmthd_pmt_channel) %>% # Group by Yearly Trend and Channel
    summarise(Amount = sum(Amount)) %>% 
    
    ungroup() %>% 
    # Ploting the trend with a line plot and scatter plot
    ggplot(aes(pymt_date, Amount)) +
    geom_point() +
    geom_line(size = 0.5, linetype = 1) +
    geom_smooth(span = 0.2, method = "loess") +
    facet_wrap(~Pmtmthd_pmt_channel, ncol = 3) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    labs(x = "Year",
         y = "Payment Amount",
         title = "Payment Trends Across Channels")
    

# Trend Analysis by Delivery Status
ecol_pay_tbl %>% 
    mutate(pymt_date = floor_date(Pmt_dt, "year") %>% ymd()) %>% # Creating the Yearly Trend
    group_by(pymt_date, Dlvry_status) %>% # Group by Yearly Trend and Delivery Status
    summarise(Amount = sum(Amount)) %>% 
    
    ungroup() %>% 
    
    ggplot(aes(pymt_date, Amount)) +
    geom_point() +
    geom_line(size = 0.5, linetype = 1) +
    geom_smooth(span = 0.2, method = "loess") +
    facet_wrap(~Dlvry_status) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    labs(x = "Year",
         y = "Payment Amount",
         title = "Upward Trends in Delivered Payment")


# Trend Analysis by State (Paid, Recon, UnRecon)
ecol_pay_tbl %>% 
    mutate(pymt_date = floor_date(Pmt_dt, "year") %>% ymd()) %>% 
    group_by(pymt_date, State) %>% 
    summarise(Amount = sum(Amount)) %>% 
    
    ungroup() %>% 
    
    ggplot(aes(pymt_date, Amount)) +
    geom_point() +
    geom_line(size = 0.5, linetype = 1) +
    geom_smooth(span = 0.2, method = "loess") +
    facet_wrap(~State, ncol = 3) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq() +
    labs(x = "Year",
         y = "Payment Amount",
         title = "Payment Trends Across Delivery State")


# Creating a Heatmap of Channels against Proportion of Count
pct_ecol_tbl <- ecol_pay_tbl %>% # Creating the Proportion Dataframe
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    count(Pmtmthd_pmt_channel, State) %>% # Create a Count of Channel and State
    group_by(Pmtmthd_pmt_channel, State, n) %>% # n is the count of both Channel and State
    summarise(quant = sum(n)) %>% # create a column "quant" to sum the counts (n)
    
    ungroup() %>% 
    
    group_by(Pmtmthd_pmt_channel) %>% # group by channel column to get proportion
    mutate(pct = quant/sum(quant)) %>% # create a column pct to calculate the proportion of count
    
    ungroup() %>% 
    
    mutate(Paymt_channel = as.factor(Pmtmthd_pmt_channel) %>% fct_rev()) %>% # convert to categorical column
    mutate(cust_num = as.numeric(Paymt_channel)) # Create a unique numeric column as an identifier


# Heatmap of Channels against State (Paid, Recon, UnRecon)
 pct_ecol_tbl %>% 
    # Creating the Visualization Heatmap
     ggplot(aes(Paymt_channel, State)) +
     geom_tile(aes(fill = pct)) +
     geom_text(aes(label =scales::percent(pct)), size = 3) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "none") +
     scale_fill_gradient(low = "white", high = palette_light()[1])
    

# Creating the Heatmap to calculate the monthly trend against channels
mth_ecol_tbl <- ecol_pay_tbl %>% # Creating the monthly trend dataframe
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    mutate(Month = month(Pmt_dt, label = TRUE, abbr = TRUE)) %>% # Create the month column
    count(Pmtmthd_pmt_channel, Month) %>% # Create a count of channel and month
    group_by(Pmtmthd_pmt_channel, Month, n) %>% 
    summarise(quant = sum(n)) %>% # sum the count column (n)
    
    ungroup() %>% 
    
    group_by(Pmtmthd_pmt_channel) %>% 
    mutate(pct = quant/sum(quant)) %>% # create the proportion column
    
    ungroup() %>% 
    
    mutate(Paymt_channel = as.factor(Pmtmthd_pmt_channel) %>% fct_rev()) %>% 
    mutate(cust_num = as.numeric(Paymt_channel)) # Creat a unique id 


Yr_tbl <- ecol_pay_tbl %>% # Create a yearly trend dataframe
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    mutate(Year = year(Pmt_dt)) %>% # Create the Year Column
    mutate(Month = month(Pmt_dt, label = TRUE, abbr = TRUE)) %>% # Create Month Column
    count(Pmtmthd_pmt_channel, Year, Month) %>% # Count of Channel, Year and Month combination
    group_by(Pmtmthd_pmt_channel, Year, Month, n) %>% 
    summarise(quant = sum(n)) %>% # Sum the Count Column
    ungroup() %>% 
    group_by(Pmtmthd_pmt_channel) %>% 
    mutate(pct = quant/sum(quant)) %>% # Calculate the Proportion of count
    ungroup() %>% 
    mutate(Paymt_channel = as.factor(Pmtmthd_pmt_channel) %>% fct_rev()) %>% 
    mutate(cust_num = as.numeric(Paymt_channel))


# Plot the Heatmap of 2018 and 2019 to see the growth trend for channels payment across months
Yr_tbl %>% 
    filter(Year == 2018 | Year == 2019) %>% # Filter to 2018 and 2019
    ggplot(aes(Paymt_channel, Month)) +
    geom_tile(aes(fill = pct)) +
    geom_text(aes(label =scales::percent(pct)), size = 3) + # write the proportion in the plot
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_fill_gradient(low = "white", high = palette_light()[1]) +
    facet_wrap(~Year)


# Bar Chart of Channel Performance Performance for a 10 Years Period
ecol_pay_tbl %>% 
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    mutate(Year = year(Pmt_dt)) %>% # Creatte a Year Column
    group_by(Pmtmthd_pmt_channel, Year) %>% 
    summarise(Amount = sum(Amount)) %>% # Sum Payment Amount
    mutate(Pmt_channel = fct_reorder(Pmtmthd_pmt_channel, Amount)) %>% 
    arrange(desc(Amount)) %>% # Rearrange Amount By Descending Order
    ggplot(aes(Pmt_channel, Amount)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Make X-axis a angle of 45
    labs(x = "Channel",
         y = "Payment Amount",
         title = "Channels Payment Performance")


# Heatmap of Entity Trend Dataframe
partner_ecol_tbl <- ecol_pay_tbl %>% # Create a Dataframe of Partner trend
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    count(Pmtmthd_pmt_channel, Partner_code) %>% # Create count of Channel and Entity
    group_by(Pmtmthd_pmt_channel, Partner_code, n) %>% 
    summarise(quant = sum(n)) %>% # Sum the count column (n)
    ungroup() %>% 
    group_by(Pmtmthd_pmt_channel) %>% 
    mutate(pct = quant/sum(quant)) %>% # Percent Proportion of Entity Count
    ungroup() %>% 
    mutate(Paymt_channel = as.factor(Pmtmthd_pmt_channel) %>% fct_rev()) %>% 
    mutate(cust_num = as.numeric(Paymt_channel))



partner_ecol_tbl %>% 
    ggplot(aes(Paymt_channel, Partner_code)) +
    geom_tile(aes(fill = pct)) +
    geom_text(aes(label = scales::percent(pct)), size = 3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_fill_gradient(low = "white", high = palette_light()[1]) +
    labs(x = "Payment Channel",
         y = "Entity")


ecol_pay_tbl %>% 
    mutate(Pyt_month = month(Pmt_dt, label = TRUE, abbr = TRUE)) %>% 
    group_by(Pyt_month, State) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ggplot(aes(Pyt_month, Amount)) +
    geom_col() +
    facet_wrap(~State) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Month",
         y = "Payment Amount")


# Creating a Monthly Trend Plot for 2018 ans 2019 ----

Yr_col_tbl <- ecol_pay_tbl %>% # Create the Year Dateframe
    mutate(Year = year(Pmt_dt)) %>% # Create the Year Column from Pmt_dt
    group_by(Year) %>% 
    summarise(Amount = sum(Amount)) # Sum the Payment Amount for each Year


# month table for 2019
mth_col_tbl <- ecol_pay_tbl %>% # Create the Month Dataframe
    mutate(Month = month(Pmt_dt, label = TRUE, abbr = FALSE)) %>% # Create the Month Column form Pmt_dt
    mutate(Year = year(Pmt_dt)) %>% # Create the Year Column
    filter(Year == 2019) %>% # Filter Only the 2019 Year
    group_by(Month) %>% 
    summarise(Amount = sum(Amount)) # Sum the amount for each month in 2019


# month table for 2018
mth_col_2018_tbl <- ecol_pay_tbl %>% # Create the 2018 Month Table
    mutate(Month = month(Pmt_dt, label = TRUE, abbr = FALSE)) %>% # Create the Month Column
    mutate(Year = year(Pmt_dt)) %>% # Create the Year Column
    filter(Year == 2018) %>% # Filter just the 2018 Column
    group_by(Month) %>% 
    summarise(Amount = sum(Amount)) # Sum each month amount in 2018


# Yearly Performance Trend for Payment Amount
Yr_col_tbl %>% 
    mutate(Amount_lag = lag(Amount, n = 1)) %>% # Create a Column and Calculate the amount difference for each year progression
    mutate(Amount_lag = case_when( # Remove the NA value
        is.na(Amount_lag) ~ Amount,
        TRUE ~ Amount_lag
    )) %>% 
    
    mutate(diff_1 = Amount - Amount_lag) %>% # Create a column and calculate the difference for each previous previous
    mutate(pct_diff = diff_1 / Amount_lag) %>% # Calculate the proportion of each year amount against the total amount
    mutate(pct_diff_char = scales::percent(pct_diff, accuracy = 1)) %>% # scale the amount to 1 decimal place
    select(Year, Amount, pct_diff_char) %>% # Select only the Year, Amount and percentage from the table
    rename(Percent = pct_diff_char) # Rename the proportion column to Percent
    

# Create a function that can do the same step as above.
calculate_pct_diff <- function(data) {
    data %>% 
        mutate(Amount_lag = lag(Amount, n = 1)) %>% 
        mutate(Amount_lag = case_when(
            is.na(Amount_lag) ~ Amount,
            TRUE ~ Amount_lag
        )) %>% 
        
        mutate(diff_1 = Amount - Amount_lag) %>% 
        mutate(pct_diff = diff_1 / Amount_lag) %>% 
        mutate(pct_diff_char = scales::percent(pct_diff, accuracy = 1)) %>% 
        select(Month, Amount, pct_diff_char) %>% 
        rename(Percent = pct_diff_char)
    
}

# Create the Year, Amount and Percent Table for 2019 using the function above
m_2019 <- mth_col_tbl %>% 
    calculate_pct_diff()

# Create the Year, Amount and Percent Table for 2018 using the function above
m_2018 <- mth_col_2018_tbl %>% 
    calculate_pct_diff()



trend_data <- m_2019 %>% # Create the trend table combiining 2018 and 2019 table
    bind_cols(m_2018 %>% filter(!Month %in% c("September", "October", "November", "December"))) %>% # Join the 2018 table and remove the last 4 months as 2019 does not contain those in order to create a join.
    rename(Pct_2019 = Percent) %>% # Rename Percent column for 2019
    rename(Pct_2018 = Percent1) %>% # Rename Percent Column for 2018
    select(-Month1) %>% # Remove the Month Column
    rename(Amount_2019 = Amount) %>% # Rename Amount column for 2019
    rename(Amount_2018 = Amount1) # Rename Amount column for 2018


# Plot the Trend for 2018 and 2019 using the scatter and line plot
trend_data %>% 
    select(Month, Amount_2018, Amount_2019, everything()) %>% # Select Columns
    gather(key = "Year_Amount", value = "Amount", -Pct_2019, -Pct_2018, -Month) %>% # Pivot Columns
    group_by(Month, Year_Amount) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ungroup() %>% 
    # Plot the graph with line and scatter plot
    ggplot(aes(x = Month, y = Amount, group = Year_Amount)) +
    geom_point() +
    geom_line(aes(color = Year_Amount)) +
    geom_smooth(span = 0.2, method = "loess") +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    theme_tq() +
    scale_fill_tq()


# Using Kmeans Algorithm to Cluster Partner and Channels Preferences

partner_trend <- ecol_pay_tbl %>% # Creating the trend dataframe
    filter(!is.na(Pmtmthd_pmt_channel)) %>% # Remove NA's
    select(Partner_code, # Select Features to Cluster on (Entity, Channel, Bank, Source, State and Amount)
           Pmtmthd_pmt_channel,
           Pmtmthd_bnk_id,
           Source_Channel,
           State, 
           Amount) %>% 
  
    group_by(Partner_code, # Group By the Selected Column except Amount as you need to group by character variables
             Pmtmthd_pmt_channel,
             Pmtmthd_bnk_id,
             Source_Channel,
             State) %>% 
    summarise(quant = sum(Amount)) %>% # Summing the amount of each combination of group
    ungroup() %>% 
    
    group_by(Partner_code) %>% # Group by Entity to get the proportion for each entity against the combined character grouped variables
    mutate(pay = quant/sum(quant)) %>% # Create a Proportion column called "Pay"
    ungroup()


ptn_tredn <- partner_trend %>% # Creating a dataframe for channels and entity
    select(Partner_code, Pmtmthd_pmt_channel, pay) %>% # select the entity, channel and proportion columns
    mutate(group_id = row_number()) %>% # Create unique ID for each observation
    spread(key = Pmtmthd_pmt_channel, value = pay, fill = 0) %>% # create a PCA of the channel column with proportion as values
    select(-group_id) # remove the id columns as we don't need it anymore


# kmeans_obj <- ptn_tredn %>% 
#     select(-Partner_code) %>% 
#     kmeans(centers = 3, nstart = 100)
# 
# broom::tidy(kmeans_obj) %>% glimpse()
# 
# broom::glance(kmeans_obj)
# 
# broom::augment(kmeans_obj, ptn_tredn) %>% 
#     select(Partner_code, .cluster) %>% View()


# Create a function to make te center selection dynamnic for the kmeans clustering

# initialize the center as 3
center <- 3

# Creating the function
kmeans_mapper <- function(centers = 3){
    ptn_tredn %>% 
        select(-Partner_code) %>% 
        kmeans(centers = centers, nstart = 100)
}

# Using the function to test which center (1 to 15) to cluster on.
kmeans_mapped_tbl <- tibble(centers = 1:15) %>% # creating a number sequence 1 to 15
    mutate(k_means = centers %>% map(kmeans_mapper)) %>% # passing the center 1 to 15 to the function
    mutate(glance = k_means %>% map(glance)) # pulling the center values


# Create a Scree Plot to visualize which center to choose. Center with the largest drop is the appropriate center to choose
# From the scree plt center 3 is the sharpest frop therefore our center will be 3
kmeans_mapped_tbl %>% 
    unnest(glance) %>% # expanding the glance column to see the values of the center (tot.witinss)
    select(centers, tot.withinss) %>% # select the center and the tot.withinss columns
  # create the Scree Plot to visualize
    ggplot(aes(centers, tot.withinss)) +
    geom_point(size = 4) + # make the size of the points in the plot bigger
    geom_line(size = 1) + # make the line thicker
    ggrepel::geom_label_repel(aes(label = centers)) # add the center label to the plot


# Creating a UMAP for the PCA
umap_object <- ptn_tredn %>% 
    select(-Partner_code) %>% # remove the entity column
    umap() # pass the code to the UMAP function



umap_result_tbl <- umap_object$layout %>% # select the UMAP object layout as it holds the decomposed values
    as_tibble() %>% 
    set_names(c("X","Y")) %>% # Set column names to X and Y
    bind_cols(ptn_tredn %>% select(Partner_code)) # Reinstate the Entity Column


# Visualize the with a scatter plot the 3 distinct clusters in the plot
set.seed(1234)
umap_result_tbl %>% 
    ggplot(aes(X, Y)) +
    geom_point()

# Using the kmeans object to select the center 3
kmeans_3_obj <- kmeans_mapped_tbl %>% 
    pull(k_means) %>% 
    pluck(3) # select only the center 3

# merging the kmeans cluster center 3 table with the Entity
kmeans_3_cluster_tbl <- kmeans_3_obj %>% 
    augment(ptn_tredn) %>% 
    select(Partner_code, .cluster)

# Merging the UMPA table and the clustering 3 table
umap_kmeans_3_result_tbl <- umap_result_tbl %>% 
    left_join(kmeans_3_cluster_tbl)


# Creating the clustering table
umap_kmeans_3_result_tbl %>% # using the merged umap and clustering table
    distinct(Partner_code, .keep_all = TRUE) %>% # select the distinct entity
    mutate(label_text = str_glue("Entity: {Partner_code} 
                                 Cluster: {.cluster}")) %>% # create label_text to display entity and the assigned cluster
    ggplot(aes(X, Y, color = .cluster)) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "eCollection Payment Entity Segmentation",
        subtitle = "UMAP Kmeans Clustering"
    ) +
  theme(legend.position = "none")



# dividing the payment amount into Low, Medium and High
 partner_trend %>% 
   pull(quant) %>% # pull the amount column
   quantile(probs = c(0, 0.33, 0.66, 1)) # select a quantile to divide the amount into Low, Medium and High


# Divied the payment amount as amount less than 16081.24 is low, amount less than 731870.91 is Medium and amount above is High
entity_trend_tbl <- partner_trend %>% 
  left_join(umap_kmeans_3_result_tbl) %>% 
  mutate(Payment_bin = case_when(
    quant <= 16081.24 ~ "Low",
    quant <= 731870.91 ~ "Medium",
    TRUE ~ "High"
  )) %>% 
  # select the columns for cluster segmentation
  select(.cluster, 
         Pmtmthd_pmt_channel, 
         Payment_bin,
         Pmtmthd_bnk_id,
         Source_Channel,
         State, pay,
         quant) %>% 
  group_by_at(.vars = vars(.cluster:pay)) %>% 
  summarise(total_payment = sum(quant)) %>% 
  ungroup()


# Cluster 1 (Internet Channel, Low Payment, Paid, BLN/G2G)
entity_trend_tbl %>% 
  filter(.cluster == 1) %>% # select only cluster 1
  arrange(desc(pay)) %>% 
  mutate(cum_prop = cumsum(pay)) %>% 
  View()


# Cluster 2 (Low Payment, BTELLER/Internet, BLN, Paid/Recon)
entity_trend_tbl %>% 
  filter(.cluster == 2) %>% # select only cluster 2
  arrange(desc(pay)) %>% 
  mutate(cum_prop = cumsum(pay)) %>% 
  View()

# Cluster 3 (INTERNET/COROP/BTELLER, High/Medium/Low, Mostly G2G, RECON)
entity_trend_tbl %>% 
  filter(.cluster == 3) %>% # select only cluster 3
  arrange(desc(pay)) %>% 
  mutate(cum_prop = cumsum(pay)) %>% 
  View()


write_rds(umap_kmeans_3_result_tbl, path = "00_Data/umap_kmeans_3_result_tbl.rds") # save the umap_kmean table

# Update Visualization with the cluster assignment labels
cluster_label_tbl <- tibble(
  .cluster = 1:3,
  .cluster_label = c(
    "Internet Channel, Low Payment, Paid, BLN/G2G", # Cluster 1 assignment
    "Low Payment, BTELLER/Internet, BLN, Paid/Recon", # Cluster 2 assignment
    "INTERNET/COROP/BTELLER, High/Medium/Low, Mostly G2G, RECON" # Cluster 3 assignment
  )
) %>% 
  mutate(.cluster = as_factor(as.character(.cluster)))


# Creating the final kmeams cluster assignment plot
umap_kmeans_3_result_tbl %>% 
  left_join(cluster_label_tbl) %>% 
  distinct(Partner_code, .keep_all = TRUE) %>% # select the unique entity
  mutate(label_text = str_glue("Entity: {Partner_code}
                                 Cluster: {.cluster}
                               {.cluster_label}")) %>% # select the entity, cluster and labels for each cluster
  
  ggplot(aes(X, Y, color = .cluster)) +
  geom_point() +
  geom_label_repel(aes(label = label_text), size = 3) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "eCollection Payment Entity Segmentation 2D Projection",
    subtitle = "UMAP Kmeans Clustering"
  ) +
  theme(legend.position = "none")



# Analyzing the Impact of delays in payment delivery with respect to amount:

# Visualizing the delay duration in days against payment amount
ecol_class %>% 
  ggplot(aes(duration, Amount)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() + # making the background have a grid look
  scale_fill_tq() +
  labs(x = "Duration (Days)",
       y = "Payment Amount",
       title = "Large Payments seems to get completed Immediately",
       subtitle = "small payments likely don't get completed Immedately")


# Payment delay duration against payment amount across Payment Channels
ecol_pay_tbl %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  ggplot(aes(duration, Amount)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Pmtmthd_pmt_channel) +
  theme_tq() +
  scale_fill_tq() +
  labs(x = "Duration (Days)",
       y = "Payment Amount",
       title = "Bank Teller Payment have large Payment delay than other channels")

# Delayed Paymenst Above 6 Months
ecol_pay_tbl %>% 
  filter(duration >= 180) %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(Pmtmthd_pmt_channel) %>% 
  ggplot(aes(Pmtmthd_pmt_channel, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Number of Delay",
       x = "Channel",
       title = "Delayed Payment Above 6 Months Across Channels")


# Entity With the Highest Number of Delayed Payments Above 6 Months
ecol_pay_tbl %>% 
  filter(duration >= 180) %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(Partner_code) %>% 
  ggplot(aes(Partner_code, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Entity",
       y = "Number of Delay",
       title = "Entity with Delayed Payments Above 6 Months")



# Analyzing Delayed Pyaments above 3 Months for 2009 to 2019
ecol_pay_tbl %>% 
  filter(duration >= 90) %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  mutate(Year = year(Creation_date)) %>% 
  count(Pmtmthd_pmt_channel, Year) %>% 
  ggplot(aes(Pmtmthd_pmt_channel, n)) +
  geom_col() +
  facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Channel",
       y = "Number of Delays",
       title = "Delayed Payments of 3 Months from 2009 to 2019")


# Analyzing Delayed Payments from 2009 to 2019 Across Source Channel
ecol_pay_tbl %>% 
  filter(duration >= 90) %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  mutate(Year = year(Creation_date)) %>% 
  count(Partner_code, Year) %>% 
  ggplot(aes(Partner_code, n)) +
  geom_col() +
  facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Channel",
       y = "Number of Delayed Payments",
       title = "3 Months Payment Delay Across Entity")

  


ecol_pay_tbl %>% 
  filter(duration >= 90) %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  mutate(Year = year(Creation_date)) %>% 
  count(Source_channel, Year) %>% 
  ggplot(aes(Source_channel, n)) +
  geom_col() +
  facet_wrap(~Year) +
  theme_tq() +
  scale_fill_tq() +
  labs(x = "Source Channel",
       y = "Delayed Number of Payment",
       title = "Delayed Payment Above 3 Months From 2009 to 2019")



ecol_class %>% 
  count(duration_range) %>% 
  ggplot(aes(duration_range, n)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq()



ecol_class %>% 
  bind_cols(ecol_pay_tbl %>% select(Creation_date)) %>%
  mutate(Year = year(Creation_date)) %>%
  select(-Creation_date) %>%
  count(duration_range, Year) %>% 
  mutate(duration_range = fct_reorder2(duration_range, n, Year)) %>% 
  ggplot(aes(duration_range, n)) +
  geom_col() +
  facet_wrap(~Year) +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq()


ecol_class %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(duration_range, Pmtmthd_pmt_channel) %>% 
  ggplot(aes(duration_range, n, fill = Pmtmthd_pmt_channel)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq() +
  labs(x = "Duration Range",
       y = "Number of Delay",
       title = "Delayed Payment by Channels")

ecol_class %>% 
  bind_cols(ecol_pay_tbl %>% select(Creation_date)) %>%
  mutate(Year = year(Creation_date)) %>%
  select(-Creation_date) %>%
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(duration_range, Year, Pmtmthd_pmt_channel) %>% 
  mutate(duration_range = fct_reorder2(duration_range, n, Year)) %>% 
  ggplot(aes(duration_range, n, fill = Pmtmthd_pmt_channel)) +
  geom_col(position = "dodge") +
  facet_wrap(~Year) +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq() +
  labs(x = "Duration Range",
       y = "Number of Delay",
       title = "Delay Across Years (2009 - 2019")


ecol_class %>% 
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(duration_range, Source_channel) %>% 
  ggplot(aes(duration_range, n, fill = Source_channel)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq() +
  labs(x = "Duration",
       y = "Number of Delay",
       title = "Delayed Payment by Source Channel")


ecol_class %>% 
  bind_cols(ecol_pay_tbl %>% select(Creation_date)) %>%
  mutate(Year = year(Creation_date)) %>%
  select(-Creation_date) %>%
  filter(!is.na(Pmtmthd_pmt_channel)) %>% 
  count(duration_range, Year, Source_channel) %>% 
  mutate(duration_range = fct_reorder2(duration_range, n, Year)) %>% 
  ggplot(aes(duration_range, n, fill = Source_channel)) +
  geom_col(position = "dodge") +
  facet_wrap(~Year) +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_fill_tq()



# Decision Tree Algorithm for State Classification

# Remove Columns we don't need
ecol_class <- ecol_pay_tbl %>% 
  select(-Pmt_dt, 
         -Dlvry_ts, 
         -Last_update_date, 
         -Creation_date, 
         -Send_flag, -duration_minutes)



# Feature Engineering
ecol_class %>% 
  filter(!is.na(duration)) %>% 
  pull(duration) %>% 
  quantile(probs = c(0, 0.33, 0.66, 1))

# Feature Engineering
ecol_class <- ecol_class %>% 
  mutate(duration_range = case_when(
    duration <= 0.91 ~ "Low",
    duration <= 26.42 ~ "Medium",
    TRUE ~ "High"
  )) %>% 
  mutate(Amount_Range = case_when(
    Amount <= 333.00 ~ "Low",
    Amount <= 1230.00 ~ "Medium",
    TRUE ~ "High"
  ))


# Filter to only the PAID Status
ecol_paid_tbl <-  ecol_class %>% filter(State == "PAID")

# Filter to Only the Recon Status
ecol_recon <- ecol_class %>% 
  filter(State == "RECON")

# Split and Resample into Train and Test Model 
ecol_recon <- initial_split(ecol_recon, prop = 0.0012, strata = Amount_Range)

# Merge the Paid and Recon Dataframe together
ML_tbl <- training(ecol_recon) %>% rbind(ecol_paid_tbl)

write_rds(ML_tbl, path = "00_Data/ML_tbl.rds")

# Split Into Train and Test with 80% Trian and 20% Test
split_ml <- ML_tbl %>% initial_split(prop = 0.80, strata = State)

train_mof_poc_tbl <- training(split_ml)
train_mof_poc_tbl <- train_mof_poc_tbl %>% select(-Dlvry_status, -Send_dlvry_state, -Source_Channel)
test_mof_poc_tbl <- testing(split_ml)
test_mof_poc_tbl <- test_mof_poc_tbl %>% select(-Dlvry_status, -Send_dlvry_state, -Source_Channel)
test_mof_poc_tbl <- test_mof_poc_tbl %>% filter(Partner_code != "eProcurement Portal")

train_mof_poc_tbl <- train_mof_poc_tbl %>% 
  mutate(Bank_ID = as.character(Pmtmthd_bnk_id)) %>% 
  mutate(Channel = as.character(Pmtmthd_pmt_channel)) %>% 
  mutate(Payment_Method = as.character(Pmt_method)) %>% 
  select(-Pmtmthd_bnk_id, -Pmtmthd_pmt_channel, -Pmt_method)



test_mof_poc_tbl <- test_mof_poc_tbl %>% 
  mutate(Bank_ID = as.character(Pmtmthd_bnk_id)) %>% 
  mutate(Channel = as.character(Pmtmthd_pmt_channel)) %>% 
  mutate(Payment_Method = as.character(Pmt_method)) %>% 
  select(-Pmtmthd_bnk_id, -Pmtmthd_pmt_channel, -Pmt_method)
  

View(train_mof_poc_tbl)

write_rds(train_mof_poc_tbl, path = "00_Data/train_mof_poc_tbl.rds")

write_rds(mof_decison_tree_model, path = "00_Data/mof_decison_tree_model.rds")



# Creating the Decision Tree Model
mof_decison_tree_model <-  decision_tree(mode = "classification", 
              cost_complexity = 0.01,
              tree_depth = 5, 
              min_n = 10) %>% 
  set_engine("rpart") %>% 
  fit(State~., data = train_mof_poc_tbl)


# Testing the Decision Tree Model on the Test Sample
mof_state_prediction <-  predict(mof_decison_tree_model, test_mof_poc_tbl[-3])

# Visualize the Decision Tree with the RPart Plot
mof_decison_tree_model$fit %>% 
  rpart.plot(roundint = FALSE, 
             type = 1, 
             extra = 101,
             fallen.leaves = TRUE,
             cex = 0.6)

# Accuracy of 99%
mof_decison_tree_model %>% 
  predict(new_data = test_mof_poc_tbl %>% 
            select(-State)) %>% 
  bind_cols(test_mof_poc_tbl %>% select(State)) %>% 
  yardstick::metrics(truth = State, estimate = .pred_class)


# Create the Confusion Matrix Table
CrossTable(test_mof_poc_tbl$State, mof_state_prediction$.pred_class, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual","Predicted"))



# Precision 100%
Precision = 74/(74 + 0)
Precision

# Recall 99%
Recall_Sensitivity = 74/(74 + 1)
Recall_Sensitivity

# Specificity 100%
Specificity = 92/(92 + 0)
Specificity











