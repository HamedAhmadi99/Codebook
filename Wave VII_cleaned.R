
library(tidyverse)

df = read_csv('AB7_ENG_Release_Version6.csv')

# Filter the data for the specified countries

df = df|>
  filter(COUNTRY %in% c(1, 12, 13, 21)) |>
  mutate(
    c_name = case_when(
      COUNTRY == 1  ~ "Algeria",
      COUNTRY == 12 ~ "Mauritania",
      COUNTRY == 13 ~ "Morocco",
      COUNTRY == 21 ~ "Tunisia"
    )
  )

table(df$c_name)

# Add a new variable 'source' with a constant value
df = df |>
  mutate(source = "Arab Barometer Wave VII")


df$source


# Rename 'ID' to 'r_id'
df = df |>
  rename(r_id = ID)

df$r_id


# Split 'DATE' into 'r_year' and 'r_month'
df = df |>
  mutate(
    r_year = year(ymd(DATE)),
    r_month = month(ymd(DATE))
  )


table(df$r_year)
table(df$r_month)



# Create 'r_female' as a binary indicator for female
df = df |>
  mutate(r_female = if_else(Q1002 == 2, 1, 0))


table(df$r_female)




# Clean, reverse order, change to factor variable and rename Q404 to p_interest

df <- df %>%
  mutate(
    p_interest_num = case_when(
      Q404 == 1 ~ 4,
      Q404 == 2 ~ 3,
      Q404 == 3 ~ 2,
      Q404 == 4 ~ 1,
      TRUE ~ NA_real_  
    )
  )


df <- df %>%
  mutate(
    p_interest = factor(
      p_interest_num,
      levels = c(1, 2, 3, 4),
      labels = c("Very uninterested", "Uninterested", "Interested", "Very interested")
    )
  )



table(df$p_interest)


# Clean, reverse order, change to factor variable and rename Q101 to p_economy


df <- df %>%
  mutate(
    p_economy_num = case_when(
      Q101 == 1 ~ 4,  # "Very good" becomes 4
      Q101 == 2 ~ 3,  # "Good" becomes 3
      Q101 == 3 ~ 2,  # "Bad" becomes 2
      Q101 == 4 ~ 1,  # "Very bad" becomes 1
      TRUE ~ NA_real_  # Recode "Don't know" and "Refused to answer" as NA
    )
  )



df <- df %>%
  mutate(
    p_economy = factor(
      p_economy_num,
      levels = c(1, 2, 3, 4),
      labels = c("Very bad", "Bad", "Good", "Very good")
    )
  )

table(df$p_economy)

df$p_economy




# Clean the 4 measures of satisfaction/preference for democracy
df <- df %>%
  mutate(
    Q516_1_num = case_when(
      Q516_1 == 1 ~ 4,
      Q516_1 == 2 ~ 3,
      Q516_1 == 3 ~ 2,
      Q516_1 == 4 ~ 1,
      TRUE ~ NA_real_  
    ),
    Q516_2_num = case_when(
      Q516_2 == 1 ~ 4,
      Q516_2 == 2 ~ 3,
      Q516_2 == 3 ~ 2,
      Q516_2 == 4 ~ 1,
      TRUE ~ NA_real_
    ),
    Q516_3_num = case_when(
      Q516_3 == 1 ~ 4,
      Q516_3 == 2 ~ 3,
      Q516_3 == 3 ~ 2,
      Q516_3 == 4 ~ 1,
      TRUE ~ NA_real_
    ),
    Q516_4_num = case_when(
      Q516_4 == 1 ~ 4,
      Q516_4 == 2 ~ 3,
      Q516_4 == 3 ~ 2,
      Q516_4 == 4 ~ 1,
      TRUE ~ NA_real_
    )
  )


df <- df %>%
  mutate(
    Q516_1 = factor(
      Q516_1_num,
      levels = c(1, 2, 3, 4),
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
    ),
    Q516_2 = factor(
      Q516_2_num,
      levels = c(1, 2, 3, 4),
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
    ),
    Q516_3 = factor(
      Q516_3_num,
      levels = c(1, 2, 3, 4),
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
    ),
    Q516_4 = factor(
      Q516_4_num,
      levels = c(1, 2, 3, 4),
      labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
    )
  ) %>%
  select(-matches("_num$"))  


table(df$Q516_1)
table(df$Q516_2)
table(df$Q516_3)
table(df$Q516_4)






# Create a new dataframe with variables in the specified order
cleaned_df <- df[, c("c_name", "source", "r_id", "r_year", "r_month", "r_female", "p_interest", "p_economy", "Q516_1", "Q516_2", "Q516_3", "Q516_4")]

# Save as .CSV
write.csv(cleaned_df, file = 'Wave_VII_Maghreb.csv', row.names = FALSE)


# save as .Rdata
save(cleaned_df, file = "Wave_VII_Maghreb.RData")


 Maghreb = read_csv("Wave_VII_Maghreb.csv")
 







