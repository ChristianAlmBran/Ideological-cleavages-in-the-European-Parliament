# Loading packages
pacman::p_load(readxl, openxlsx, lubridate, tidyverse)

# Loading dataframes
## All legislation
ep7_voted_original <- read_excel("EP7_Voted docs.xlsx")
ep8_voted_original <- read_excel("EP8_Voted docs.xlsx")
ep9_voted_original <- read_excel("EP9_Voted docs.xlsx")

## Only environmental legislation df
ep7_voted_env <- read_excel("ep7_voted_env_public_health_marked_all_votes.xlsx")
ep8_voted_env <- read_excel("ep8_voted_env_public_health_marked_all_votes.xlsx")
ep9_voted_env <- read_excel("ep9_voted_env_public_health_marked_all_votes.xlsx")

# Filtering
## EP7
### Only environmental votes
filtered_df_ep7 <- ep7_voted_original %>% filter(`Vote ID` %in% ep7_voted_env$`Vote ID`)
summary(filtered_df_ep7)

### Only legislative votes
filtered_df_ep7_leg <- ep7_voted_original %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep7_leg)

### Only environmental legislative votes
filtered_df_ep7_leg_env <- filtered_df_ep7 %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep7_leg_env)

## EP8
### Only environmental votes
filtered_df_ep8 <- ep8_voted_original %>% filter(`Vote ID` %in% ep8_voted_env$`Vote ID`)
summary(filtered_df_ep8)

### Only legislative votes
filtered_df_ep8_leg <- ep8_voted_original %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep8_leg)

### Only environmental legislative votes
filtered_df_ep8_leg_env <- filtered_df_ep8 %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep8_leg_env)

## EP9
### Only environmental votes
filtered_df_ep9 <- ep9_voted_original %>% filter(`Vote ID` %in% ep9_voted_env$`Vote ID`)
summary(filtered_df_ep9)

### Only legislative votes
filtered_df_ep9_leg <- ep9_voted_original %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep9_leg)

### Only environmental legislative votes
filtered_df_ep9_leg_env <- filtered_df_ep9 %>% filter(`Leg/Non-Leg/Bud` == "Leg")
summary(filtered_df_ep9_leg_env)

# IPP using only environmental votes
## EP7
ipp_ep7 <- filtered_df_ep7 %>%
  mutate(Total = Yeas + No + Abs,
         M_i = (Yeas / Total) * 100)

IPP_ep7 <- sqrt(mean((100 - ipp_ep7$M_i)^2, na.rm = TRUE))

## Print result
print(IPP_ep7)

## EP8
ipp_ep8 <- filtered_df_ep8 %>%  
  mutate(Total = Yeas + No + Abs,  
         M_i = (Yeas / Total) * 100) 

IPP_ep8 <- sqrt(mean((100 - ipp_ep8$M_i)^2, na.rm = TRUE))

## Print result
print(IPP_ep8)

## EP9
ipp_ep9 <- filtered_df_ep9 %>% 
  mutate(Total = Yes + No + Abs, 
         M_i = (Yes / Total) * 100)  

ipp_ep9 <- sqrt(mean((100 - ipp_ep9$M_i)^2, na.rm = TRUE))

# Print result
print(ipp_ep9)


# Compute IPP using all votes
## EP7
ipp_ep7 <- ep7_voted_original %>%
  mutate(Total = Yeas + No + Abs,
         M_i = (Yeas / Total) * 100) 

IPP_ep7 <- sqrt(mean((100 - ipp_ep7$M_i)^2, na.rm = TRUE))

print(IPP_ep7)

## EP8
ipp_ep8 <- ep8_voted_original %>%
  mutate(Total = Yeas + No + Abs, 
         M_i = (Yeas / Total) * 100) 

IPP_ep8 <- sqrt(mean((100 - ipp_ep8$M_i)^2, na.rm = TRUE))

print(IPP_ep8)

## EP9
ipp_ep9 <- ep9_voted_original %>% 
  mutate(Total = Yes + No + Abs,  
         M_i = (Yes / Total) * 100)

ipp_ep9 <- sqrt(mean((100 - ipp_ep9$M_i)^2, na.rm = TRUE))

print(ipp_ep9)


# Compute IPP using only legislative votes
## EP7
ipp_ep7 <- filtered_df_ep7_leg %>% 
  mutate(Total = Yeas + No + Abs, 
         M_i = (Yeas / Total) * 100) 

IPP_ep7 <- sqrt(mean((100 - ipp_ep7$M_i)^2, na.rm = TRUE))

print(IPP_ep7)

## EP8
ipp_ep8 <- filtered_df_ep8_leg %>%  
  mutate(Total = Yeas + No + Abs,  
         M_i = (Yeas / Total) * 100)  

IPP_ep8 <- sqrt(mean((100 - ipp_ep8$M_i)^2, na.rm = TRUE))

print(IPP_ep8)

## EP9
ipp_ep9 <- filtered_df_ep9_leg %>% 
  mutate(Total = Yes + No + Abs, 
         M_i = (Yes / Total) * 100)  

ipp_ep9 <- sqrt(mean((100 - ipp_ep9$M_i)^2, na.rm = TRUE))

print(ipp_ep9)


# Compute IPP using only environmental legislative votes
## EP7
ipp_ep7 <- filtered_df_ep7_leg_env %>% 
  mutate(Total = Yeas + No + Abs, 
         M_i = (Yeas / Total) * 100) 

IPP_ep7 <- sqrt(mean((100 - ipp_ep7$M_i)^2, na.rm = TRUE))

print(IPP_ep7)


## EP8
ipp_ep8 <- filtered_df_ep8_leg_env %>%
  mutate(Total = Yeas + No + Abs,  
         M_i = (Yeas / Total) * 100)

IPP_ep8 <- sqrt(mean((100 - ipp_ep8$M_i)^2, na.rm = TRUE))

print(IPP_ep8)

## EP9
ipp_ep9 <- filtered_df_ep9_leg_env %>% 
  mutate(Total = Yes + No + Abs,  
         M_i = (Yes / Total) * 100)  

ipp_ep9 <- sqrt(mean((100 - ipp_ep9$M_i)^2, na.rm = TRUE))

print(ipp_ep9)
