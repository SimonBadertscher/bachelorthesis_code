# Loaded necessary packages.
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(haven)
library(kableExtra)
library(sjlabelled)
library(xtable)
library(ggpubr)
library(extrafont)
library(labelled)
library(stargazer)
library(networkD3)
library(webshot)
library(DescTools)
library(plm)
library(readr)
library(olsrr)
library(lmtest)
library(nlme)
library(fixest)
library(scales)
library(modelsummary)


### Data preparation

# Load fonts (especially "CMU Serif") for the plot description.
extrafont::loadfonts(device="win")


# Set working directory (the SHP and worldbank data should be in this folder).
wd <- "C:/Users/Simon/OneDrive/Uni Bern/Bachelorarbeit/Data"
setwd(wd)



# Setting up the years and variables to then combine the annual personal and 
# annual household data.

year <- c("02", "03", "04", "05", "06", "07", "08", "09", 10:19)

setnames_variables_p <- c("id","interview_type","househ_id","isced","edu_year","esec","isco_main","occupation","age","sex","civstat",
                          "wstat","public_private","annual_gross_income_emp","WfH","commute_m",
                          "n_employees","conWH","actualHW", "income_sat","cond_sat","atmo_sat","n_kids",
                          "health_stat","con_type","superv","first_nat_reg","job_quali")



for (y in year) {
  # Loaded the data sets individually.
  file_path <- paste0("shp", y,"_p_user.dta")
  df <- read_dta(file_path)
  select_variables_p <- c("idpers", paste0("status",y),paste0("idhous",y),paste0("isced",y),paste0("edyear",y),paste0("esecmj",y),
                          paste0("is1maj",y),paste0("occupa",y),paste0("age",y), 
                          paste0("sex",y),paste0("civsta",y),paste0("wstat",y),paste0("p",y,"w32"),paste0("i",y,"empyg"),paste0("p",y,"w80"),paste0("p",y,"w84"),paste0("p",y,"w85"),
                          paste0("p",y,"w74"),paste0("p",y,"w77"),paste0("p",y,"w92"),
                          paste0("p",y,"w93"),paste0("p",y,"w94"),paste0("ownkid",y),
                          paste0("p",y,"c01"),paste0("p",y,"w37"),paste0("p",y,"w87"),
                          paste0("reg_1_",y),paste0("p",y,"w100"))
  df <- df[select_variables_p]
  # Rename the chosen variables.
  setnames(df, select_variables_p,setnames_variables_p)
  # Keep only personal interviews.
  df <- df %>% filter(get(paste0("interview_type")) == 0)
  df <- df[, -which(names(df) == "interview_type")]
  # Add year as a variable.
  df$year <- as.numeric(paste0("20",y))
  
  # Save the annual adjusted dataset.
  write_dta(df, file.path(wd, paste0("temp_p", y, ".dta")))
}

# Load all datasets and combine them.
datasets <- list()
for (i in 1:length(year)){
  y <- year[i]
  datasets[[i]] <- read_dta(paste0("temp_p",y,".dta"))
}

temp_data <- bind_rows(datasets)
# Remove the original sets.
remove(datasets)

# Save the combined personal dataset.
write_dta(temp_data, "data_p.dta")


# Setting up the variables to then combine the annual household data.

setnames_variables_h <- c("househ_id","househ_type","new_born","hh_n_kids","h_you_kid","h_old_kid")

for (y in year) {
  file_path <- paste0("shp", y,"_h_user.dta")
  df <- read_dta(file_path)
  select_variables_h <- c(paste0("idhous",y),paste0("hldcen",y),paste0("nbb_",y),paste0("nbkid",y),paste0("ayouki",y),paste0("aoldki",y))
  # Select the chosen variables.
  df <- df[select_variables_h]
  # Rename the chosen variables.
  setnames(df, select_variables_h,setnames_variables_h)
  # Add year as a variable.
  df$year <- as.numeric(paste0("20",y))
  
  # Save the annual adjusted dataset.
  write_dta(df, file.path(wd, paste0("temp_h", y, ".dta")))
}


# Load all datasets and combine them.
datasets <- list()
for (i in 1:length(year)){
  y <- year[i]
  datasets[[i]] <- read_dta(paste0("temp_h",y,".dta"))
}
temp_data <- bind_rows(datasets)
# Remove the original sets.
remove(datasets)

write_dta(temp_data, "data_h.dta")
# Save the combined household dataset.


# Load both datasets.
data_p <- read_dta(paste0(wd,"/data_p.dta"))
data_h <- read_dta(paste0(wd,"/data_h.dta"))

# Merge the personal and household datasets.
data <- merge(data_p,data_h,by = c("househ_id","year"))

# Write and load the data (necessary only if the code is not run consecutively.)
write_dta(data, "raw_data.dta")

data <- read_dta("raw_data.dta")

### Data filtration

# Keep only people that work.
data <-  data[which(data$wstat == 1),]

# Keep only positive values for multiple variables.
data <- data[which(data$conWH >= 0),]
data <- data[which(data$actualHW >= 0),]
data <- data[which(data$annual_gross_income_emp >=0),]
data <- data[which(data$income_sat >= 0 ),]
data <- data[which(data$cond_sat >= 0),]
data <- data[which(data$atmo_sat >= 0),]
data <- data[which(data$n_kids >= 0),]
data <- data[which(data$isced >= 0),]
data <- data[which(data$esec >0),]
data <- data[which(data$n_employees > 0),]
data <- data[which(data$public_private > 0),]
# Keep age observations that are between 18 and 70.
data <- data[which(data$age %in% c(18:70)),]
# Keep only contractual working hours below 50.
data <- data[which(data$conWH < 50),]

# Recode supervisory role.
data$superv[data$superv == 2] <- 0
attr(data[["superv"]], "labels") <- c("No" = 0, "Yes" = 1)
data <- mutate(data, superv = set_label(superv, label = "Supervisory Role"))
data <- data[which(data$superv  %in% c(0,1)),]

# Recode number of employees.
data$n_employees[data$n_employees %in% c(1:2)] <- 1
data$n_employees[data$n_employees %in% c(3:5)] <- 2
data$n_employees[data$n_employees %in% c(6:7)] <- 3
data$n_employees[data$n_employees %in% c(8:9)] <- 4
attr(data[["n_employees"]], "labels") <- c("1-19" = 0, "20-99" = 1,"100-499"=2,"500+" = 3)
data <- mutate(data, n_employees = set_label(n_employees, label = "Number of employees"))

# Recode nationalities.
data$first_nat_reg[data$first_nat_reg == 10] <- 0
data$first_nat_reg[data$first_nat_reg %in% c(11:17,31)] <- 1
data$first_nat_reg[data$first_nat_reg %in% c(20,30,40,50,60)] <- 2

attr(data[["first_nat_reg"]], "labels") <- c("Switzerland" = 0, "Europe or North America" = 1
                                             ,"Rest of the world" = 2)
data <- mutate(data, first_nat_reg = set_label(first_nat_reg, label = "First nationality"))
data <- data[which(data$first_nat_reg  %in% c(0:2)),]

# Recode contract types (remove trainees and apprentices).
data <-  data[which(data$con_type %in% c(-3:-1,3:10)),]
attr(data[["con_type"]], "labels") <- c("as unempl person participating in occupational programmer" = 0,
                                        "as seasonal worker" = 1 ,"as casual worker" =2,
                                        "as other type of employee" = 3, "through a placement agency" = 4,
                                        "A trial period" = 5, 
                                        "A Position regularly renewed (e.g. teaching)" = 6, "Other" = 7)
data <- mutate(data, con_type = set_label(con_type, label = "Contract type"))

# Recode marital status (only married/registered partnership or single remain).
data$civstat[data$civstat %in% c(1,3,4,5,7)] <- 0
data$civstat[data$civstat %in% c(2,6)] <- 1
attr(data[["civstat"]], "labels") <- c("Single" = 0, "Married or registered partnership" = 1)
data <- mutate(data, civstat = set_label(civstat, label = "Martial status"))
data <- data[which(data$civstat >= 0),]

# Add experience variable as contructed variable from age and education.
data$experience <- (data$age - data$edu_year - 6) 
data <- mutate(data, experience = set_label(experience, label = "Years of experience"))


# Recode gender.
data$sex[data$sex == 1] <- 0
data$sex[data$sex == 2] <- 1
attr(data[["sex"]], "labels") <- c("Male" = 0, "Female" = 1)
data <- mutate(data, sex = set_label(sex, label = "Gender"))



# Recode WfH.
data$WfH[data$WfH %in% c(1:3)] <- 1
data$WfH[data$WfH %in% c(5)] <- 0
data <- data[which(data$WfH %in% c(0,1)),]
attr(data[["WfH"]], "labels") <- c("No WfH" = 0, "WfH" = 1)
data <- mutate(data, WfH = set_label(WfH, label = "Working from home"))


# Construct hourly_wage and fte_salaries.
data$hourly_wage <- (data$annual_gross_income_emp/52)/data$conWH
data$fte_salaries <- (data$annual_gross_income_emp/data$conWH)*40

# Calculations for real wages. (The file "inflation_rates_worldbank.csv"
# is also available on github.)
inflation_rates_worldbank <- read_csv("inflation_rates_worldbank.csv", skip = 3)
swiss_inflation_data <-  inflation_rates_worldbank[which(inflation_rates_worldbank$`Country Name` == "Switzerland"),]
remove(inflation_rates_worldbank)
swiss_inflation_data <-  head(data.frame(t(swiss_inflation_data)[-c(1:4),]),-2)
swiss_inflation_data$year <- rownames(swiss_inflation_data)
swiss_inflation_data[,1] <- as.numeric(swiss_inflation_data[,1])/100
colnames(swiss_inflation_data) <- c("inflation_rate_dec","year")
rownames(swiss_inflation_data) <- c(1:length(swiss_inflation_data$year))
swiss_inflation_data <- swiss_inflation_data[which(swiss_inflation_data$year %in% c(2003:2019)),] 

cpi_adjust <- c(1)

# Calculating real hourly wages and real full-time equivalent salaries.
for (i in c(1:length(swiss_inflation_data$inflation_rate_dec))){
  cpi_adjust <- append(cpi_adjust,cpi_adjust[i]/(1+swiss_inflation_data$inflation_rate_dec[i]))
}
x <- 0
data$real_fte_salaries <- 0
data$real_hourly_wage <- 0
for (i in sort(unique(data$year))){
  x = x +1
  print(i)
  data[which(data$year == i),"real_fte_salaries"] <- data[which(data$year == i),"fte_salaries"] * cpi_adjust[x]
  data[which(data$year == i), "real_hourly_wage"] <- data[which(data$year == i), "hourly_wage"] * cpi_adjust[x]
}

data <- mutate(data, real_hourly_wage = set_label(real_hourly_wage, label = "Real hourly wage"))
data <- mutate(data, real_fte_salaries = set_label(real_fte_salaries, label = "Real full-time equivalent salary"))
data <- mutate(data, real_hourly_wage = set_label(real_hourly_wage, label = "Real hourly wage"))
data <- mutate(data, fte_salaries = set_label(fte_salaries, label = "Full-time equivalent salary"))
data <- mutate(data, hourly_wage = set_label(hourly_wage, label = "Hourly wage"))

# Weekly overtime calculation.
data$overtime <- data$actualHW- data$conWH
data <- mutate(data, overtime = set_label(overtime, label = "Overtime hours"))
               
# Keep only real_hourly wages that are smaller than the top 0.5% and  greater than
# 10 CHF.
data <-  data[which(data$real_hourly_wage < quantile(data$real_hourly_wage,0.995)),]
data <-  data[which(data$real_hourly_wage >= 10),]

# Keep only observations that appear atleast 10 times in the panel data.
data <- data %>%
  group_by(id) %>%
  filter(n() > 9)

# Recode Highest level of education
data$isced[data$isced ==  10] <- 0
data$isced[data$isced == 20] <- 1
data$isced[data$isced == 31] <- 2
data$isced[data$isced == 32] <- 3
data$isced[data$isced == 33] <- 4
data$isced[data$isced == 41] <- 5
data$isced[data$isced %in% c(51,52,60)] <- 6

attr(data$isced,"labels") <- c(
  "Primary or first stage of basic education" = 0,
  "Lower secondary or Second stage of basic education" = 1,
  "Upper secondary education (preparation for tertiary education"= 2,
  "Upper secondary education (preparation for further prof. education)" = 3,
  "Upper secondary education (entrance into the labor market)" = 4,
  "Post-secondary education non tertiary (preparation for an institution for higher education)" = 5,
  "First or second stage of tertiary education"= 6)
data$isced <- set_label(data$isced, label = "Highest level of education")


# Generate child under a certain age variables and recode education to only have
# three categories.
data <- data %>%
  group_by(id) %>%
  mutate(
    kid_u_3 = h_you_kid %in% c(0:3),
    kid_4_6 = h_you_kid %in% c(4:6),
    kid_7_9 = h_you_kid %in% c(7:9),
    kid_10_12 = h_you_kid %in% c(10:12),
    kid_13_17 = h_you_kid %in% c(13:17),
    kid_max_12 = h_you_kid %in% c(0:11),
    basic_edu = isced %in% c(0:1),
    upper_and_post_secondary_edu = isced %in% c(2:5),
    tertiary_edu = isced %in% c(6)
  )

data$basic_edu <- set_label(data$basic_edu, label = "Highest level of education: basic education")
attr(data[["basic_edu"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$upper_and_post_secondary_edu <- set_label(data$upper_and_post_secondary_edu, label = "Highest level of education: secondary education")
attr(data[["upper_and_post_secondary_edu"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$tertiary_edu <- set_label(data$upper_and_post_secondary_edu, label = "Highest level of education: tertiary education")
attr(data[["tertiary_edu"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)

data$kid_u_3 <- set_label(data$kid_u_3, label = "Youngest child under 3")
attr(data[["kid_u_3"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$kid_4_6 <- set_label(data$kid_4_6, label = "Youngest child between 4 and 6")
attr(data[["kid_4_6"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$kid_7_9 <- set_label(data$kid_7_9, label = "Youngest child between 7 and 9")
attr(data[["kid_7_9"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$kid_10_12 <- set_label(data$kid_10_12, label = "Youngest child between 10 and 12")
attr(data[["kid_10_12"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$kid_13_17 <- set_label(data$kid_13_17, label = "Youngest child between 13 and 17")
attr(data[["kid_13_17"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$kid_max_12 <- set_label(data$kid_max_12, label = "Youngest child under 13")
attr(data[["kid_max_12"]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
data$year <- set_label(data$year, label = "Year")

write_dta(data,"data.dta")





### Descriptive analysis

# Set dpi_n for ggsave (quality).
dpi_n <- 1000

# Only necessary if not run consecutively.
data <- read_dta("data.dta") 

# Seperate the data into men and women.
data_male <- data[which(data$sex == 0),]
data_female <- data[which(data$sex == 1),]

# Function that generates a summary statistic including a t-test for select variables.
summary_table <- function(variable_name,sex, year=0){
  if (variable_name != "n"){
    col_n <- which(colnames(data) == variable_name)
  }
  data_WfH <- data[which(data$WfH == 1),]
  data_no_WfH <- data[which(data$WfH == 0),]
  if (sex == "female"){
    data_WfH <- data_WfH[which(data_WfH$sex == 1),]
    data_no_WfH <- data_no_WfH[which(data_no_WfH$sex == 1),]
  }
  else if (sex == "male"){
    data_WfH <- data_WfH[which(data_WfH$sex == 0),]
    data_no_WfH <- data_no_WfH[which(data_no_WfH$sex == 0),] 
  }
  if (all(year %in% unique(data$year))){
    data_WfH <-  data_WfH[which(data_WfH$year %in% year),]
    data_no_WfH <-  data_no_WfH[which(data_no_WfH$year %in% year),]
  }
  if (variable_name == "n"){
    return(list(as.character(length(data_WfH$WfH)),as.character(length(data_no_WfH$WfH))," "," "))
  }
  a <- t.test(data_WfH[,col_n],data_no_WfH[,col_n])
  signi = ""
  if (a$p.value < 0.001){
    signi = "***"
  } else if (a$p.value < 0.05){
    signi = "**"
  } else if (a$p.value < 0.10){
    signi = "*"
  }
  return(list(round(a$estimate[1],2),round(a$estimate[2],2),paste0(round(a$estimate[1]-a$estimate[2],2),signi),round(a$statistic,2)))
}

# Row names for the latex table.
r_names_list <-  c("Real hourly wage","Contractual working hours","Actual working hours","Overtime (hours)","Age","Number of children","Youngest child younger than 3","Youngest child between 4 and 6",
                   "Youngest child between 7 and 9","Youngest child between 10 and 12","Youngest child between 13 and 17","Tertiary education",
                   "Upper and post secondary education","Basic education","Income Satisfaction","Satisfaction work conditions","Satisfaction work atmosphere","N")

# Choose variables.
variables <- c("real_hourly_wage","conWH","actualHW","overtime","age","n_kids","kid_u_3","kid_4_6","kid_7_9","kid_10_12","kid_13_17","tertiary_edu",
               "upper_and_post_secondary_edu","basic_edu","income_sat","cond_sat","atmo_sat","n")

# Generate appropriate dataframes and generate a table.

table_frame_male <- data.frame()
table_frame_female <- data.frame()
for (v in variables){
  table_frame_female <- rbind(table_frame_female,c(summary_table(v,"female")))
}
colnames(table_frame_female) <- c("WfH","No WfH","Difference","T-Statistic")
rownames(table_frame_female) <- r_names_list
table_frame_male <- data.frame()

for (v in variables){
  table_frame_male <- rbind(table_frame_male,c(summary_table(v,"male")))
}
colnames(table_frame_male) <- c("WfH","No WfH","Difference","T-Statistic")
rownames(table_frame_male) <- r_names_list


# Clean up so that the latex tables are correctly formatted.
table_frame <- cbind(table_frame_female, table_frame_male)
out <- capture.output(print(xtable(table_frame), type = "latex", include.rownames = TRUE, include.colnames = TRUE))
start <- grep("^\\\\begin\\{tabular\\}", out)
stop <- grep("^\\\\end\\{tabular\\}", out)
out <- out[start:stop]

# Add title.
titles <- "& & Female& & & & Male && \\\\"


lines <- unlist(strsplit(out,"\n"))
lines <- gsub("rllllllll","r|llll|llll",lines)
position <- which(lines == "\\begin{tabular}{r|llll|llll}") + 1
mod_lines <- append(lines, titles, position)
mod_table <- paste(mod_lines, collapse = "\n")
cat(mod_table)

write(mod_table,"summary_statistics.tex")



# Generate annual means for multiple variables.
annual_mean <- data %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_male <- data_male %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_female <- data_female %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_male_kids <- data_male[which(data_male$kid_max_12 == TRUE),] %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_female_kids <- data_female[which(data_female$kid_max_12 == TRUE),] %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_male_no_kids <- data_male[which(data_male$kid_max_12 ==FALSE),] %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))

annual_mean_female_no_kids <- data_female[which(data_female$kid_max_12 ==FALSE),] %>%
  group_by(year) %>%
  summarize(mean_value = mean(WfH))



# Generate ID frequency distribution plot.

panel_dist <-  data.frame((table(table(data$id)))/sum(table(table(data$id))))

gg_unbalanced_panel <- ggplot(data= panel_dist, aes(x = Var1, y = Freq))+
  geom_bar(stat ="identity",fill= "blue") +
  labs(title = "Distribution of the unbalanced panel", x = "Number of observations per ID", y = "Share") +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("gg_unbalanced_panel.png",gg_unbalanced_panel, dpi= dpi_n, width = 8, height = 5)

# Generate  wage by year 
mean_wage_by_year <- data %>%
  group_by(year) %>%
  summarise(mean(fte_salaries),
            mean(real_fte_salaries))

# Generate real and nominal fte salaries by year plot. 
gg_adjusted_wage_by_year <- ggplot(data= reshape2::melt(mean_wage_by_year, id = c("year")), aes(x = year, y = value,fill = variable))+
  geom_col(position = "dodge") +
  labs(title = "Mean nominal and real wage by year", x = "Year", y = "Wage") +
  coord_cartesian(xlim = c(2002,2019), ylim = c(80000,110000)) +
  scale_fill_discrete(name ="",labels = c("Nominal wage","Real wage"))+
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.text =  element_text(size=18),
        legend.position  = c(.18,.9),
        legend.background = element_rect(fill = "transparent"))

ggsave("gg_adjusted_wage_by_year.png",gg_adjusted_wage_by_year,dpi = dpi_n, width = 8, height = 5)

# Mean contractual working hours by year.

mean_conWH_by_year <- data %>%
  group_by(year) %>%
  summarise(mean(actualHW),mean(conWH))

gg_conWH_actualHW_by_year <- ggplot(data= reshape2::melt(mean_conWH_by_year, id = c("year")), aes(x = year, y = value,fill = variable))+
  geom_col(position = "dodge") +
  labs(title = "Mean contractual and actual working hours by year", x = "Year", y = "Working hours") +
  coord_cartesian(xlim = c(2002,2019), ylim = c(30,40)) +
  scale_fill_discrete(name ="",labels = c("Actual hours worked","Contractual working hours"))+
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.text =  element_text(size=18),
        legend.position  = c(.25,.9),
        legend.background = element_rect(fill = "transparent"))

ggsave("gg_conWH_actualHW_by_year.png",gg_conWH_actualHW_by_year,dpi = dpi_n, width = 8, height = 5)


# gg_cpi_by_year <- ggplot(data= data.frame(cpi_adjust,mean_wage_by_year$year), aes(x = mean_wage_by_year.year, y = cpi_adjust))+
#   geom_bar(stat ="identity",fill= "blue") +
#   labs(title = "Aggregated inflation factor by year", x = "Year", y = "Aggregated inflation factor") +
#   coord_cartesian(xlim = c(2002,2019), ylim = c(0.92,1)) +
#   theme(text = element_text(family = "CMU Serif"),
#         axis.title = element_text(size=18),
#         axis.text = element_text(size=18),
#         plot.title = element_text(size = 20, hjust = 0.5),
#         axis.title.x = element_text(margin = margin(t = 10)),
#         axis.title.y = element_text(margin = margin(r = 10)))

# Generate mean age by year.
mean_age_by_year <- data %>%
  group_by(year) %>%
  summarise(mean(age))

# Generate plot for mean age by year.
gg_age_by_year <- ggplot(data= mean_age_by_year, aes(x = year, y = `mean(age)`))+
  geom_bar(stat ="identity",fill= "blue") +
  labs(title = "Mean age by year", x = "Year", y = "Age") +
  coord_cartesian(xlim = c(2002,2019), ylim = c(35,55)) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("gg_age_by_year.png",gg_age_by_year,dpi = dpi_n, width = 8, height = 5)

# Generate mean satisfaction with working conditions.
mean_cond_sat_by_year <- data %>%
  group_by(year) %>%
  summarise(mean(cond_sat))

# Generate plot for mean satisfaction with working conditions by year.
gg_mean_cond_sat_by_year <- ggplot(data= mean_cond_sat_by_year, aes(x = year, y = `mean(cond_sat)`))+
  geom_bar(stat ="identity",fill= "blue") +
  labs(title = "Mean satisfaction with work conditions by year", x = "Year", y = "Mean satisfaction with work conditions") +
  coord_cartesian(xlim = c(2002,2019), ylim = c(7,8)) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("gg_mean_cond_sat_by_year.png",gg_mean_cond_sat_by_year,dpi = dpi_n, width = 8, height = 5)

# Calculate mean WfH by year.
mean_wfh_by_year <- data %>%
  group_by(year) %>%
  summarise(mean(WfH))

# Generate plot for mean WfH by year.
gg_mean_wfh_by_year <- ggplot(data= mean_wfh_by_year, aes(x = year, y = `mean(WfH)`))+
  geom_bar(stat ="identity",fill= "blue") +
  labs(title = "Mean share WfH by year", x = "Year", y = "Share WfH") +
  coord_cartesian(xlim = c(2002,2019), ylim = c(0.2,0.4)) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("gg_mean_wfh_by_year.png",gg_mean_wfh_by_year,dpi = dpi_n, width = 8, height = 5)

# Generate a dataframe for plots for men and women.
frame_annual_mean_female <- data.frame(
  Year = rep(annual_mean_female$year,3),
  Mean_Values = c(annual_mean_female$mean_value,annual_mean_female_no_kids$mean_value,annual_mean_female_kids$mean_value),
  Group = c(rep("Female",length(unique(data$year))),rep("Female (no child younger than 13)",length(unique(data$year))),rep("Female (child younger than 13)",length(unique(data$year))))
)

frame_annual_mean_male <- data.frame(
  Year = rep(annual_mean_male$year,3),
  Mean_Values = c(annual_mean_male$mean_value,annual_mean_male_no_kids$mean_value,annual_mean_male_kids$mean_value),
  Group = c(rep("Male",18),rep("Male (no younger than 13)",18),rep("Male (child younger than 13)",18))
)

# Generate plot for mean share WfH by year for women.
gg_line_female <- ggplot(frame_annual_mean_female, aes(x=Year, y=Mean_Values, color = Group)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Mean share WfH",title = "Mean share WfH by year for women")+
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")

ggsave("gg_line_female.png",gg_line_female, dpi= dpi_n, width = 8, height = 5)

# Generate plot for mean share WfH by year for men.
gg_line_male <- ggplot(frame_annual_mean_male, aes(x=Year, y=Mean_Values, color = Group)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Mean share WfH",title = "Mean share WfH by year for men")+
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")


ggsave("gg_line_male.png",gg_line_male,dpi = dpi_n, width = 8, height = 5)

# Generate density plots for actual contractual working hours in 2002 and 2019 for women.

density_data_2019_female_wfH <- data[which(data$year == 2019 & data$sex == 1 & data$WfH == 1),c("year","conWH")]
density_data_2019_female_no_wfH <- data[which(data$year == 2019 & data$sex == 1 & data$WfH == 0),c("year","conWH")]
density_data_2002_female_wfH <- data[which(data$year == 2002 & data$sex == 1 & data$WfH == 1),c("year","conWH")]
density_data_2002_female_no_wfH <- data[which(data$year == 2002 & data$sex == 1 & data$WfH == 0),c("year","conWH")]
density_plot_female_WfH <- rbind(density_data_2019_female_wfH, density_data_2002_female_wfH)
density_plot_female_no_WfH <- rbind(density_data_2019_female_no_wfH, density_data_2002_female_no_wfH)
density_plot_female_WfH<- ggplot(density_plot_female_WfH, aes(x = conWH, fill = as.character(year))) +
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("2002" = "blue4", "2019" = "yellow3")) +
  labs(x = "Contractual working hours",
       y = "Density",
       fill = "Year", title = "Women that WfH") +
  coord_cartesian(xlim = c(0,50)) +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

density_plot_female_no_WfH <- ggplot(density_plot_female_no_WfH, aes(x = conWH, fill = as.character(year))) +
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("2002" = "blue4", "2019" = "yellow3")) +
  labs(x = "Contractual working hours",
       y = "Density",
       fill = "Year", title = "Women that do not WfH") +
  coord_cartesian(xlim = c(0,50)) +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

# ggsave("density_plot_female_no_WfH.png",density_plot_female_no_WfH, dpi = dpi_n, width = 8, height = 5)
# ggsave("density_plot_female_WfH.png", density_plot_female_WfH, dpi = dpi_n, width = 8, height = 5)

# Generate density plots for actual contractual working hours in 2002 and 2019 for men.

density_data_2019_male_wfH <- data[which(data$year == 2019 & data$sex == 0 & data$WfH == 1),c("year","conWH")]
density_data_2019_male_no_wfH <- data[which(data$year == 2019 & data$sex == 0 & data$WfH == 0),c("year","conWH")]
density_data_2002_male_wfH <- data[which(data$year == 2002 & data$sex == 0 & data$WfH == 1),c("year","conWH")]
density_data_2002_male_no_wfH <- data[which(data$year == 2002 & data$sex == 0 & data$WfH == 0),c("year","conWH")]
density_plot_male_WfH <- rbind(density_data_2019_male_wfH, density_data_2002_male_wfH)
density_plot_male_no_WfH <- rbind(density_data_2019_male_no_wfH, density_data_2002_male_no_wfH)
density_plot_male_WfH<- ggplot(density_plot_male_WfH, aes(x = conWH, fill = as.character(year))) +
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("2002" = "blue4", "2019" = "yellow3")) +
  labs(x = "Contractual working hours",
       y = "Density",
       fill = "Year", title = "Men that WfH") +
  coord_cartesian(xlim = c(0,50)) +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

density_plot_male_no_WfH <- ggplot(density_plot_male_no_WfH, aes(x = conWH, fill = as.character(year))) +
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("2002" = "blue4", "2019" = "yellow3")) +
  labs(x = "Contractual working hours",
       y = "Density",
       fill = "Year", title = "Men that do not WfH") +
  coord_cartesian(xlim = c(0,50)) +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

# ggsave("density_plot_male_no_WfH.png",density_plot_male_no_WfH, dpi = dpi_n, width = 8, height = 5)
# ggsave("density_plot_male_WfH.png", density_plot_male_WfH, dpi = dpi_n, width = 8, height = 5)


# Save the plots in a 2x2 arrangement.
density_plots <- ggarrange(density_plot_female_WfH+ rremove("xlab"),density_plot_female_no_WfH+ rremove("xlab") + rremove("ylab"),
                           density_plot_male_WfH,density_plot_male_no_WfH + rremove("ylab"), ncol = 2, nrow= 2, 
                           legend = "bottom",common.legend = TRUE)

ggsave("density_plots.png",density_plots, dpi = dpi_n, width= 8, height = 5)



# Generate a real wage by year boxplot.

wage_data <- data.frame(data$real_fte_salaries, data$year)
colnames(wage_data) <- c("real_fte_salaries","year")
wage_data <- wage_data[which(wage_data$year %in% c(2002,2005,2008,2011,2014,2017,2019)),]

wage_plot <- ggplot(wage_data, aes(x = as.factor(year),y = real_fte_salaries))+
  geom_boxplot(fill='lightblue', color="black") +
  scale_y_continuous(labels = label_number(big.mark = ))+
  labs(x = "Year",
       y = "Real wage",
       fill = "Year", title = "Boxplots of real wages by year") +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("wage_plot.png",wage_plot, dpi = dpi_n, width= 8, height = 5)



### Empirical analysis


# Add a few constructed variables such as a copy of year and experience squared.
data$experience_sq <- data$experience^2
data$age_sq <- data$age^2
data$c_year <- data$year


data$cond_sat_bin <- data$cond_sat
data$cond_sat_bin[data$cond_sat_bin %in% c(1:7)] <- 0
data$cond_sat_bin[data$cond_sat_bin %in% c(8:10)] <- 1

# Seperate the data for panel and gender.
female_data <- data[which(data$sex == 1),]
male_data <- data[which(data$sex == 0),]
female_pdata <- pdata.frame(female_data, index = c("id","year"))
male_pdata <- pdata.frame(male_data,index= c("id","year"))


# Function to choose the highest standard error (in this case it is always HC3)
rb_se <-function(model,round_n = 4){
  hc0_se <- coeftest(model, vcov = vcovHC(model, type = "HC0"), cluster = "id" )
  hc1_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"), cluster = "qwe" )
  hc2_se <- coeftest(model, vcov = vcovHC(model, type = "HC2"), cluster = "group")
  hc3_se <- coeftest(model, vcov = vcovHC(model, type = "HC3"), cluster = "group" )
  se_frame <- data.frame(hc0_se[,2], hc1_se[,2], hc2_se[,2], hc3_se[,2],  summary(model)$coefficients[,2])
  colnames(se_frame) <- c("HC0","HC1","HC2","HC3","Conventional")
  model_constel <- summary(model)$coefficients
  name_vec <- c()
  for (i in c(1:nrow(se_frame))){
    name = colnames(se_frame[which(se_frame[i,] == max(se_frame[i,]))])
    #name_vec <- append(name_vec,name)
    print(name)
    # if (name == "HC0"){
    #   model_constel[i,] <- hc0_se[i,]
    # }
    # else if (name == "HC1"){
    #   model_constel[i,] <- hc1_se[i,]
    # }
    # else if (name == "HC2"){
    #   model_constel[i,] <- hc2_se[i,]
    # }
    # else if (name == "HC3"){
    #   model_constel[i,] <- hc3_se[i,]
    # }
    # else {
    #   model_constel[i,] <- summary(model)$coefficients[i,]
    # }
    model_constel[i,] <- hc3_se[i,]
    model_constel[i,1] <- round(as.numeric(model_constel[i,1]),round_n)
    if (as.numeric(model_constel[i,4]) < 0.01){
      model_constel[i,1] <- paste0(model_constel[i,1],"***")
    }
    else if (as.numeric(model_constel[i,4]) < 0.05){
      model_constel[i,1] <- paste0(model_constel[i,1],"**")
    }
    else if (as.numeric(model_constel[i,4]) < 0.1){
      model_constel[i,1] <- paste0(model_constel[i,1],"*")
    }
  }
  return(model_constel)
  #return(coeftest(model, vcov = vcovHC(model,type = max(name_vec))))
}


# Fixed effects model for wages for women.

fe_model_wages_female <- plm(log(fte_salaries) ~ factor(WfH)+ edu_year +experience +experience_sq +factor(public_private) +
                               factor(superv)+ factor(esec) +
                               factor(n_employees)+  factor(c_year), 
                             data = female_pdata, model = "within", index = c("id"), effect = "individual")

# Fixed effects model for wages for men.

fe_model_wages_male <- plm(log(fte_salaries) ~ factor(WfH)+ edu_year +experience +experience_sq +factor(public_private) +
                             factor(superv)+ factor(esec) +
                             factor(n_employees)+  factor(c_year), 
                           data = male_pdata, model = "within", index = c("id"), effect = "individual")

# Pooled OLS model for wages for women.

ols_model_wages_female <- plm(log(fte_salaries) ~ factor(WfH)+ edu_year +experience +experience_sq +factor(public_private) +
                                factor(superv)+ factor(esec) +
                                factor(n_employees)+  factor(c_year), data = female_pdata, model = "pooling")

# Pooled OLS model for wages for men.

ols_model_wages_male <- plm(log(fte_salaries) ~ factor(WfH)+ edu_year +experience +experience_sq +factor(public_private) +
                              factor(superv)+ factor(esec) +
                              factor(n_employees)+  factor(c_year), data = male_pdata, model = "pooling")

# Extract robust standard errors using the function above.

label_wages <-  c("WfH")
fe_model_wages_female_rb <-  data.frame(rb_se(fe_model_wages_female,4))
fe_model_wages_male_rb <-  data.frame(rb_se(fe_model_wages_male,4))
ols_model_wages_female_rb <-  data.frame(rb_se(ols_model_wages_female,4))
ols_model_wages_male_rb <-  data.frame(rb_se(ols_model_wages_male,4))
robust_se_wages <- list(as.numeric(fe_model_wages_female_rb$Std..Error),as.numeric(fe_model_wages_male_rb$Std..Error), as.numeric(ols_model_wages_female_rb$Std..Error),as.numeric(ols_model_wages_male_rb$Std..Error))
p_value_wages <- list(as.numeric(fe_model_wages_female_rb$Pr...t..),as.numeric(fe_model_wages_male_rb$Pr...t..),as.numeric(ols_model_wages_female_rb$Pr...t..),as.numeric(ols_model_wages_male_rb$Pr...t..))


# Save the model using stargazer (short version).
stargazer(fe_model_wages_female,fe_model_wages_male,ols_model_wages_female,ols_model_wages_male,
          se = robust_se_wages, p = p_value_wages,
          align=TRUE, type="latex", out="table_wages.tex", covariate.labels = label_wages, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Log(Full-time equivalent salary)",model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          keep = c("WfH"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))


# If stargazer throws the error: stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2

# Run this code:
################################################################################
# ## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# # Unload stargazer if loaded
# detach("package:stargazer",unload=T)
# # Delete it
# remove.packages("stargazer")
# # Download the source
# download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# # Unpack
# untar("stargazer_5.2.3.tar.gz")
# # Read the sourcefile with .inside.bracket fun
# stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# # Move the length check 5 lines up so it precedes is.na(.)
# stargazer_src[1990] <- stargazer_src[1995]
# stargazer_src[1995] <- ""
# # Save back
# writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# # Compile and install the patched package
# install.packages("stargazer", repos = NULL, type="source")
################################################################################
# Source/Credit: https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53



label_wages <-  c("WfH","Years of eduction","Experience","Experience squared","Public sector","Supervisory role","Lower professionals","Intermediate occupations","Lower supervisors \\& technicians","Lower sales \\& service","Lower technical","Routine","N° of employees: 20-99","N° of employees: 100-499","N° of employees: 500+")

# Save the model using stargazer (long version).
stargazer(fe_model_wages_female,fe_model_wages_male,ols_model_wages_female,ols_model_wages_male,
          se = robust_se_wages, p = p_value_wages,
          align=TRUE, type="latex", out="table_wages_long.tex", covariate.labels = label_wages, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Log(Full-time equivalent salary)",model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          omit = c("c_year"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                                 paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                                 paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))



# Fixed effects model for actual working hours for women.
fe_model_actualHW_female <- plm(actualHW ~ factor(WfH)+ age +age_sq+ edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv) +
                                  factor(n_employees)+ conWH, 
                                data = female_pdata, model = "within", index = c("id"), effect = "individual")

# Fixed effects model for actual working hours for men.
fe_model_actualHW_male <- plm(actualHW ~ factor(WfH)+ age +age_sq+ edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv)  +
                                factor(n_employees)+ conWH
                              , data = male_pdata, model = "within", index = c("id"), effect = "individual")

# Pooled OLS model for actual working hours for women.
ols_model_actualHW_female <- plm(actualHW ~ factor(WfH)+ age + age_sq + edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv)  +
                                   factor(n_employees)+ 
                                   conWH, data = female_pdata, model = "pooling")

# Pooled OLS model for actual working hours for men.
ols_model_actualHW_male <- plm(actualHW ~ factor(WfH)+ age + age_sq + edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv) +
                                 factor(n_employees)+ conWH, data = male_pdata, model = "pooling")

# Extract robust standard errors using the function above.
label_actualHW <-  c("WfH")
fe_model_actualHW_female_rb <-  data.frame(rb_se(fe_model_actualHW_female,4))
fe_model_actualHW_male_rb <-  data.frame(rb_se(fe_model_actualHW_male,4))
ols_model_actualHW_female_rb <-  data.frame(rb_se(ols_model_actualHW_female,4))
ols_model_actualHW_male_rb <-  data.frame(rb_se(ols_model_actualHW_male,4))
robust_se_actualHW <- list(as.numeric(fe_model_actualHW_female_rb$Std..Error),as.numeric(fe_model_actualHW_male_rb$Std..Error), as.numeric(ols_model_actualHW_female_rb$Std..Error),as.numeric(ols_model_actualHW_male_rb$Std..Error))
p_value_actualHW <- list(as.numeric(fe_model_actualHW_female_rb$Pr...t..),as.numeric(fe_model_actualHW_male_rb$Pr...t..),as.numeric(ols_model_actualHW_female_rb$Pr...t..),as.numeric(ols_model_actualHW_male_rb$Pr...t..))

# Save the model using stargazer (short version).
stargazer(fe_model_actualHW_female,fe_model_actualHW_male,ols_model_actualHW_female,ols_model_actualHW_male,
          se = robust_se_actualHW, p = p_value_actualHW,
          align=TRUE, type="latex", out="table_actualHW.tex", covariate.labels = label_actualHW, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Actual hours worked",model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          keep = c("WfH"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))




label_actualHW <-  c("WfH","Age","Age squared","Years of education","Hourly wage","N° of children",
                     "Child aged 12 or younger","Public sector","Married","Supervisory role","N° of employees: 20-99",
                     "N° of employees: 100-499","N° of employees: 500+","Contractual working hours")

# Save the model using stargazer (long version).
stargazer(fe_model_actualHW_female,fe_model_actualHW_male,ols_model_actualHW_female,ols_model_actualHW_male,
          se = robust_se_actualHW, p = p_value_actualHW,
          align=TRUE, type="latex", out="table_actualHW_long.tex", covariate.labels = label_actualHW, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Actual hours worked",model.names = NULL,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          omit = ("xyz"),add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                            paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                            paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))


# Fixed effects model for contractual working hours for women.
fe_model_conWH_female <- plm(conWH ~ factor(WfH)+ age + age_sq+edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv) +
                               factor(n_employees)+ actualHW, 
                             data = female_pdata, model = "within", index = c("id"), effect = "individual")

# Fixed effects model for contractual working hours for men.
fe_model_conWH_male <- plm(conWH ~ factor(WfH)+ age +age_sq+ edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv)  +
                             factor(n_employees)+ actualHW
                           , data = male_pdata, model = "within", index = c("id"), effect = "individual")

# Pooled OLS model for contractual working hours for women.
ols_model_conWH_female <- plm(conWH ~ factor(WfH)+ age +age_sq+ edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv)  +
                                factor(n_employees)+ 
                                actualHW, data = female_pdata, model = "pooling")

# Pooled OLS model for contractual working hours for men.
ols_model_conWH_male <- plm(conWH ~ factor(WfH)+ age + age_sq + edu_year +hourly_wage+n_kids+factor(kid_max_12)+factor(public_private) +factor(civstat) + factor(superv) +
                              factor(n_employees)+ actualHW, data = male_pdata, model = "pooling")


# Extract robust standard errors using the function above.
label_conWH <-  c("WfH")
fe_model_conWH_female_rb <-  data.frame(rb_se(fe_model_conWH_female,4))
fe_model_conWH_male_rb <-  data.frame(rb_se(fe_model_conWH_male,4))
ols_model_conWH_female_rb <-  data.frame(rb_se(ols_model_conWH_female,4))
ols_model_conWH_male_rb <-  data.frame(rb_se(ols_model_conWH_male,4))


robust_se_conWH <- list(as.numeric(fe_model_conWH_female_rb$Std..Error),as.numeric(fe_model_conWH_male_rb$Std..Error), as.numeric(ols_model_conWH_female_rb$Std..Error),as.numeric(ols_model_conWH_male_rb$Std..Error))
p_value_conWH <- list(as.numeric(fe_model_conWH_female_rb$Pr...t..),as.numeric(fe_model_conWH_male_rb$Pr...t..),as.numeric(ols_model_conWH_female_rb$Pr...t..),as.numeric(ols_model_conWH_male_rb$Pr...t..))

# Save the model using stargazer (short version).
stargazer(fe_model_conWH_female,fe_model_conWH_male,ols_model_conWH_female,ols_model_conWH_male,
          se = robust_se_conWH, p = p_value_conWH,
          align=TRUE, type="latex", out="table_conWH.tex", covariate.labels = label_conWH, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Contractual working hours",model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          keep = c("WfH"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))




label_conWH <-  c("WfH","Age","Age squared","Years of education","Hourly wage","N° of children",
                  "Child aged 12 or younger","Public sector","Married","Supervisory role","N° of employees: 20-99",
                  "N° of employees: 100-499","N° of employees: 500+","Actual working hours")

# Save the model using stargazer (long version).
stargazer(fe_model_conWH_female,fe_model_conWH_male,ols_model_conWH_female,ols_model_conWH_male,
          se = robust_se_conWH, p = p_value_conWH,
          align=TRUE, type="latex", out="table_conWH_long.tex", covariate.labels = label_conWH, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = "Contractual working hours",model.names = NULL,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          omit = ("xyz"),add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                            paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                            paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))




# Fixed effects model for satisfaction with working conditions for women.
fe_model_cond_sat_female <- plm(cond_sat ~  factor(WfH)+  edu_year + conWH + hourly_wage+factor(kid_max_12)+ commute_m  + factor(superv),
                                data = female_pdata, model = "within", index = c("id"), effect = "individual")

# Fixed effects model for satisfaction with working conditions for men.

fe_model_cond_sat_male <- plm(cond_sat ~  factor(WfH)+  edu_year + conWH + hourly_wage+factor(kid_max_12)+ commute_m  + factor(superv),
                              data = male_pdata, model = "within", index = c("id"), effect = "individual")

# Pooled OLS model for satisfaction with working conditions for women.
ols_model_cond_sat_female <- plm(cond_sat ~  factor(WfH)+  edu_year + conWH + hourly_wage+factor(kid_max_12)+ commute_m  + factor(superv),
                                 data = female_data, model = "pooling")

# Pooled OLS model for satisfaction with working conditions for men.
ols_model_cond_sat_male <- plm(cond_sat ~  factor(WfH)+  edu_year + conWH + hourly_wage+factor(kid_max_12)+ commute_m  + factor(superv),
                               data = male_pdata, model = "pooling")


ols_model_cond_sat_male_bin <- plm(cond_sat_bin ~  factor(WfH)+  edu_year + conWH + hourly_wage+factor(kid_max_12)+ commute_m  + factor(superv),
                                   data = male_pdata, model = "pooling")


# Extract robust standard errors using the function above.
label_cond_sat <-  c("WfH")
fe_model_cond_sat_female_rb <-  data.frame(rb_se(fe_model_cond_sat_female,4))
fe_model_cond_sat_male_rb <-  data.frame(rb_se(fe_model_cond_sat_male,4))
ols_model_cond_sat_female_rb <-  data.frame(rb_se(ols_model_cond_sat_female,4))
ols_model_cond_sat_male_rb <-  data.frame(rb_se(ols_model_cond_sat_male,4))
robust_se_cond_sat <- list(as.numeric(fe_model_cond_sat_female_rb$Std..Error),as.numeric(fe_model_cond_sat_male_rb$Std..Error), as.numeric(ols_model_cond_sat_female_rb$Std..Error),as.numeric(ols_model_cond_sat_male_rb$Std..Error))
p_value_cond_sat <- list(as.numeric(fe_model_cond_sat_female_rb$Pr...t..),as.numeric(fe_model_cond_sat_male_rb$Pr...t..),as.numeric(ols_model_cond_sat_female_rb$Pr...t..),as.numeric(ols_model_cond_sat_male_rb$Pr...t..))

# Save the model using stargazer (short version).
stargazer(fe_model_cond_sat_female,fe_model_cond_sat_male,ols_model_cond_sat_female,ols_model_cond_sat_male,
          se = robust_se_cond_sat, p = p_value_cond_sat,
          align=TRUE, type="latex", out="table_cond_sat.tex", covariate.labels = label_cond_sat, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = c("Satisfaction with work conditions"),model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          keep = c("WfH"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))


label_cond_sat <-  c("WfH","Years of education","Contractual working hours","Hourly wage","Child aged 12 or younger","Minutes of commute","Supvervisory role")

# Save the model using stargazer (long version).
stargazer(fe_model_cond_sat_female,fe_model_cond_sat_male,ols_model_cond_sat_female,ols_model_cond_sat_male,
          se = robust_se_cond_sat, p = p_value_cond_sat,
          align=TRUE, type="latex", out="table_cond_sat_long.tex", covariate.labels = label_cond_sat, 
          column.labels = c("FE Female","FE Male","OLS Female","OLS Male"),colnames = FALSE, dep.var.labels = c("Satisfaction with work condition"),model.names = FALSE,
          dep.var.caption = "", omit.stat = c("f","ser"), notes.label = "", model.numbers =FALSE, float = FALSE,
          omit = c("xyz"), add.lines = list(c("No. of individuals", paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"), 
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"),paste0("\\multicolumn{1}{c}{",length(unique(female_data$id)),"}"),
                                              paste0("\\multicolumn{1}{c}{",length(unique(male_data$id)),"}"))))



# Define model for WfH. 
model_logit <- WfH ~ factor(kid_max_12) +n_kids + superv +commute_m  + conWH+ actualHW +hourly_wage + age + factor(year)

# Fixed effects logistic regression for WfH for women.
fe_logit_model_wfh_female <- feglm(WfH ~ factor(kid_max_12) +n_kids+ superv+ commute_m  + conWH+ actualHW +hourly_wage + age + factor(year) | id, data = female_pdata, family = binomial(link = "logit"))

# Fixed effects logistic regression for WfH for men.
fe_logit_model_wfh_male <- feglm(WfH ~ factor(kid_max_12)+n_kids + superv+ commute_m  + conWH+ actualHW +hourly_wage + age + factor(year) | id, data = male_pdata, family = binomial(link = "logit"))

# Logit model for WfH for women.
pooled_logit_model_wfh_female <- feglm(model_logit, data = female_pdata, family = binomial(link = "logit"))

# Logit model for WfH for men.
pooled_logit_model_wfh_male <- feglm(model_logit, data = male_pdata, family = binomial(link = "logit"))


logit_models <- list()
logit_models[['FE Female']] <- fe_logit_model_wfh_female
logit_models[['FE Male']] <- fe_logit_model_wfh_male
logit_models[['Pooled Female']] <- pooled_logit_model_wfh_female
logit_models[['Pooled Male']] <- pooled_logit_model_wfh_male

rename_v <- c("factor(kid_max_12)1" = "Child aged 12 or younger",
              "superv" = "Supvervisory Role",
              "n_kids" = "Number of children",
              "commute_m"="Minutes of commute",
              "conWH" = "Contractual working hours",
              "actualHW" = "Actual hours worked",
              "hourly_wage" = "Hourly wage",
              "age"="Age",
              "(Intercept)"="Intercept")

# Save via model summary and adjust latex code. (stargazer does not support
# fixest objects)
out <- capture.output(modelsummary(logit_models, output = "latex", stars = c('***' = 0.01,'**' = 0.05, '*' = 0.1) ,coef_rename = rename_v, coef_omit = "year",
                                 gof_omit = "AIC|BIC|RMSE|Std.Errors|FE: id"))

start <- grep("^\\\\begin\\{talltblr\\}", out)
stop <- grep("^\\\\end\\{talltblr\\}", out)
out <- out[start:stop]
out[3] ="note{} = {* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01},"

lines <- unlist(strsplit(out,"\n"))
mod_table <- paste(lines, collapse = "\n")

write(mod_table,"logit_table.tex")



# Functions to calculate probabilities for the logit model, with the baseline values
# defined as the rounded means.
female_averages <- data.frame(
  age = 45,
  commute_m = 49,
  hourly_wage = 44,
  conWH = 29,
  actualHW = 31,
  n_kids = 1,
  superv = 0,
  kid_max_12 = 0,
  year = 2002
)

logit_pred_female <- predict(pooled_logit_model_wfh_female, female_averages, type = "link")
prob_pred_female <- predict(pooled_logit_model_wfh_female, female_averages, type = "response")


male_averages <- data.frame(
  age = 46,
  commute_m = 53,
  hourly_wage = 53,
  conWH = 40,
  actualHW = 43,
  n_kids = 2,
  superv = 0,
  kid_max_12 = 0,
  year = 2002
)

logit_pred_male <- predict(pooled_logit_model_wfh_male, male_averages, type = "link")
prob_pred_male <- predict(pooled_logit_model_wfh_male, male_averages, type = "response")


