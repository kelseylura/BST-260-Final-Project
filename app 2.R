#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(maps)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(rvest)
library(shinythemes)
library(ggthemes)

# FIRST TAB

# Read in CDC Disability and Health Data System (DHDS) for 2018
# Source: https://data.cdc.gov/Disability-Health/DHDS-Prevalence-of-Disability-Status-and-Types-by-/qjg3-6acf
disability <- read_csv("us_disabilities_2018.csv")

us_states <- map_data("state")

# Wrangling code
disability_clean <- disability[c(2,3,6,7,10,19)] 
disability_clean$LocationDesc <- tolower(disability_clean$LocationDesc)
disability_clean <- disability_clean %>% 
  filter(Stratification1=="Any Disability") %>% 
  filter(!grepl("HHS", LocationAbbr)) %>% 
  filter(!grepl("GU", LocationAbbr)) %>% 
  filter(!grepl("PR", LocationAbbr)) %>% 
  filter(!grepl("US", LocationAbbr))

disability_map <- left_join(us_states, disability_clean, by = c("region" = "LocationDesc"))

#-----------------------SETTING COLOR KEYS:

# GENDER
disability_map <- disability_map %>% mutate(manual_fill_gender = cut(Data_Value, 
                                                                     breaks = c(-Inf, 20, 25.1, 30.1, 35, Inf),
                                                                     labels = c("<20", "20-25", "25.1-30", "30.1-35", ">35"), 
                                                                     right = TRUE))

gender <- c("lightskyblue2", "dodgerblue3", "dodgerblue4", "red", "red4")



# RACE/ETHNICITY
disability_map <- disability_map %>% mutate(manual_fill_race = cut(Data_Value, 
                                                                   breaks = c(-Inf, 10, 20.1, 30.1, 40, Inf),
                                                                   labels = c("<10", "10-20", "20.1-30", "30.1-40", ">40"), 
                                                                   right = TRUE))
race <- c("lightskyblue2", "dodgerblue3", "dodgerblue4", "red", "red4")



# AGE (12.9-62.8)
disability_map <- disability_map %>% mutate(manual_fill_age = cut(Data_Value, 
                                                                  breaks = c(-Inf, 15, 30.1, 45.1, 60, Inf),
                                                                  labels = c("<15", "15-30", "30.1-45", "45.1-60", ">60"), 
                                                                  right = TRUE))
age <- c("lightskyblue2", "dodgerblue3", "dodgerblue4", "red", "red4")



# VETERAN STATUS (26.3-33.1)
disability_map <- disability_map %>% mutate(manual_fill_vet = cut(Data_Value, 
                                                                  breaks = c(-Inf, 27, 28.1, 29.1, 30, Inf),
                                                                  labels = c("<27", "27.1-28", "28.1-29", "29.1-30", ">30"), 
                                                                  right = TRUE))
vet <- c("lightskyblue2", "dodgerblue3", "dodgerblue4", "red", "red4")



#-----------------------GROUPED GRAPHS:

# GENDER
gender_plot <- disability_map %>% 
  filter(Response %in% c("Male", "Female")) %>% 
  ggplot(aes(x = long, y = lat, fill=Data_Value, group=group)) + 
  geom_polygon(aes(fill=manual_fill_gender), color = "white") + 
  coord_fixed(1.3) + 
  scale_fill_manual(name = "Prevalence (%)", values = gender, drop = F) + 
  ggtitle("Disability Prevalence by Gender") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) + facet_grid(~ Response)

# RACE
race_names <- c(
  "White, non-Hispanic" = "White",
  "Black, non-Hispanic" = "Black",
  "Hispanic" = "Hispanic",
  "Asian, non-Hispanic" = "Asian",            
  "American Indian or Alaska Native, non-Hispanic" = "Native American or Alaskan",
  "Other / Multirace, non-Hispanic" = "Other/Multirace")

race_plot <- disability_map %>% filter(Response %in% c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Asian, non-Hispanic", "American Indian or Alaska Native, non-Hispanic", "Other / Multirace, non-Hispanic")) %>% 
  ggplot(aes(x = long, y = lat, fill=Data_Value, group=group)) + 
  geom_polygon(aes(fill=manual_fill_race), color = "black") + 
  coord_fixed(1.3) + 
  scale_fill_manual(name = "Prevalence (%)", values = race, drop = F) + 
  ggtitle("Disability Prevalence by Race/Ethnicity") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
  labs(caption= "*Disability data for Native Hawaiian or Other Pacific Islander not shown due to lack of data") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) + facet_wrap(~ Response, labeller=as_labeller(race_names))

# AGE
age_plot <- disability_map %>% filter(Response %in% c("18-44", "45-64", "65+")) %>% 
  ggplot(aes(x = long, y = lat, fill=Data_Value, group=group)) + 
  geom_polygon(aes(fill=manual_fill_age), color = "white") + 
  coord_fixed(1.3) + 
  scale_fill_manual(name = "Prevalence (%)", values = age, drop = F) + 
  ggtitle("Disability Prevalence by Age") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) + facet_grid(~ Response)


# VETERAN STATUS
vet_plot <- disability_map %>% filter(Response %in% c("Non-Veteran", "Veteran")) %>% 
  ggplot(aes(x = long, y = lat, fill=Data_Value, group=group)) + 
  geom_polygon(aes(fill=manual_fill_vet), color = "white") + 
  coord_fixed(1.3) + 
  scale_fill_manual(name = "Prevalence (%)", values = vet, drop = F) + 
  ggtitle("Disability Prevalence by Veteran Status") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) + facet_grid(~ Response)

# SECOND TAB

# Web Scraping

url <- "https://disabilitystatistics.org/reports/2018/English/HTML/report2018.cfm?fips=2000000&html_year=2018&subButton=Get+HTML#emp-state"
web_scrap <- read_html(url)
tables <- web_scrap %>% html_nodes("table")

# Table 3
# Web Scraping Table "Employment of non-institutionalized working-age people (ages 21 to 64) by disability status in the United States in 2018"

table_3 <- tables %>% html_table(fill = TRUE) %>% .[[13]] 
table_3

table_3 <- table_3 %>% setNames(c("disability_type", "percent_employed", "MOE", 
                                  "number", "MOE_2", "base_pop", "sample_size")) %>%
    select(disability_type, percent_employed)

# Table 4
# Web scraping table: "Median annual income* of households including any working-age people (ages 21 to 64) by disability status in the United States in 2018"

table_4 <- tables %>% html_table(fill = TRUE) %>% .[[17]] 

table_4 <- table_4 %>% setNames(c("disability_type", "median_hh_income", "MOE", 
                                  "base_pop", "sample_size")) %>%
    select(disability_type, median_hh_income)

# Table 5
# Web scraping table: "Median annual earnings of non-institutionalized working-age people (ages 21 to 64) who work full-time/full-year by disability status in the United States in 2018"

table_5 <- tables %>% html_table(fill = TRUE) %>% .[[16]] 
table_5 <- table_5 %>% setNames(c("disability_type", "median_earnings", "MOE", 
                                  "base_pop", "sample_size")) %>%
    select(disability_type, median_earnings)

# Table 6
# Web scraping table: "Percentage of non-institutionalized working-age people (ages 21 to 64) with a Bachelor's degree or more by disability status in the United States in 2018"

table_6 <- tables %>% html_table(fill = TRUE) %>% .[[22]] 
table_6 <- table_6 %>% setNames(c("disability_type", "percent_bachelor", "MOE", "number", "MOE", 
                                  "base_pop", "sample_size")) %>%
    select(disability_type, percent_bachelor)

# Joining tables
table_join_1 <- full_join(table_3, table_4, by = "disability_type")
table_join_2 <- full_join(table_join_1, table_5, by = "disability_type")
table_final <- full_join(table_join_2, table_6, by = "disability_type")

table_final$disability_type <- factor(table_final$disability_type, 
                                      levels = c("No Disability", "Any Disability", 
                                                 "Visual", "Hearing", "Ambulatory", 
                                                 "Cognitive", "Self-Care", "Independent Living"))


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  #Application title
  titlePanel("Exploration of Disability Status in the U.S. by Sociodemographic Factors, 2018"),
  
  tabsetPanel(
    # First tab
    tabPanel("Prevalence by Demographic Group and State",
        # First row: interactive elements 
        fluidRow(
            # first column: explanatory text 
            column(7,
                   p("According to the", a("CDC,", href="https://www.cdc.gov/ncbddd/disabilityandhealth/infographic-disability-impacts-all.html"), "61 million, or roughly 1 in 4, adults in the U.S. live with a disability.
                   However, the percentage of people living with disabilities is not spread evenly across the country. Additionally, disability disproportionately affects certain demographic groups, especially women, elderly, and Non-Hispanic American Indians and Alaska Natives.
                   Identifying geographic and sociodemographic disparities in disability status is critical to targeting resources to those who need it most.
                   To explore the geographical distribution of disabilities by demographic group, select a variable of interest to the right:")
            ), # end of first column
            
            # second column: radio button to select demographic factor
            column(5,
                   radioButtons(inputId = "buttons", label = "Select a Demographic Factor:",
                                choices=list("Gender"="1", "Race/Ethnicity"="2", "Age"="3", "Veteran Status"="4"),
                                selected=NULL)
            ), # end of second column
            
        ), # end of fluidRow 
        
    # map plot output
    fluidRow(
        plotOutput("disability_plots")
    ), # end of fluidRow
                     
), # end of first tab
  
    # Second tab
    tabPanel("Prevalence by Type and Socioeconomic Status",
        # First row: employment and education
        fluidRow(
          column(10, 
                 p("\nThe data used to create these plots was obtained from a number of spreadsheets 
            available from", a("The 2018 Annual Disability Status Report", href="https://disabilitystatistics.org/reports/2018/English/HTML/report2018.cfm?fips=2000000&html_year=2018&subButton=Get+HTML#emp-state"), ".
            Here, we explore socioeconomic status (SES) disparities by disability type across the working-age population in the U.S.
            Investigation of socioeconomic factors of people without disabilities and across disability types is crucial to gaining a better understanding of the employment gap between individuals with and without disabilities. 
            Studying disparities in SES is important to promoting social integration and economic self-sufficiency among working-age individuals with disabilities. 
            Use the selections below to explore how disability status and type differ by various socioeconomic status metrics.")
                 )      
          ), #end of fluidRow
        
        fluidRow(
          column(3,
                 radioButtons("stat",
                              "Choose Statistic to Display:",
                              choiceNames = c("Percent Employed",
                                              "Percent with Bachelors Degree or Higher"),
                              choiceValues = c("percent_employed",
                                               "percent_bachelor"))),
                column(7,
                    plotOutput("bar_graph"))
        ), # end of fluidRow
        
        # Second row: income
        fluidRow(
                column(3,
                       radioButtons("stat_2",
                                          "Choose Statistic to Display:",
                                          choiceNames = c("Median Annual Household Income",
                                                          "Median Annual Earnings"),
                                          choiceValues = c("median_hh_income",
                                                           "median_earnings"))
                             
                ),
                column(7,
                       plotOutput("bar_graph_2")),
            
        ) # end of fluidRow
        
), # end of second tab 

    # Third tab 
    tabPanel("Prevalence by Type and Age: Regression Analysis",
        fluidRow(
            column(6,
                   p("There are statistically significant differences of functional disability prevalence across age groups. To investigate the statistical associations between disability prevalence and age group, select the disability type of interest to the right and view the corresponding regression analysis below:")
            ), # end of first column
            
            column(6,
                   selectInput("disability", "Select Type of Disability for Regression Analysis:", c("Any Disability", "No Disability", "Hearing Disability", "Vision Disability", "Self-care Disability", "Cognitive Disability"), multiple = FALSE, selectize = FALSE, size = 1),
            ), # end of second column
            
        ), # end of fluidRow
        
        fluidRow(
            column(12,
                htmlOutput("RegAnalysisText"),
                tableOutput("RegAnalysis"),
                htmlOutput("RegTableText"),
                tableOutput("RegTable"),
                plotOutput("BoxPlot")
        ),
        ), # end fluidRow
        
) # end of third tab

)) # end of fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

    # heat maps   
    output$disability_plots <- renderPlot({
        switch(input$buttons,
           "1"=plot(gender_plot),
           "2"=plot(race_plot),
           "3"=plot(age_plot),
           "4"=plot(vet_plot)
    )
  })
    # bar graphs 
    output$bar_graph <- renderPlot({
        
        if (input$stat == "percent_employed") {
            
            table_final %>%
                ggplot() +
                geom_bar(aes(disability_type, percent_employed), stat = "identity", fill = "dark blue")+
                theme_bw() +
                ggtitle("Employment of People, Ages 21-64, by Disability Status") +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
                xlab("\nDisability Type")+
                ylab("Percent Employed\n")+
                coord_cartesian(ylim = c(0, 100)) +
                scale_x_discrete(labels = c("No Disability", "Any \nDisability", "Visual", "Hearing", "Ambulatory", "Cognitive", 
                                            "Self-Care", "Independent \nLiving"))
            
        } else {
            
            table_final %>%
                ggplot() +
                geom_bar(aes(disability_type, percent_bachelor), stat = "identity", fill = "purple")+
                theme_bw() +
                ggtitle("Percent of People, Ages 21-64, \nwith a Bachelor's Degree or More, \nby Disability Status") +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
                xlab("\nDisability Type")+
                ylab("Percent with Degree" )+
                coord_cartesian(ylim = c(0, 100)) +
                scale_x_discrete(labels = c("No Disability", "Any \nDisability", "Visual", "Hearing", 
                                            "Ambulatory", "Cognitive", 
                                            "Self-Care", "Independent \nLiving")) }
    })
    
    
    output$bar_graph_2 <- renderPlot({
        
        if (input$stat_2 == "median_hh_income") {
            
            table_final %>%
                ggplot() +
                geom_bar(aes(disability_type, median_hh_income), stat = "identity", fill = "dark green")+
                theme_bw() +
                ggtitle("Median Annual Houshold Income of Households \nIncluding Anyone Ages 21-64, \nby Disability Status") +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
                xlab("\nDisability Type")+
                ylab("Median Household Income (in dollars)\n")+
                scale_x_discrete(labels = c("No Disability", "Any \nDisability", "Visual", "Hearing", "Ambulatory", "Cognitive", 
                                            "Self-Care", "Independent \nLiving"))
            
        } else {
            
            table_final %>%
                ggplot() +
                geom_bar(aes(disability_type, median_earnings), stat = "identity", fill = "orange")+
                theme_bw() +
                ggtitle("Median Annual Earnings of People, Ages 21-64, \nby Disability Status") +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13))) +
                xlab("\nDisability Type")+
                ylab("Median Earnings (in dollars)" )+
                scale_x_discrete(labels = c("No Disability", "Any \nDisability", "Visual", "Hearing", 
                                            "Ambulatory", "Cognitive", 
                                            "Self-Care", "Independent \nLiving")) 
        }
    })
    
    #Box Plot Output
    output$BoxPlot = renderPlot({
        unwanted = c("HHS1", "HHS2", "HHS3", "HHS4", "HHS5", "HHS6", "HHS7", "HHS8", "HHS9", "HHS10", "US")
        cdc <- disability %>% 
            filter(Indicator == "Disability status and types among adults 18 years of age or older by age group") %>%
            filter((LocationAbbr %in% unwanted) == FALSE) %>%
            filter(Stratification1 == input$disability)
        
        cdc %>%
            ggplot() + 
            xlab("Age Group") +
            ylab("Prevalence (%)") +
            geom_boxplot(aes(Response, Data_Value, group = Response)) + 
            ggtitle(paste0( "Prevalence of ", input$disability, " in U.S. States by Age Group")) +
                theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (13)))
    })
    
    #Regression Table Text
    output$RegTableText <- renderUI({
        str1 <- paste0("Regression Results (Coeffecient Details) for Prevalence of ", input$disability, " in U.S. States by Age Group")
        HTML(paste("<b>", str1, "</b>"))
    })
    
    # Regression Table Output
    output$RegTable = renderTable({
        unwanted = c("HHS1", "HHS2", "HHS3", "HHS4", "HHS5", "HHS6", "HHS7", "HHS8", "HHS9", "HHS10", "US")
        cdc <- disability %>% 
            filter(Indicator == "Disability status and types among adults 18 years of age or older by age group") %>%
            filter((LocationAbbr %in% unwanted) == FALSE) %>%
            filter(Stratification1 == input$disability)
        
        log_reg <- glm(Data_Value ~ Response, data=cdc)
        cdc_sum = summary(log_reg)
        cdc_co1 = cdc_sum$coefficients
        Group = c("18-44", "45-64", "65+")
        cdc_co = cbind(Group, cdc_co1)
        cdc_co
    })
    
    #Regression Analysis Text
    output$RegAnalysisText <- renderUI({
        str2 <- paste0("Regression Analysis of the Prevalence of ", input$disability, " in U.S. States by Age Group")
        HTML(paste("<b>", str2, "</b>"))
    })
    
    # Regression Analysis Output
    output$RegAnalysis = renderTable({
        unwanted = c("HHS1", "HHS2", "HHS3", "HHS4", "HHS5", "HHS6", "HHS7", "HHS8", "HHS9", "HHS10", "US")
        cdc <- disability %>% 
            filter(Indicator == "Disability status and types among adults 18 years of age or older by age group") %>%
            filter((LocationAbbr %in% unwanted) == FALSE) %>%
            filter(Stratification1 == input$disability)
        
        log_reg <- glm(Data_Value ~ Response, data=cdc)
        cdc_sum = summary(log_reg)
        cdc_co1 = cdc_sum$coefficients
        Group = c("18-44", "45-64", "65+")
        cdc_co = as.data.frame(cbind(Group, cdc_co1))
        cdc_analysis <- cdc_co %>%
            filter(Group != "18-44")
        names(cdc_analysis)[names(cdc_analysis) == "Pr(>|t|)"] <- "P_Value"
        cdc_analysis <- cdc_analysis %>% mutate(Analysis = cut(as.numeric(P_Value), breaks = c(-Inf, 0.05, Inf), labels = c("There is a statistically significant difference between this age group and the 18-44 age group (p-value < 0.05).", "There is NOT a statistically significant difference between this age group and the 18-44 age group (p-value >= 0.05)."), right = TRUE)) %>%
            select(Group, Analysis)
        cdc_analysis
    })
    
} # end function

# Run the application 
shinyApp(ui = ui, server = server)
