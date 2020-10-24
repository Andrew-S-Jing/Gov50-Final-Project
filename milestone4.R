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
library(janitor)

#read_csv()

seda_district_cs <- read_csv("raw_data/stanford/seda_geodist_long_cs_v30.csv",
                         col_types = cols(.default = col_number(),
                                          leanm = col_character(),
                                          stateabb = col_character(),
                                          subject = col_character())) %>% 
    clean_names()
seda_district_cov <- read_csv("raw_data/stanford/SEDA_cov_geodist_long_v30.csv",
                              col_types = cols(.default = col_number(),
                                               leaname = col_character(),
                                               stateabb = col_character(),
                                               gslo = col_character())) %>% 
    clean_names()



#object setup

seda_district_cov_sesall <- seda_district_cov %>%
    filter(year == 2016 & grade == 8) %>% 
    select(leaid, sesall) %>% 
    arrange(sesall) %>% 
    mutate(sesall_rank = 1:n()) %>% 
    filter(sesall_rank <= 0.15 * n())

seda_district_math_scores <- seda_district_cs %>% 
    filter(year == 2016 & grade == 8 & subject == "math") %>% 
    select(leaid_c, leanm, mn_all)

seda_district_ela_scores <- seda_district_cs %>% 
    filter(year == 2016 & grade == 8 & subject == "ela") %>% 
    select(leaid_c, mn_all)

temp_ljoin_1 <- left_join(seda_district_cov_sesall, seda_district_math_scores, by = c("leaid" = "leaid_c")) 
seda_ses_vs_cs_scores <- left_join(temp_ljoin_1, seda_district_ela_scores, by = c("leaid" = "leaid_c"))








ui <- fluidPage(navbarPage(
    "Shiny Example",
    tabPanel(
        "Main",
        mainPanel(
            "Data on United States School District Educational Equity:",
            plotOutput("seda_cs_distribution_plot"),
            plotOutput("seda_cs_wbg_plot"),
            plotOutput("seda_cs_mfg_plot"),
            plotOutput("seda_ses_vs_math_plot"),
            plotOutput("seda_ses_vs_ela_plot")
    
        )
    ),
    tabPanel("About",
             
             
             h3("Milestone #5 Update"),
             p("Currently, I'm exploring data about the bottom 15% of socioeconomic status (SES) of school districts in the U.S.
               Hopefully, I can find some correlational (or even causal) factors that may bolster Math/ELA test scores despite 
               the SES disadvantage, in hopes of finding specific problems districts can tackle that would in turn bolster
               academic performance. I plan on doing this by looking at the difference in covariates between the high-performing districts
               in the bottom 15% SES vs low-performing districts in the bottom 15% SES."),
             
             
             h3("Archive: Milestone #4 Update"),
             p("My goal for this project is to look at education equity gaps studied by the SEDA,
               and hopefully find some cause of why some districts' gaps are less intense than others'.
               Basically, are there certain underlying trends that school districts could pour resources
               into to lessen gaps in their education systems? plan on evaluating many of the covariate
               candidates by parsing through the Opportunity Insights data I've also downloaded.
               However, there will be a couple challenges."),
             p("1. The Opportunity Insights data is not formatted simply, and I will have to figure out
                  how to read_csv() the data into an object in the first place."),
             p("2. The Opportunity Insights data uses census tracts as its observations, rather than
                  school districts, like Stanford does. I will need to figure out a way to map or approximate
                  census tract data onto school district data. If this proves to be entirely messy,
                  I will have to find other datasets."),
             p("Currently, my plan is to do and solve everything above, but this is subject to change,
               especially because I first need to figure out whether finding influential covariates or
               mapping tracts onto districts are plausible tasks.")
             
             )
)
)
server <- function(input, output, session) {
    output$seda_cs_distribution_plot <- renderPlot({
        seda_district_cs %>% 
            filter(year == 2016 & grade == 8) %>% 
            group_by(leaid_c, subject) %>% 
            summarize(cs_scores = mean(mn_all, na.rm = TRUE), .groups = "drop") %>%
            ggplot(aes(x = cs_scores, y = ..count.. / sum(..count..))) +
                geom_histogram(binwidth = 0.02) +
                labs(title = "Score on Cohort Scale (School District Compared with National Average)",
                     x = "Cohort Scale Score", y = "Distribution",
                     subtitle = "8th Grade Cohort of 2016",
                     caption = "Source: The Stanford Education Data Archive") +
                theme_bw() +
                geom_vline(xintercept = 0)
    })
    output$seda_cs_wbg_plot <- renderPlot({
        seda_district_cs %>% 
            filter(year == 2016 & grade == 8) %>% 
            group_by(leaid_c, subject) %>% 
            summarize(mean_white_black_gap = mean(mn_wbg, na.rm = TRUE), .groups = "drop") %>% 
            ggplot(aes(x = mean_white_black_gap)) +
                geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.02) +
                geom_vline(xintercept = 0) +
                theme_bw() +
                labs(title = "Distribution of the White-Black Gap Among School Districts",
                     subtitle = "8th Grade Cohort of 2016",
                     x = "Mean White CS Score - Mean Black CS Score", y = "Distribution",
                     caption = "Source: The Stanford Education Data Archive")
    })
    output$seda_cs_mfg_plot <- renderPlot({
        seda_district_cs %>%
            filter(year == 2016 & grade == 8) %>%
            group_by(leaid_c, subject) %>%
            summarize(mean_male_female_gap = mean(mn_mfg, na.rm = TRUE), .groups = "drop") %>%
            ggplot(aes(x = mean_male_female_gap)) +
                geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.02) +
                geom_vline(xintercept = 0) +
                theme_bw() +
                labs(title = "Distribution of the Male-Female Gap Among School Districts",
                     subtitle = "8th Grade Cohort of 2016",
                     x = "Mean Male CS Score - Mean Female CS Score", y = "Distribution",
                     caption = "Source: The Stanford Education Data Archive")
    })
    output$seda_ses_vs_math_plot <- renderPlot({
        seda_ses_vs_cs_scores %>% 
            drop_na() %>% 
            ggplot(aes(x = mn_all.x)) +
            geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.05) +
            labs(title = "Math performance of 2016 8th graders by school district",
                 subtitle = "From school districts in the bottom 15% of socioeconomic ranking in the U.S.",
                 x = "Math CS Scale Score", y = "Percent of School Districts") +
            theme_bw() +
            geom_vline(xintercept = 0)
    })
    output$seda_ses_vs_ela_plot <- renderPlot({
        seda_ses_vs_cs_scores %>% 
            drop_na() %>% 
            ggplot(aes(x = mn_all.y)) +
            geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.05) +
            labs(title = "ELA performance of 2016 8th graders by school district",
                 subtitle = "From school districts in the bottom 15% of socioeconomic ranking in the U.S.",
                 x = "ELA CS Scale Score", y = "Percent of School Districts") +
            theme_bw() +
            geom_vline(xintercept = 0)
    })
        

}
shinyApp(ui, server)