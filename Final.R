library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(usmap)
library(tidymodels)
library(rstanarm)
library(RColorBrewer)



state_policy <- read_csv("used_data/state_policy_list.csv") %>% 
  mutate(merit_pay = factor(merit_pay, levels = c("No Infrastructure", "Encouraged", "Required"))) %>% 
  mutate(need_pay = factor(need_pay, levels = c("No", "Yes"))) %>% 
  mutate(merit_tenure = factor(merit_tenure, levels = c("Not Required", "Partially Required", "Proof Required", "No State Tenure")))

growth_scores <- read_csv("raw_data/stanford/seda_geodist_pool_cs_v30.csv", col_types = cols(leaidC = col_double())) %>% 
  select(leaidC, fips, leanm, stateabb, subgroup, gap_est, mn_grd_ol, mn_grd_ol_se)
demographics <- read_csv("raw_data/stanford/SEDA_cov_geodist_pool_v30.csv") %>% 
  select(leaid, totenrl, perasn, perhsp, perblk, perwht, perecd)



policy_names <- c("Merit Pay", "Need Pay", "Merit Tenure")
gap_names <- c("Econ. disadvantaged vs. all students",
               "Econ. disadvantaged vs. not students",
               "Black vs. all students",
               "Black vs. white students")


equity <- inner_join(growth_scores, demographics, by = c("leaidC" = "leaid")) %>% 
  rename(state = stateabb) %>% 
  filter(gap_est == 0) %>% 
  filter(subgroup != "fem" & subgroup != "mal" & subgroup != "nam") %>% 
  mutate(subgroup_count = case_when(subgroup == "all" ~ totenrl,
                                    subgroup == "asn" ~ totenrl * perasn,
                                    subgroup == "blk" ~ totenrl * perblk,
                                    subgroup == "ecd" ~ totenrl * perecd,
                                    subgroup == "hsp" ~ totenrl * perhsp,
                                    subgroup == "wht" ~ totenrl * perwht,
                                    subgroup == "nec" ~ totenrl * (1 - perecd))) %>% 
  select(-perasn, -perhsp, -perblk, -perwht, -perecd) %>% 
  group_by(state, fips, subgroup) %>% 
  summarize(weighted_mn_grd_ol = weighted.mean(x = mn_grd_ol, y = totenrl * perecd, na.rm = TRUE), .groups = "drop") %>% 
  # This circumvents the need for individuals' growth scores because I care
  # about state-wide subgroup gaps, not geographical district-wide gaps. The
  # weighted average allows me to get the state-wide gap from district-wide
  # data.
  # mn_grd_ol is an estimate of average growth scores, and this dataset
  # categorizes based on subgroup.
  filter(!is.na(weighted_mn_grd_ol)) %>% 
  pivot_wider(names_from = subgroup, values_from = weighted_mn_grd_ol) %>% 
  mutate(all_black = all - blk,
         white_black = wht - blk,
         all_ecd = all - ecd,
         nec_ecd = nec - ecd) %>% 
  select(-asn, -blk, -ecd, -hsp, -nec, -wht) %>% 
  rename(avg_growth_scores = all)


policy_vs_equity <- inner_join(state_policy, equity, by = c("state_abb" = "state"))



#POLICY MAPS
map_merit_tenure <- plot_usmap(data = policy_vs_equity, values = "merit_tenure") +
  scale_fill_manual(name = "Merit Tenure", values = c("#FF9593", "#FFCB5C", "#ACDC6E", "#f7f7f7"))+ 
  labs(title = "Merit Tenure by State",
       subtitle = "Requirement of proof of teacher effectiveness when evaluating teachers for tenure")
map_merit_pay <- plot_usmap(data = policy_vs_equity, values = "merit_pay") +
  scale_fill_manual(name = "Merit Pay", values = c("#FF9593", "#FFCB5C", "#ACDC6E", "#f7f7f7")) +
  labs(title = "Merit Pay by State",
       subtitle = "Consideration of teacher performance when determining teacher pay")
map_need_pay <- plot_usmap(data = policy_vs_equity, values = "need_pay") +
  scale_fill_manual(name = "Need Pay", values = c("#FF9593", "#ACDC6E", "#f7f7f7")) +
  labs(title = "Need Pay by State",
       subtitle = "Incentive for teaching in high-need schools through differential teacher pay")

#INEQUITY MAPS
map_all_ecd <- plot_usmap(data = policy_vs_equity, values = "all_ecd") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "ECD-all gap", guide = "legend", breaks = c(-0.02, 0, 0.02, 0.04), labels = c("More Equitable", "Neutral", "More Inequitable", "Most Inequitable")) +
  labs(title = "ECD-All Inequity by State",
       subtitle = "Inequitable school systems benefit ECD students less than the state average")
map_nec_ecd <- plot_usmap(data = policy_vs_equity, values = "nec_ecd") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "ECD-NEC gap", guide = "legend", breaks = c(-0.02, 0, 0.02, 0.04), labels = c("More Equitable", "Neutral", "More Inequitable", "Most Inequitable")) +
  labs(title = "ECD-All Inequity by State",
       subtitle = "Inequitable school systems benefit ECD students less than other students")
map_all_blk <- plot_usmap(data = policy_vs_equity, values = "all_black") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Black-all gap", guide = "legend", breaks = c(-0.02, 0, 0.02, 0.04), labels = c("More Equitable", "Neutral", "More Inequitable", "Most Inequitable")) +
  labs(title = "Black-All Inequity by State",
       subtitle = "Inequitable school systems benefit black students less than the state average")
map_wht_blk <- plot_usmap(data = policy_vs_equity, values = "white_black") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Black-white gap", guide = "legend", breaks = c(-0.02, 0, 0.02, 0.04), labels = c("More Equitable", "Neutral", "More Inequitable", "Most Inequitable")) +
  labs(title = "Black-White Inequity by State",
       subtitle = "Inequitable school systems benefit black students less than white students")



















ui <- fluidPage(navbarPage(
  "State-Wide Equitable Education Policies",
  tabPanel(
    "Main",
    mainPanel(
      "Data on United States School District Educational Equity:",
      
      
      
      
      
      selectInput(inputId = "policy",
                  label = "Select a state-wide policy",
                  choices = policy_names),
      plotOutput("map_policy"),
      
      
      
      selectInput(inputId = "gap",
                  label = "Select a measurement of inequity, comparing:",
                  choices = gap_names),
      plotOutput("map_equity")
      
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
))














server <- function(input, output, session) {
  
  app_policy_vs_equity <- reactive({ policy_vs_equity })
  
  selected_policy <- reactive({
    if(input$policy == "Merit Pay") return(map_merit_pay)
    if(input$policy == "Merit Tenure") return(map_merit_tenure)
    if(input$policy == "Need Pay") return(map_need_pay)
  })
  output$map_policy <- renderPlot({
    selected_policy()
  })
  
  selected_equity <- reactive({
    if(input$gap == "Econ. disadvantaged vs. all students") return(map_all_ecd)
    if(input$gap == "Econ. disadvantaged vs. not students") return(map_nec_ecd)
    if(input$gap == "Black vs. all students") return(map_all_blk)
    if(input$gap == "Black vs. white students") return(map_wht_blk)
    
  })
  output$map_equity <- renderPlot({
    selected_equity()
  })
    
  
}
shinyApp(ui, server)