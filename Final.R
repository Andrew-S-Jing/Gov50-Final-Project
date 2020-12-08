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

growth_scores <- read_csv("used_data/seda_pool_cs.csv", col_types = cols(leaidC = col_double())) 
demographics <- read_csv("used_data/seda_cov_pool.csv")



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
                                    subgroup == "blk" ~ totenrl * perblk,
                                    subgroup == "ecd" ~ totenrl * perecd,
                                    subgroup == "wht" ~ totenrl * perwht,
                                    subgroup == "nec" ~ totenrl * (1 - perecd))) %>% 
  select(-perblk, -perwht, -perecd) %>% 
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










#models
set.seed(2020)
mod_ne_need_pay <- stan_glm(data = policy_vs_equity,
                            refresh = 0,
                            nec_ecd ~ need_pay)
mod_ne_merit_pay <- stan_glm(data = policy_vs_equity,
                             refresh = 0,
                             nec_ecd ~ merit_pay)
mod_ne_merit_tenure <- stan_glm(data = policy_vs_equity,
                                refresh = 0,
                                nec_ecd ~ merit_tenure)

mod_ae_need_pay <- stan_glm(data = policy_vs_equity,
                            refresh = 0,
                            all_ecd ~ need_pay)
mod_ae_merit_pay <- stan_glm(data = policy_vs_equity,
                             refresh = 0,
                             all_ecd ~ merit_pay)
mod_ae_merit_tenure <- stan_glm(data = policy_vs_equity,
                                refresh = 0,
                                all_ecd ~ merit_tenure)

mod_wb_need_pay <- stan_glm(data = policy_vs_equity,
                            refresh = 0,
                            white_black ~ need_pay)
mod_wb_merit_pay <- stan_glm(data = policy_vs_equity,
                             refresh = 0,
                             white_black ~ merit_pay)
mod_wb_merit_tenure <- stan_glm(data = policy_vs_equity,
                                refresh = 0,
                                white_black ~ merit_tenure)

mod_ab_need_pay <- stan_glm(data = policy_vs_equity,
                            refresh = 0,
                            all_black ~ need_pay)
mod_ab_merit_pay <- stan_glm(data = policy_vs_equity,
                             refresh = 0,
                             all_black ~ merit_pay)
mod_ab_merit_tenure <- stan_glm(data = policy_vs_equity,
                                refresh = 0,
                                all_black ~ merit_tenure)

state_need_pay <- tibble(need_pay = c("No", "Yes"))
state_merit_pay <- tibble(merit_pay = c("No Infrastructure", "Encouraged", "Required"))
state_merit_tenure <- tibble(merit_tenure = c("Not Required", "Partially Required", "Proof Required", "No State Tenure"))
















ui <- fluidPage(navbarPage(
  "State-Wide Equitable Education Policies",
  tabPanel(
    "Overview",
      "Policies and Equity by State:",
      
      
      
      
      
      selectInput(inputId = "policy",
                  label = "Select a state-wide policy:",
                  choices = policy_names),
      plotOutput("map_policy"),
      
      
      
      selectInput(inputId = "gap",
                  label = "Select a measurement of inequity, comparing:",
                  choices = gap_names),
      plotOutput("map_equity")

  ),
  tabPanel("Correlation",
    h3("Merit Pay"),
    plotOutput("ae_merit_pay"),
    plotOutput("ne_merit_pay"),
    plotOutput("ab_merit_pay"),
    plotOutput("wb_merit_pay"),
    p(""),
    h3("Need Pay"),
    plotOutput("ae_need_pay"),
    plotOutput("ne_need_pay"),
    plotOutput("ab_need_pay"),
    plotOutput("wb_need_pay"),
    p(""),
    h3("Merit Tenure"),
    plotOutput("ae_merit_tenure"),
    plotOutput("ne_merit_tenure"),
    plotOutput("ab_merit_tenure"),
    plotOutput("wb_merit_tenure")
    
    
  ),
  tabPanel("About",
           
           h4("Author"),
           p("Andrew Jing"),
           p(""),
           
           h4("About"),
           p("This final project uses currently established state-wide policies
             to predict the racial and socioeconomic inequities within the
             state's school system."),
           p(""),
           p("Currently the data comes from The Education Opportunity Project
             at Stanford University and the National Council on Teacher Quality.")
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
  
  output$ae_merit_pay <- renderPlot({
    posterior_epred(mod_ae_merit_pay, newdata = state_merit_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-ECD equity by state",
           subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
           x = "Average difference in All-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
    
  output$ne_merit_pay <- renderPlot({
    posterior_epred(mod_ne_merit_pay, newdata = state_merit_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for NEC-ECD equity by state",
           subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
           x = "Average difference in NEC-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ab_merit_pay <- renderPlot({
    posterior_epred(mod_ne_merit_pay, newdata = state_merit_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-Black equity by state",
           subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
           x = "Average difference in All-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$wb_merit_pay <- renderPlot({
    posterior_epred(mod_ne_merit_pay, newdata = state_merit_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for White-Black equity by state",
           subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
           x = "Average difference in White-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ae_need_pay <- renderPlot({
    posterior_epred(mod_ae_need_pay, newdata = state_need_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-ECD equity by state",
           subtitle = "Difference between need pay and no need pay",
           x = "Average difference in All-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ne_need_pay <- renderPlot({
    posterior_epred(mod_ne_need_pay, newdata = state_need_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for NEC-ECD equity by state",
           subtitle = "Difference between need pay and no need pay",
           x = "Average difference in NEC-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ab_need_pay <- renderPlot({
    posterior_epred(mod_ab_need_pay, newdata = state_need_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-Black equity by state",
           subtitle = "Difference between need pay and no need pay",
           x = "Average difference in All-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$wb_need_pay <- renderPlot({
    posterior_epred(mod_wb_need_pay, newdata = state_need_pay) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for White-Black equity by state",
           subtitle = "Difference between need pay and no need pay",
           x = "Average difference in White-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ae_merit_tenure <- renderPlot({
    posterior_epred(mod_ae_merit_tenure, newdata = state_merit_tenure) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-ECD equity by state",
           subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
           x = "Average difference in All-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ne_merit_tenure <- renderPlot({
    posterior_epred(mod_ne_merit_tenure, newdata = state_merit_tenure) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for NEC-ECD equity by state",
           subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
           x = "Average difference in NEC-ECD gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$ab_merit_tenure <- renderPlot({
    posterior_epred(mod_ae_merit_tenure, newdata = state_merit_tenure) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for All-Black equity by state",
           subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
           x = "Average difference in All-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  output$wb_merit_tenure <- renderPlot({
    posterior_epred(mod_ae_merit_tenure, newdata = state_merit_tenure) %>% 
      as_tibble() %>% 
      mutate(diff = `2` - `1`) %>%  
      ggplot(aes(x = diff, y = after_stat(count / sum(count)))) +
      geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.0005) +
      theme_bw() +
      labs(title = "Posterior for White-Black equity by state",
           subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
           x = "Average difference in White-Black gaps",
           y = "Distribution") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
  })
  
  
}
shinyApp(ui, server)