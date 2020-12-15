library(shiny)
library(tidyverse)
library(readxl)
library(usmap)
library(tidymodels)
library(rstanarm)



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
policy_vs_equity <- policy_vs_equity[c(1, 2, 6, 3, 4, 5, 7, 8, 9, 10, 11)]




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











hist_ae_merit_pay <- posterior_epred(mod_ae_merit_pay, newdata = state_merit_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-ECD equity by state",
       subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
       x = "Average difference in All-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ne_merit_pay <- posterior_epred(mod_ne_merit_pay, newdata = state_merit_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for NEC-ECD equity by state",
       subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
       x = "Average difference in NEC-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ab_merit_pay <- posterior_epred(mod_ab_merit_pay, newdata = state_merit_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-Black equity by state",
       subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
       x = "Average difference in All-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_wb_merit_pay <- posterior_epred(mod_wb_merit_pay, newdata = state_merit_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for White-Black equity by state",
       subtitle = "Difference between encouraged merit pay and not encouraged merit pay",
       x = "Average difference in White-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ae_need_pay <- posterior_epred(mod_ae_need_pay, newdata = state_need_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-ECD equity by state",
       subtitle = "Difference between need pay and no need pay",
       x = "Average difference in All-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ne_need_pay <- posterior_epred(mod_ne_need_pay, newdata = state_need_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for NEC-ECD equity by state",
       subtitle = "Difference between need pay and no need pay",
       x = "Average difference in NEC-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ab_need_pay <- posterior_epred(mod_ab_need_pay, newdata = state_need_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-Black equity by state",
       subtitle = "Difference between need pay and no need pay",
       x = "Average difference in All-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_wb_need_pay <- posterior_epred(mod_wb_need_pay, newdata = state_need_pay) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for White-Black equity by state",
       subtitle = "Difference between need pay and no need pay",
       x = "Average difference in White-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ae_merit_tenure <- posterior_epred(mod_ae_merit_tenure, newdata = state_merit_tenure) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-ECD equity by state",
       subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
       x = "Average difference in All-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ne_merit_tenure <- posterior_epred(mod_ne_merit_tenure, newdata = state_merit_tenure) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for NEC-ECD equity by state",
       subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
       x = "Average difference in NEC-ECD gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_ab_merit_tenure <- posterior_epred(mod_ab_merit_tenure, newdata = state_merit_tenure) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for All-Black equity by state",
       subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
       x = "Average difference in All-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")

hist_wb_merit_tenure <- posterior_epred(mod_wb_merit_tenure, newdata = state_merit_tenure) %>% 
  as_tibble() %>% 
  mutate(diff = `2` - `1`) %>%  
  ggplot(aes(x = diff, fill = after_stat(x), y = after_stat(count / sum(count)))) +
  geom_histogram(position = "identity", binwidth = 0.0005, color = "darkgrey") +
  theme_bw() +
  labs(title = "Posterior for White-Black equity by state",
       subtitle = "Difference between required teacher effectiveness proof for tenure and no proof requirement",
       x = "Average difference in White-Black gaps",
       y = "Distribution") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_fill_gradient2(low = "#1ad63f", mid = "#f7f7f7", high = "red", name = "Policy-Gap Correlation")













ui <- fluidPage(navbarPage(
  "Equity in Education: Effectiveness of State-Level Policies",
  tabPanel(
    "Overview",
    h4("Growth Scores"),
    p("The premise of this project is to correlate specific state-wide policies with each state school system's performance.
      A classic measurement of school performance would measure each student's standardized test score, assuming that higher test scores mean that the school is benefitting the student highly.
      However, the test score measurement of school performance neglects to account for student readiness: if a student comes into the schoolyear
      already prepared to do well on standardized tests, then the end-of-year results may reflect the student's out-of-school preparedness
      (e.g. academic summer camps, private tutoring, personal access to books and the internet) more than it reflects the school's effectiveness.
      This means that schools with high testing may not have helped students' academic growth all that much, and this measurement could have mislabelled
      lower-testing schools as ineffective when they could have already raised student performance since the year's beginning.
      This also generally means that for analysis on educational equity, it seems that schools with more families with high access to
      extracurricular education may seem fundamentally linked with better schools, which is discouraging for children with less access but
      want to improve through certain equitable education policies."),
    p("A better measure takes the \"rate of learning\" for each cohort of students across their years of standardized testing.
      This growth score measurement somewhat eliminates out-of-school factors, giving both a better measurement of school performance and
      ultimately a better measurement of how effective implemented equitable education policies are at benefitting students."),
    h4("Equity"),
    p("This project takes growth score data from The Educational Oppportunity Project at Stanford University, which is subdivided by \"subgroups\"
      including race and socioeconomic status. This project's measurements of equity take the difference in the state average growth scores between
      these subgroups (e.g. the difference between a state's average white student growth scores and black student growth scores."),
    h4("Policies"),
    p("This project currently looks at the correlation between equity and 3 state-wide policies: merit pay, need pay, and merit tenure."),
    p("Merit pay is the consideration of students' performances on testing when deciding on a teacher's pay."),
    p("Need pay is the incentivization of teaching in high-need schools with differential pay."),
    p("Merit tenure is the requirement of proof of teacher effectiveness when a teacher is being considered for tenure.")
  ),
  tabPanel(
    "Maps",
    
    h3("Policies and Equity by State"),
      
      
      
      
      
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
           
           h4("Correlations between state policies and state equities"),
           
           selectInput(inputId = "policy2",
                       label = "Select a state-wide policy:",
                       choices = policy_names),
           
           selectInput(inputId = "gap2",
                       label = "Select a measurement of inequity, comparing:",
                       choices = gap_names),
    
    
    plotOutput("hist_correlation"),
    
    
    
    h4("Model"),
    p("Stan engine linear regression:"),
    withMathJax(uiOutput("model"))
    
    
  ),
  tabPanel("About",
           
           sidebarPanel("Downloads:", p(""),
             downloadLink("downloadData", "Data Used"), p(""),
             downloadLink("downloadCodebook", "Data Codebook"), p(""),
             downloadLink("downloadApp", "Shiny App Code")
             
           ),
           
           mainPanel(
           h4("Author:"),
           p("Andrew Jing"),
           
           
           h4("About"),
           p("This final project uses currently established state-wide policies
             to predict the racial and socioeconomic inequities within
             state school systems."),
           p("Next steps involve making the Shiny app easily expandable to other policies
             without having to manually create maps, graphs, or boolean returns.
             My goal is just to have to insert policy data into a .csv and the codebook."),
           p("It would also be nice to find district-wide
             or county-wide equity policies so there are more that only 50 data points to base predictions off of.
             As of now, because of the limited data predictions are uncertain and relatively statistically insignificant.
             However, I've not been able to find any, but maybe there's one state or another
             that has collected that data.")
           )
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
  

  
  selected_correlation <- reactive({
    if(input$policy2 == "Merit Pay" & input$gap2 == "Econ. disadvantaged vs. all students") return(hist_ae_merit_pay)
    if(input$policy2 == "Merit Pay" & input$gap2 == "Econ. disadvantaged vs. not students") return(hist_ne_merit_pay)
    if(input$policy2 == "Merit Pay" & input$gap2 == "Black vs. all students") return(hist_ab_merit_pay)
    if(input$policy2 == "Merit Pay" & input$gap2 == "Black vs. white students") return(hist_wb_merit_pay)
    
    if(input$policy2 == "Merit Tenure" & input$gap2 == "Econ. disadvantaged vs. all students") return(hist_ae_merit_tenure)
    if(input$policy2 == "Merit Tenure" & input$gap2 == "Econ. disadvantaged vs. not students") return(hist_ne_merit_tenure)
    if(input$policy2 == "Merit Tenure" & input$gap2 == "Black vs. all students") return(hist_ab_merit_tenure)
    if(input$policy2 == "Merit Tenure" & input$gap2 == "Black vs. white students") return(hist_wb_merit_tenure)
    
    if(input$policy2 == "Need Pay" & input$gap2 == "Econ. disadvantaged vs. all students") return(hist_ae_need_pay)
    if(input$policy2 == "Need Pay" & input$gap2 == "Econ. disadvantaged vs. not students") return(hist_ne_need_pay)
    if(input$policy2 == "Need Pay" & input$gap2 == "Black vs. all students") return(hist_ab_need_pay)
    if(input$policy2 == "Need Pay" & input$gap2 == "Black vs. white students") return(hist_wb_need_pay)
  })
  
  output$hist_correlation <- renderPlot({
    selected_correlation()
  })
  
  output$model <- renderUI({
    withMathJax("$$ equity\\_measurement_{i} = \\beta_0 + \\beta_1policy_{i} + \\epsilon_{i} $$")
  })

  output$downloadCodebook <- downloadHandler(
    filename = "policy_vs_equity_codebook.xlsx",
    content = function(file) {
      file.copy("used_data/policy_vs_equity_codebook.xlsx", file)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = "policy_vs_equity.csv",
    content = function(file) {
      write.csv(policy_vs_equity, file, row.names = FALSE)
    }
  )
  
  output$downloadApp <- downloadHandler(
    filename = "education_equity_project.R",
    content = function(file) {
      file.copy("Final.R", file)
    }
  )
  
  
}
shinyApp(ui, server)