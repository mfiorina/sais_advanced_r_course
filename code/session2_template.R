# SAIS Advanced R Course — Session 2 — Iterative Modeling Output Creation

# Purpose: Show how to produce useful and efficient
# visualizations from iterated modeling and regression outputs

  ## 1. Setup ----

#  options(scipen = 999)
#  options(tibble.width = Inf)

  if(!(require(pacman))) install.packages("pacman")
  pacman::p_load(dplyr, tidyr, purrr, stringr, janitor, skimr, data.table, broom, lfe, stargazer, tinytex, ggplot2)
  
  ## 2. Import Data ----
  
  wvs_raw <- fread(
    "data/intermediate/wvs_clean.csv", na.strings = ""
  )
  
  ## 3. Explore Data ----
  
  # Life-Importance Subjects (Q1-6)
  wvs_raw %>% skim(matches("q00[1-6]_life"))
  # Child Values (Q7-17)
  wvs_raw %>% skim(matches("q0(0[7-9]|1[0-7])_child"))
  # Neighbor Preferences (Q18-26)
  wvs_raw %>% skim(matches("q0(1[8-9]|2[0-6])_neighbor"))
  # Trust in Other People (Q58-63)
  wvs_raw %>% skim(matches("q0(5[8-9]|6[0-3])_trust"))
  # Confidence in Institutions (Q64-89, NOT with 82_suffix options)
  wvs_raw %>% skim(matches("q0(6[4-9]|7[0-9]|8[0-9])_confidence"))
  # Organization Membership (Q94-104, NOT with _r suffix)
  wvs_raw %>% skim(matches("q(09[4-9]|10[0-4])_member"))
  # Corruption Perception (Q113-117)
  wvs_raw %>% skim(matches("q11[3-7]_corr"))
  # Immigration Perception (Q122-129)
  wvs_raw %>% skim(matches("q12[2-9]_immigr"))
  # Information Source (Q201-208)
  wvs_raw %>% skim(matches("q20[1-8]_info_source"))
  # Elections Perception (Q224-233)
  wvs_raw %>% skim(matches("q2(2[4-9]|3[0-3])_elect"))
  
  ## 4. Simple Regression Example — One Variable ----
  
  # Research question: What is the relationship between child values and personal education level?
  
  # Necessary components:
  # Child value variables:
  # Q7:  good manners
  # Q8:  independence
  # Q9:  hard work
  # Q10: feeling of responsibility
  # Q11: imagination
  # Q12: tolerance and respect of other people
  # Q13: thrift saving money and things
  # Q14: determination and perseverance
  # Q15: religious faith
  # Q16: unselfishness
  # Q17: obedience
  # Education level variable: Q275
  
  child_value_data <- wvs_raw %>%
    select(
      d_interview, b_country_alpha, continent, matches("q0(0[7-9]|1[0-7])_"), q275_educ_level,
      # Control variables
      q260_gender, q262_age, q263_immigrant, q266_birth_country, q274_num_children, q279_emp_status, q288_income_scale
    )
  
    ### Simple Regression ----
  
  q007_lm <- lm(
    data    = child_value_data,
    formula = q007_child_manners ~ q275_educ_level
  )
  
  summary(q007_lm)  
  
    ### Simple Regression with Control Variables ----
  
  # Control variables:
  # Gender
  # Age
  # Immigration status
  # Home country
  # Has Children
  # Citizen status
  # Employment status
  # Income level
  
  q007_control_lm <- lm(
    data = child_value_data,
    formula = q007_child_manners ~ q275_educ_level + q260_gender + q262_age + q263_immigrant + q266_birth_country +
      q274_num_children + q279_emp_status + q288_income_scale
  )
  
  summary(q007_control_lm)
  
    ### Simple Regression with Control Variables and Country-Level Fixed Effects ----
  
  q007_control_fe_lm <- felm(
    data = child_value_data,
    formula = q007_child_manners ~ q275_educ_level + q260_gender + q262_age + q263_immigrant + q266_birth_country +
      q274_num_children + q279_emp_status + q288_income_scale | b_country_alpha | 0 | b_country_alpha
  )
  
  summary(q007_control_fe_lm)
  
  ## 5. Iterated Regressions ----
  
  # Process: use map() to perform lm process on multiple variables
  
    ### Iterated Regressions ----
  
  child_value_vars <- child_value_data %>%
    select(q007_child_manners:q017_child_obedience) %>%
    names()
  
  child_value_lms <- child_value_vars %>%
    map(
      \(var) lm(
        data    = child_value_data,
        formula = paste0(var, " ~ q275_educ_level")
      )
    )
  
    ### Iterated Regressions with Control Variables ----
  
  child_value_control_lms <- child_value_vars %>%
    map(
      \(var) lm(
        data    = child_value_data,
        formula = paste0(
          var, 
          " ~ q275_educ_level + q260_gender + q262_age + q263_immigrant + q266_birth_country + q274_num_children + q279_emp_status + q288_income_scale"
        )
      )
    )
  
    ### Iterated Regressions with Control Variables and Country-Level Fixed Effects ----
  
  child_value_control_fe_lms <- child_value_vars %>%
    map(
      \(var) felm(
        data    = child_value_data,
        formula = paste0(
          var, 
          " ~ q275_educ_level + q260_gender + q262_age + q263_immigrant + q266_birth_country + q274_num_children + q279_emp_status + q288_income_scale",
          " | b_country_alpha | 0 | b_country_alpha"
        ) %>% as.formula()
      )
    )
  
  ## 6. Outputting Iterated Regressions as Dataset ----
  
  child_value_control_fe_lm_data <- child_value_control_fe_lms %>%
    map2(
      child_value_vars,
      \(lm, var) broom::tidy(lm, conf.int = TRUE) %>%
        filter(term == "q275_educ_level") %>%
        mutate(
          dep_var = str_replace(var, "^q[0-9]{3}_", "")
        ) %>%
        select(dep_var, everything(), -c(term, statistic))
    ) %>%
    list_rbind()
  
  ## 7. Export Data ----
  
  write.csv(
    child_value_control_fe_lm_data, "output/child_value_lm.csv", row.names = FALSE, na = ""
  )
  
  ## 8. Estimates Plot Using ggplot2 ----
  
  child_value_labels <- c(
    "child_manners"        = "Manners",
    "child_independence"   = "Independence",
    "child_hard_work"      = "Hard Work",
    "child_responsibility" = "Responsibility",
    "child_imagination"    = "Imagination",
    "child_tolerance"      = "Tolerance",
    "child_thrift"         = "Thrift",
    "child_perseverance"   = "Perseverance",
    "child_faith"          = "Faith",
    "child_unselfish"      = "Selflessness",
    "child_obedience"      = "Obedience"
  )
  
  plot_child_value_vars <- child_value_vars %>%
    str_replace(., "^q[0-9]{3}_", "")
  
  child_value_ests_plot <- child_value_control_fe_lm_data %>%
    mutate(
      dep_var = factor(dep_var, levels = plot_child_value_vars)
    ) %>%
    ggplot() +
    geom_point(
      aes(x = dep_var, y = estimate), size = 3
    ) +
    geom_errorbar(
      aes(x = dep_var, ymin = conf.low, ymax = conf.high), width = 0.2
    ) +
    geom_hline(
      yintercept = 0, color = "red", linewidth = 1.5, linetype = "dotted"
    ) +
    scale_x_discrete(
      labels = child_value_labels, position = "top"
    ) +
    labs(
      title = "Relationship Between Education Level and Child Values",
      x = "", y = "Regression Estimate",
      caption = "A positive regression estimate indicates a positive correlation between higher education attained and the likelihood a respondent expressed a specific child value.\nError bars show estimates' 95% confidence interval."
    ) +
    theme_minimal(base_size = 18)
  
  child_value_ests_plot
  
  ggsave(
    plot = child_value_ests_plot, filename = "output/child_value_ests_plot.png",
    width = 5000, height = 2500, unit = "px"
  )
  
  ## 9. Quick Visualization Using Stargazer ----
  
  q007_lm_stargazer <- list(
    q007_lm, q007_control_lm, q007_control_fe_lm
  ) %>%
    stargazer(
      title            = "Relationship Between Child Manners and Education Level",
      dep.var.labels.include = FALSE,
      dep.var.caption        = "Child Manners",
      column.labels    = c("Reg", "Reg w/ Controls", "Reg w/ Controls and Country FE"),
      covariate.labels = c(
        "Education Level", "Gender", "Age", "Immigrant", "Birth Country", "Has Children",
        "Employment Status", "Income Level"
      ),
      add.lines = list(c("Country FE", "", "", "X")),
      digits = 4,
      header = FALSE
    )
  
# Manual modifications to be able to modify within R
  
  q007_lm_stargazer <- c(
    "\\documentclass[preview]{standalone}", "\\usepackage{adjustbox}", "\\usepackage{graphicx}", "\\begin{document}",
    q007_lm_stargazer[1:4],
    "\\begin{adjustbox}{max width=\\textwidth}",
    q007_lm_stargazer[5:51],
    "\\end{adjustbox}", "\\end{table}", "\\end{document}"
  )
  
  writeLines(q007_lm_stargazer, "output/q007_lm.tex")
  pdflatex(file = "output/q007_lm.tex", pdf_file = "output/q007_lm.pdf")
  
  ## 10. Iterated Visualizations Using Stargazer ----
  
  child_value_lms_stargazer <- pmap(
    list(child_value_lms, child_value_control_lms, child_value_control_fe_lms, child_value_labels),
    \(lm, lm_control, lm_control_fe, label) {
      lm_stargazer <- list(lm, lm_control, lm_control_fe) %>%
        stargazer(
          title                  = paste0("Relationship Between Child ", label, " and Education Level"),
          dep.var.labels.include = FALSE,
          dep.var.caption        = paste0("Child ", label),
          column.labels          = c("Reg", "Reg w/ Controls", "Reg w/ Controls and Country FE"),
          covariate.labels       = c(
            "Education Level", "Gender", "Age", "Immigrant", "Birth Country", "Has Children",
            "Employment Status", "Income Level"
          ),
          add.lines = list(c("Country FE", "", "", "X")),
          digits = 4,
          header = FALSE
        )
      lm_stargazer <- c(
        "\\documentclass[preview]{standalone}", "\\usepackage{adjustbox}", "\\usepackage{graphicx}", "\\begin{document}",
        lm_stargazer[1:4],
        "\\begin{adjustbox}{max width=\\textwidth}",
        lm_stargazer[5:51],
        "\\end{adjustbox}", "\\end{table}", "\\end{document}"
      )
    }
  )
  
  child_value_lms_stargazer %>%
    map2(
      child_value_vars,
      \(sg, var) writeLines(sg, paste0("output/child_value_tex/", var, ".tex"))
    )
  
  child_value_vars %>%
    map(
      \(var) pdflatex(
        file     = paste0("output/child_value_tex/", var, ".tex"),
        pdf_file = paste0("output/child_value_pdf/", var, ".pdf")
      )
    )
  