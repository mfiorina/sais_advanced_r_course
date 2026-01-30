# World Values Survey — Dataset Creation and Wrangling

# Purpose — Clean and wrangle dataset in preparation for course

  ## 1. Setup ----
  
  options(scipen = 999)
  options(tibble.width = Inf)
  
  if(!(require(pacman))) install.packages("pacman")
  pacman::p_load(dplyr, tidyr, purrr, stringr, janitor, skimr, data.table)

  ## 2. Import Data ----
  
    ### World Values Survey
  
  wvs_raw <- fread(
    "data/raw/wvs_data_2022.csv", na.strings = ""
  )
  
  country_continent_raw <- data.table::fread(
    "data/raw/country_continent.csv", na.strings = ""
  )
  
  wvs_varnames_raw <- data.table::fread(
    "documentation/wvs_varnames.csv", na.strings = ""
  )

  ## 3. Clean and Wrangle Data ----
  
  wvs_data <- wvs_raw %>%
    janitor::clean_names() %>%
    # Rename names so that numbers are consistent
    rename_with(
      ~ str_replace(.x, "(?<=^q)(?=[0-9])", "00"),
      matches("^q[0-9]($|_|[a-z])")
    ) %>%
    rename_with(
      ~ str_replace(.x, "(?<=^q)(?=[0-9])", "0"),
      matches("^q([0-9]{2})($|_|[a-z])")
    ) %>%
    # Clean and reorder variables: higher value = more and negative values as missing
    mutate(
      across(
        where(is.numeric),
        \(var) case_when(
          var < 0 ~ NA_real_,
          TRUE    ~ var
        )
      ),
      # 4-scale
      across(
        matches("^q(0((0[1-6])|(2[7-9])|(3[0-2])|(46)|(5[1-5])|(5[8-9]|(6[0-9])|(7[0-9])|(8[0-9])))|1((3[0-8])|(4[2-3])|(4[6-8])|(69)|(70)|(72r)|(9[6-9]))|(2((2[1-2])|(2[4-9])|(3[0-9])|(53)|(5[5-9]))))($|_)"),
        \(var) 4 - var # 4 -> 0, 3 -> 1, 2 -> 2, 1 -> 3
      ),
      # 2-scale
      across(
        matches("^q(0((0[7-9])|(1[0-9])|(2[0-6])|(57))|1(((39)|(4[0-1])|(4[4-5])|(51)|(6[5-8])|(2((63)|(69)|(85))))))$"),
        \(var) 2 - var # 2 -> 0, 1 -> 1
      ),
      # 5-scale
      across(
        matches("^q(0((3[3-9])|(4[0-1])|(47))|(2((0[1-8])|(34A)|(87))))$"),
        \(var) 5 - var # 5 -> 0, 4 -> 1, 3 -> 2, 2 -> 3, 1 -> 4
      ),
      # 3-scale
      across(
        matches("^q(0((4[3-5]))|(2((00)|(09)|(1[0-9])|(20))))$"),
        \(var) 3 - var # 3 -> 0, 2 -> 1, 1 -> 2
      ),
      # 7-scale
      across(
        matches("^q171$"),
        \(var) 7 - var # 7 -> 0, 6 -> 1, ..., 1 -> 6
      ),
      # 8-scale
      across(
        matches("^q172$"),
        \(var) 8 - var # 8 -> 0, 7 -> 1, ..., 1 -> 7
      ),
      # Correct order
      across(
        matches("^q((0((4[8-9])|(50)))|(1((1[3-8])|(21)))|(2((6[4-5]))))$"),
        \(var) var - 1 # 9 -> 8, 8 -> 7, ..., 1 -> 0
      ),
      # 3-scale, good/bad/meh order
      across(
        c(q033_3, q034_3, q035_3, q056),
        \(var) case_when(
          var == 1 ~ 2, # Agree
          var == 2 ~ 0, # Disagree
          var == 3 ~ 1,  # Neither agree nor disagree
          TRUE     ~ NA_real_
        )
      ),
      # Q119 — What's with the "hard to say" bro
      q119 = case_when(
        q119 == 0 ~ NA_real_,
        TRUE      ~ 4 - q119 # 4 -> 0, 3 -> 1, 2 -> 2, 1 -> 3
      ),
      # Q176 — Only example so far of 10-scale with smallest being "agree"?
      # Keep smallest as 1 and largest as 10 but reverse
      q176 = 11 - q176, # 10 -> 1, 9 -> 2, ..., 1 -> 10,
      # Q254 — "Not nationality" should be NA
      q254 = case_when(
        q254 == 5 ~ NA_real_,
        TRUE      ~ 4 - q254 # 4 -> 0, 3 -> 1, 2 -> 2, 1 -> 3
      ),
      # Adjust Morocco to work with continent data
      b_country_alpha = case_when(
        b_country_alpha == "MAR" ~ "MOR",
        TRUE                     ~ b_country_alpha
      )
    ) %>%
    # Add continent data
    left_join(
      country_continent_raw %>% select(country, continent), by = c("b_country_alpha" = "country")
    ) %>%
    relocate(
      continent, .after = "b_country_alpha"
    ) %>%
    # Provide final variable names
    rename_with(
      ~ wvs_varnames_raw$var_name[wvs_varnames_raw$var_num == .x],
      everything()
    )
  
  ## 4. Export Data ----
  
  fwrite(
    wvs_data, "data/intermediate/wvs_clean.csv", row.names = FALSE, na = ""
  )
  