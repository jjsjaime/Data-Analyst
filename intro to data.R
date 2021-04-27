#------------------------------#
#   Introduction to Data       #
#------------------------------#



# Table of the number variable
table(email50_big$number)

# Drop levels
email50_big$number <- droplevels(email50_big$number)

# Another table of the number variable
table(email50_big$number)


# Create number_yn column in email50
email50_fortified <- email50 %>%
  mutate(
    number_yn = case_when(
      # if number is "none", make number_yn "no"
      number == "none" ~ "no", 
      # if number is not "none", make number_yn "yes"
      number != "none" ~ "yes"  
    )
  )


# Visualize the distribution of number_yn
ggplot(email50_fortified, aes(x = number_yn)) +
  geom_bar()


# fazendo um estudo para ver se tem vies em um vestibular
# vendo numero de estudantes admitidos e rejeitados de ambos os sexos

# Load packages
library(dplyr)

# Count number of male and female applicants admitted
ucb_admit %>%
  count(Gender, Admit)

ucb_admission_counts %>%
  # Group by gender
  group_by(Gender) %>%
  # Create new variable
  mutate(prop = n / sum(n)) %>%
  # Filter for admitted
  filter(Admit == "Admitted")

ucb_admission_counts <- ucb_admit %>%
  # Counts by department, then gender, then admission status
  count(Dept, Gender, Admit)

# See the result
ucb_admission_counts


ucb_admission_counts  %>%
  # Group by department, then gender
  ___(___, ___) %>%
  # Create new variable
  ___(prop = ___) %>%
  # Filter for male and admitted
  ___(___, ___)


# fazendo amostragem em R
install.packages("openintro")
library(openintro)
library(dplyr)
data(county)
str(county)

# tirando DC
county_noDC <- county %>%
  filter(state != "District of Columbia") %>%
  droplevels()

# Simple random sample: states_srs
county_srs <- county_noDC %>%
  sample_n(size = 150)

# visual
glimpse(county_srs) 

# Count states by region
county_srs %>%
  group_by(state) %>%
  count()

# amostra estratificada por estado
county_str <- county_noDC %>%
  group_by(state) %>%
  sample_n(size = 3)

#visual
glimpse(county_str) 
table(county_str$state)
table(county$state)



# Simple random sample: states_srs
states_srs <- us_regions %>%
  sample_n(size = 8)

# Count states by region
states_srs %>%
  count(region)

# Stratified sample
states_str <- us_regions %>%
  group_by(region) %>%
  sample_n(size = 2)

# Count states by region
states_str %>%
  group_by(region) %>%
  count(region)


# capitulo final - o mais bunito da sala

# Inspect variable types
glimpse(evals)


# Recode cls_students as cls_type
evals_fortified <- evals %>%
  mutate(
    cls_type = case_when(
      cls_students <= 18 ~ "small",
      cls_students >= 19 & cls_students <= 59 ~ "midsize",
      cls_students >= 60 ~ "large"
    )
  )


# Scatterplot of score vs. bty_avg
ggplot(evals, aes(bty_avg, score)) +
  geom_point()


# Scatterplot of score vs. bty_avg colored by cls_type
ggplot(evals, aes(y = score, x = bty_avg, color = cls_type)) +
  geom_point()




