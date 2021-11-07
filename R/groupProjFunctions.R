library(ggplot2)
library(reshape2)
library(tidyverse)

##devtools::install_github("mallorymaher12/QBS181_ConvenienceFunctions1", ref = "main", force = T)

#' Region/Country Plot with Mental Health Disorder Prevalence
#'
#' Function that returns a plot of mental health prevalence rates
#' per country or region
#'
#' @param entity enter the country or region you want as a string
#' @param entityDF the data frame you want to use
#'
#' @return a plot that has the mental health prevalence rates per country or region
#' @examples
#' country_region_plots("United States", fullDF_region)
#'
#' @export

country_region_plots <- function(entity, entityDF){
  library(tidyverse)
  library(ggplot2)
  library(reshape2)
  regionDF_entity <- entityDF %>%
    filter(Entity == entity) %>%
    group_by(Year) %>%
    summarise(Alcohol_Use = mean(`Alcohol use disorders (%)`),
              Anxiety = mean(`Anxiety disorders (%)`),
              Bipolar = mean(`Bipolar disorder (%)`),
              Depression = mean(`Depression (%)`),
              Drug_Use = mean(`Drug use disorders (%)`),
              Eating_Disorder = mean(`Eating disorders (%)`),
              Schizophrenia = mean(`Schizophrenia (%)`))

  entity_df <- melt(regionDF_entity, id.vars = 'Year',
                    variable.name = 'Mental Health Disorder')

  ggplot(data = entity_df, aes(x = Year, y = value, color = `Mental Health Disorder`)) +
    geom_line(stat = "identity", size = 1) +
    geom_point(aes(y = value)) +
    theme_light() +
    labs(title = paste(entity, "Mental Health Prevalence from 1990 to 2017"),
         x = "Year", y = "Prevalence (%)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Paired")
}

#' Age Gender Plot by Country/Region
#'
#' Function that returns all non-unique values in a vector
#'
#' @param entity enter the country or region you want as a string
#' @param entityDF the data frame you want to use
#'
#' @return a plot that has the mental health prevalence rates by
#' age and gender country or region
#'
#' @examples
#' age_gender_plots("United States", fullDF_region)
#' @export

age_gender_plots <- function(entity, dataframe){
  plotDF_age <- dataframe %>%
    filter(Entity == entity) %>%
    select(Year, `10-14 years old (%)`,
           `15-19 years old (%)`,
           `20-24 years old (%)`,
           `25-29 years old (%)`,
           `30-34 years old (%)`,
           `50-69 years old (%)`,
           `70+ years old (%)`)

  plotDF_gender <- dataframe %>%
    filter(Entity == entity) %>%
    select(Year, `Prevalence in males (%)`,
           `Prevalence in females (%)`)

  plotDF2_age <- melt(plotDF_age, id.vars = 'Year')
  plotDF2_gender <- melt(plotDF_gender, id.vars = 'Year')

  ggplot() +
    geom_density(aes(x = Year, y = value, group = variable,
                     fill = variable), color = "white",
                 data = plotDF2_gender, stat = "identity", alpha=0.3) +
    geom_line(aes(x = Year, y = value, group = variable, color = variable),
              data = plotDF2_age, size = 1) +
    geom_point(aes(x = Year, y = value, color = variable), data = plotDF2_age) +
    labs(y = "Depression Prevalence (% Population)",
         title = paste("Prevalence of Depression by Sex and Age in", entity),
         fill = "Gender", color = "Age Group") +
    theme_light() +
    scale_fill_manual(values = c("#34c9eb", "pink")) +
    scale_color_brewer(palette = "Dark2")
}

#' Region Graphs by Age Group
#'
#' Function to create a plot that shows the prevalence of
#' each age group in a specific region over time
#'
#' @param entity name of entity
#' @param df name of dataframe
#'
#' @return line plot of prevalence for each age group
#'
#' @examples
#' entity = North America
#' df = northAmerica_pivot
#' graph_for_region("North America", northAmerica_pivot)
#'
#' @export

graph_for_region <- function(entity, df){
  df %>%
    ggplot(aes(x=Year, y=prevalence, group=age_group, color = age_group)) +
    geom_line(size =1) +
    labs(y = "Prevalence",
         title = paste("Prevalence of Depression in", entity)) +
    scale_colour_brewer(palette = "Paired") +
    theme_light()
}

#' Pivot my Data
#'
#' Function that pivots the data to put Year in a column,
#' age group in a column, and prevalence in a column
#'
#' @param entity name of entity
#'
#' @return a pivoted dataframe with the Year in a column,
#' the age groups in a separate column, and the prevalence in a
#' column for one region where the region is the value passed in as entity
#'
#' @examples
#' entity = North America
#' pivot_my_data("North America)
#'
#' @export

pivot_my_data <- function(entity, df){
  df %>%
    filter(Entity == entity) %>%
    select(Year, ten_to_fourteen, fifteen_to_nineteen, twenty_to_twentyFour,
           twentyFive_to_twentyNine, thirty_to_thirtyFour, fifty_to_sixtyNine,
           seventy_plus) %>%
    pivot_longer(!Year, names_to = "age_group", values_to = "prevalence")
}

#' Build an Age Graph
#'
#' Function that creates a line graph of the prevalence of depression for a certain age group in all regions
#'
#' @param age_grp age group to graph
#'
#' @return a line graph that shows the prevalence of depression across all regions for the specified age groups
#'
#' @examples
#' age_grp = ten_to_fourteen
#' build_age_graphs(df$ten_to_fourteen, df)
#'
#' @export

build_age_graphs <- function(age_grp, df){

  if(age_grp == df$ten_to_fourteen){
    age_title = "10-14"
  }
  if(age_grp == df$fifteen_to_nineteen){
    age_title = "15-19"
  }
  if(age_grp == df$twenty_to_twentyFour){
    age_title = "20-24"
  }
  if(age_grp == df$twentyFive_to_twentyNine){
    age_title = "25-29"
  }
  if(age_grp == df$thirty_to_thirtyFour){
    age_title = "30-34"
  }
  if(age_grp == df$fifty_to_sixtyNine){
    age_title = "50-69"
  }
  if(age_grp == df$seventy_plus){
    age_title = "70+"
  }

  df %>%
    ggplot(aes(x=Year, y=age_grp, group=Entity, color = Entity)) +
    geom_line(size=.5) +
    labs(y = "Prevalence",
         title = paste("Prevalence of Depression in", age_title, "Age Group")) +
    theme_light() +
    scale_color_manual(values = region_cols)
}
