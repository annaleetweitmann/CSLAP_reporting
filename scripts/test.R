library(tidyverse)
library(gt)

lake2 <- lake1 %>%
  filter(year == report_year)

lake2$Result.Sample.Fraction <- replace_na(lake2$Result.Sample.Fraction, "None")

lake3 <- lake2 %>% 
  mutate(FULL_CHARACTERISTIC_NAME = paste(Characteristic.Name, INFO_TYPE, Result.Sample.Fraction, sep=" ")) %>% 
  filter(FULL_CHARACTERISTIC_NAME %in% c("DEPTH, SECCHI DISK DEPTH SD None",
                                         "PHOSPHORUS OW T",
                                         "PHOSPHORUS OW D", 
                                         "PHOSPHORUS BS T",
                                         "PHOSPHORUS BS D",
                                         "NITROGEN, TOTAL OW T",
                                         "NITROGEN, TOTAL DISSOLVED OW D",
                                         "TN:TP",
                                         "AMMONIA OW T",
                                         "AMMONIA BS T",
                                         "CHLOROPHYLL A OW T",
                                         "PH OW None",
                                         "SPECIFIC CONDUCTANCE OW None",
                                         "CALCIUM OW T",
                                         "CALCIUM BS T",
                                         "CHLORIDE OW T",
                                         "CHLORIDE BS T",
                                         "TEMPERATURE, WATER OW None",
                                         "TEMPERATURE, WATER BS None",
                                         "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE) OW None"
  )) %>% 
  group_by(FULL_CHARACTERISTIC_NAME, SAMPLE_DATE) %>% 
  summarize(Avg.Result.Value = mean(as.numeric(Result.Value))) %>% 
  ungroup()

tibble_plot <- lake3 %>%
  group_by(FULL_CHARACTERISTIC_NAME) %>%
  nest() %>%
  mutate(plot = map(data, ~ggplot(., aes(SAMPLE_DATE, Avg.Result.Value)) +
                      geom_line(color="blue", size=5) +
                      theme_void())) %>%
  select(-data) %>% 
  # Create empty column (a placeholder for the images)
  mutate(ggplot = NA)


#Creates the length of the tibble
text_names <-  lake3 %>% 
  select(FULL_CHARACTERISTIC_NAME) %>%
  unique() %>% 
  pull() 


# Is a bit slow for me
tibble_output <- tibble(
  text = text_names,
  ggplot = NA,
  .rows = length(text_names)) %>%
  gt() %>%
  text_transform(
    locations = cells_body(vars(ggplot)),
    fn = function(x) {
      map(tibble_plot$plot, ggplot_image, height = px(100))
    }
  )

tibble_output








