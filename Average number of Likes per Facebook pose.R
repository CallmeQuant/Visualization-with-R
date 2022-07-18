library(tidyverse) # Data Wrangling 
library(showtext) # Package for using extra fonts. 
library(grid)

dat <- read_csv("http://infographics.economist.com/databank/Economist_corbyn.csv")

# Data preprocessing for visualization
clean_data <- dat %>%
  mutate(num_likes = `Average number of likes per Facebook post 2016` / 1000) %>%
  filter(!is.na(num_likes)) %>%
  mutate(font = case_when(Page == "Jeremy Corbyn" ~ "bold",
                          Page == "Owen Smith" ~ "plain",
                          Page == "Andy Burnham" ~ "plain",
                          TRUE ~ "italic")) %>%
  mutate(text_color_y_axis = case_when(Page == "Jeremy Corbyn" ~ "grey10",
                                       TRUE ~ "grey30")) %>%
  arrange(num_likes) %>%
  mutate(Page = factor(Page, Page))
    
# Constant including font and color 

my_font <- "Lato" 

# Load font for ploting: 

font_add_google(name = my_font, family = my_font) 

font_y <- "Roboto Condensed" 

font_add_google(name = font_y, family = font_y)

showtext_auto() # Automatically render text. 


bar_color <- "#116ea1"

background_color <- "#d9e9f0"

red_icon <- "#ed1b24"

jeremy_color <- "#c1dae6"

grid_color <- "#b6c5ce"

p_title <- "Left-click"

p_subtitle <- "Average number of likes per Facebook post\n2016, '000"

p_caption <- "Source: Facebook"

# Visualization using Bar Plot 
clean_data %>% 
  ggplot(aes(y = Page, x = num_likes)) + 
  geom_col(fill = bar_color, width = 0.6) + 
  geom_col(data = clean_data %>% filter(Page == "Jeremy Corbyn"), 
           fill = bar_color, width = 0.6) + 
  labs(title = p_title, subtitle = p_subtitle, caption = p_caption) + 
  theme(plot.background = element_rect(fill = background_color, color = NA)) +
  theme(panel.background = element_rect(fill = background_color, color = NA)) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.title = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(plot.margin = unit(rep(0.7, 4), "cm")) + 
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, 1), expand = c(0, 0),
                     position = "top") +  
  theme(plot.title.position = "plot") + 
  theme(plot.caption.position = "plot") + 
  theme(plot.title = element_text(family = my_font, size = 18, face = "bold", hjust = 0, margin = margin(b = 0.2, unit = "cm"))) +  
  theme(plot.subtitle = element_text(family = my_font, size = 14, color = "grey10", hjust = 0)) + 
  theme(axis.line.y = element_line(size = 0.8, color = "grey40")) + 
  theme(plot.caption = element_text(family = my_font, color = "grey40", size = 12, hjust = 0, vjust = -3)) + 
  theme(axis.text.x = element_text(size = 14, color = "grey25", family = my_font)) + 
  theme(axis.text.y = element_text(size = 14, color = clean_data$text_color_y_axis, family = font_y, face = clean_data$font, hjust = 0)) + 
  theme(panel.grid.major.x = element_line(color = grid_color, size = 0.8)) 


grid.rect(x = 0.045, y = 1, width = 0.05*1.5, height = 0.028, just = c("left", "top"), gp = gpar(fill = red_icon, col = red_icon))
