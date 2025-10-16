# Customized economist ggplot theme

library(ggplot2)
library(ggthemes)

theme_economist_wheat_bkgrnd <- function() {
    ggthemes::theme_economist() +
        theme(
            text = element_text(family = "Arial"),      # Change font
            plot.background = element_rect(fill = "#eff0cc", color = NA),  # Change background
            panel.background = element_rect(fill = "#eff0cc"), # Modify panel background
            axis.text = element_text(size = 12, color = "black"),  # Adjust axis text
            axis.title = element_text(size = 14, face = "bold"),   # Bold axis titles
            legend.position = "bottom"  # Move legend to bottom
        )
}
