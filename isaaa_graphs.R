library(tidyverse)

setwd('~/Desktop/unicorn')
isaaa <- read_csv("isaaa_full.csv")
view(isaaa)

#Graph 1, looking at approval rates

isaaa <- isaaa %>%
  filter(
    !Editing_Method %in% c("Other"),
    !Regulatory_Process %in% c("NA", "", NA)
  ) %>%
  mutate(Country = fct_reorder(
    Country,
    Regulatory_Process == "Yes",
    sum,
    .desc = FALSE
  ))

animal_approval <- ggplot(isaaa, aes(x=Country, fill = Regulatory_Process)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Editing_Method) +
  scale_fill_manual(values = c("Yes" = "purple", "No" = "pink")) +
  coord_flip() +
  labs(
    title = "Animal Embryo-Editing Experiments are\n Rarely Approved",
    x = NULL,
    y = "Number of Studies",
    fill = "Regulatory Approval?",
    subtitle = "The varied agricultural applications of animal\n gene-editing are rarely approved.\n Often, these projects are mpeded by stringent country-to-country\nregulation.",
    caption = "Data from _"
  ) +
  theme_minimal()

animal_approval

ggsave("animal_approval_plot.pdf", width = 8, height = 6)

# How many approved? Where?

mean(isaaa$Regulatory_Process == "Yes", na.rm = TRUE) * 100

sum(isaaa$Country == "China" & isaaa$Regulatory_Process == "Yes", na.rm = TRUE)

sum(isaaa$Country == "Argentina" & isaaa$Regulatory_Process == "Yes", na.rm = TRUE)
sum(isaaa$Country == "Argentina" & isaaa$Regulatory_Process == "No", na.rm = TRUE)

# Graph 2, what about the U.S.?

us_only <- isaaa %>%
  filter (Country == "United States",
          !is.na(Species),
          Species != "NA",
          Species != "Other") %>%
  count(Species) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n),
         label = n
    )

us_plot <- ggplot(us_only, aes(x = "", y = n, fill = reorder(Species, -n))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(palette = "Greens") +
  labs(
    title = "Animal embryo experimentation in the United States,\n by species",
    subtitle = "Most studies involve cattle and pigs, whom\n are mostly gene-edited via CRISPR.",
    caption = "Data from ISAAA, Y to Y.",
    fill = "Species") +
  theme_minimal() 

table(isaaa$Editing_Method)[c("Cloned cell line", "Embryo edit")]
      
sum(isaaa$Country == "United States" & isaaa$Regulatory_Process == "Yes", na.rm = TRUE)
sum(isaaa$Country == "United States" & isaaa$Regulatory_Process == "No", na.rm = TRUE)

us_plot

ggsave("US_plot.pdf", width = 8, height = 6)


# Graph 3 and 4, looking at MSTN

MSTN <- isaaa %>%
  filter(grepl("\\bMSTN\\b", Genes)) %>%
  count(Species, Country) %>%
  arrange(desc(n))

mstn_1 <- ggplot(MSTN, aes(x = Country, y = Species, fill = n)) +
  geom_tile() +
  labs(
    title = "Researchers are editing the myostatin gene,\n associated with meat tenderness and\n musculature, a lot.",
    subtitle = "Myostatin, which limits the production of muscle fibers,\n subject to at least 59 experiments across a range of\n species. Approximately 32% of all experiments\n scraped from ISAAA involved editing the MSTN gene,\n where myostatin is encoded. Only 8\n experiments received regulatory approval.",
    x = "Country",
    y = "Species",
    fill = "Count"
    )

mstn_1

ggsave("mstn1_plot.pdf", width = 8, height = 6)


sum(isaaa$Genes == "MSTN", na.rm = TRUE)

MSTN_reg <- isaaa %>%
  filter(grepl("\\bMSTN\\b", Genes)) %>%
  filter(!is.na(Regulatory_Process)) %>%
  group_by(Species, Country, Regulatory_Process) %>%
  summarise(n = n(), .groups = "drop")

mstn_2 <- ggplot(MSTN_reg, aes(x = Country, y = Species, fill = Regulatory_Process)) +
  geom_tile() +
  labs(
    title = "Researchers are editing the myostatin gene,\n associated with meat tenderness and\n musculature, a lot.",
    subtitle = "Myostatin, which limits the production of muscle fibers,\n subject to at least 59 experiments across a range of\n species. Approximately 32% of all experiments\n scraped from ISAAA involved editing the MSTN gene,\n where myostatin is encoded. Only 8\n experiments received regulatory approval.",
    x = "Country",
    y = "Species",
    fill = "Regulatory Approval"
  )

mstn_2

ggsave("mstn2_plot.pdf", width = 8, height = 6)


  