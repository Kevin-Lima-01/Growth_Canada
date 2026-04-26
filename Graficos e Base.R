# Establencendo os pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)

# Estabelecndo as bases
df <- read_excel("Documents/Trabalho Macro/PIB per capita.xls")
df <- df %>% slice(-(1:2))
nomes_colunas <- as.character(unlist(df[1, ]))  
df <- df[-1, ]                               
names(df) <- nomes_colunas
df <- df %>% select(-3, -4)

# Paises de interesse
df <- df %>%
  filter(`Country Code` %in% c("CAN", "USA", "GBR", "AUS", "OED"))


df_fin <- df %>%
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "Ano",
    values_to = "PIB_per_capita"
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    PIB_per_capita = as.numeric(PIB_per_capita)
  ) %>%
  filter(Ano >= 1980) %>%  # Filtra apenas anos a partir de 1980
  filter(`Country Code` %in% c("AUS", "CAN", "GBR", "OED", "USA"))  # Filtra países novamente para garantir


# Fazendo os plots
cores_paises <- c(
  "AUS" = "#00843D",       # Verde australiano
  "CAN" = "#D80621",       # Vermelho canadense
  "GBR" = "#012169",       # Azul britânico
  "OED" = "#777777",       # Cinza para OECD
  "USA" = "#3C3B6E"        # Azul americano
)

# Plot 1
ggplot(df_fin %>% filter(`Country Code` %in% c("AUS", "CAN", "GBR", "OED", "USA")), 
       aes(x = as.numeric(Ano), y = as.numeric(PIB_per_capita), 
           color = `Country Code`)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(1960, 2023, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ano", y = "PIB per capita") +
  theme_minimal()

# Plot 2
ggplot(df_fin %>% filter(`Country Code` %in% c("AUS", "CAN", "GBR", "OED", "USA")), 
       aes(x = as.numeric(Ano), y = as.numeric(PIB_per_capita), 
           color = `Country Code`)) +
  
  
  geom_line(data = . %>% filter(`Country Code` != "CAN"), 
            linewidth = 0.8, alpha = 0.7) +
  
  
  geom_line(data = . %>% filter(`Country Code` == "CAN"), 
            linewidth = 2.0, alpha = 1) +
  
  
  
  scale_x_continuous(breaks = seq(1960, 2023, 10)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  
  
  scale_color_manual(values = cores_paises,
                     labels = c("Austrália", "Canadá", "Reino Unido", "OECD", "EUA")) +
  
  
  labs(x = "Ano", y = "PIB per capita (USD)",
       title = "Evolução do PIB per capita (1980-2023)",
       color = "País") +
  
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(linewidth = 0.1),
    panel.grid.minor = element_blank()
  )


