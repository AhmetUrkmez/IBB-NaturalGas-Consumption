library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

font_add_google("Roboto")
showtext_auto()

theme_set(theme_minimal(base_family = "Roboto"))
theme_update(
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40"),
    axis.text.x = element_text(
                size = 14,
                margin = margin(t = 5),
                angle = 65,
                vjust = 0.6),
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    axis.ticks = element_line(color = "grey91", linewidth = .5),
    axis.ticks.length.x = unit(1.3, "lines"),
    axis.ticks.length.y = unit(.7, "lines"),
    
    panel.grid = element_blank(),
    plot.margin = margin(20, 40, 20, 40),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    panel.background = element_rect(fill = "grey98", color = "grey98"),

    plot.title = element_text(
        color = "grey10", 
        size = 22, 
        face = "bold",
        margin = margin(t = 15)
    ),

    plot.subtitle = element_markdown(
        color = "grey30", 
        size = 14,
        lineheight = 1.35,
        margin = margin(t = 15, b = 40)
    ),

    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
        color = "grey30", 
        size = 12,
        lineheight = 1.2, 
        hjust = 0,
        margin = margin(t = 40)
    ),

    legend.position = "none"
)

Converter <- function(text) {
    turkish_chars <- c("ç", "Ç", "ğ", "Ğ", "ı", "İ",
                        "ö", "Ö", "ş", "Ş", "ü", "Ü")
    english_chars <- c("c", "C", "g", "G", "i", "I",
                        "o", "O", "s", "S", "u", "U")

    for (i in 1: length(turkish_chars)) {
        text <- gsub(turkish_chars[i], english_chars[i], text)
    }

    return(text)
}

data <- readxl::read_excel("data.xlsx")

labels <- c("YIL", "AY", "ILCE", "TUKETIM")
ILCE3 <- c("BESIKTAS", "CATALCA", "ESENYURT")
AYLAR <- c("Ocak", "Şubat", "Mart", "Nisan", "Mayıs", "Haziran",
        "Temmuz", "Ağustos", "Eylül", "Ekim", "Kasım", "Aralık")

data2022 <- data %>%
    rename_with(~labels, everything()) %>%
    filter(YIL == 2022) %>%
    mutate(ILCE = as.factor(ILCE)) %>%
    mutate(ILCE = factor(ILCE, labels = Converter(unique(ILCE)))) %>%
    mutate_at(vars(ILCE), as.character) %>%
    mutate(ILCE3 = ifelse(ILCE %in% ILCE3, ILCE, "DIGER")) %>%
    mutate(
        ILCE3 = fct_relevel(ILCE3, "DIGER", after = Inf),
        name_lab = if_else(AY == 12, ILCE, NA_character_)
    )

str(data2022)

plt <- ggplot(
    data2022 %>% filter(ILCE3 != "DIGER"),
    aes(x = AY, y = TUKETIM, group = ILCE)
    ) +
    geom_vline(
        xintercept = seq(1, 12, by = 1),
        color = "grey91", 
        linewidth = .5
    ) +
    geom_segment(
        data = tibble(y = seq(0, 8*10**7, by = 10**7), x1 = 1, x2 = 12),
        aes(x = x1, xend = x2, y = y, yend = y),
        inherit.aes = FALSE,
        color = "grey91",
        lwd = .5
    ) +
    geom_line(
        data = data2022 %>% filter(ILCE3 == "DIGER"),
        color = "grey75",
        linewidth = .5,
        alpha = .5
    ) +
    geom_line(
        aes(color = ILCE),
        linewidth = .9
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(1, 12.5), 
        breaks = seq(1, 12, by = 1),
        labels = AYLAR, 
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(0, 8*10**7, by = 2*10**7),
        labels = scales::label_number(scale = 1e-6, suffix = "M")
    )

plt <- plt +
    geom_text_repel(
        aes(color = ILCE3, label = name_lab),
        family = "sans",
        fontface = "bold",
        size = 3.5,
        direction = "y",
        xlim = c(13, NA),
        hjust = 0,
        segment.size = .7,
        segment.alpha = .5,
        segment.linetype = "dotted",
        box.padding = .4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20
    ) +
    coord_cartesian(
        clip = "off",
        ylim = c(0, 8*10**7)
    )

plt <- plt +
    scale_color_manual(
        values = c(rcartocolor::carto_pal(n = length(ILCE3), name = "Bold")[1:length(ILCE3)-1], "grey50")
    ) +
    labs(
        title = "İSTANBUL BÜYÜKŞEHİR BELEDİYESİ 2022 YILI AYLIK DOĞALGAZ TÜKETİMİ",
        subtitle = "Tüm ilçelerin doğalgaz tüketim miktarı milyon metreküp cinsinden grafiğe dökülmüştür. Esenyurt, Beşiktaş ve Çatalca ilçeleri ön plana çıkarılmıştır.",
        caption = "İBB açık veri portalından elde edilen veriler kullanılmıştır."
    )

plt
