
# library --------------

# load the necessary packages
library(stringr)
library(fastverse)
library(XML)
library(ggplot2)
library(ggpubr)
library(ggh4x)
library(geomtextpath)
library(ragg)

#  Load the required font ---------

library(systemfonts)

register_variant(
  name = "Heebo-Medium", 
  family = "Heebo", 
  weight = "medium")

# Read the data from the HTML of the Knesset website -----------------
#> Download the website below and save it in HTML format
#> https://main.knesset.gov.il/About/History/Pages/Lobby.aspx



k_wab <-   XML::htmlParse("row_data/knesset/knesset.html")
 
# Extract the election dates -------------


elections <-  xpathSApply(k_wab ,path = '//*[@data-bind="text: ElectionDate"]',getChildrenStrings)

# Change to date format --------

elections %<>% str_extract_all("\\d+",simplify = TRUE)


elections_date <- paste(elections[,3],elections[,2],elections[,1],sep = "-")

# as data.table ---------

df1 <- data.table(elections_date)


df1[,elections_date := as.IDate(elections_date)]

# add vars ---------

setorder(df1,elections_date)


df1[,knesset := 1:.N]

df1[, start := elections_date]
df1[, end := shift(elections_date,-1)]

## Last date  == today ---------

df1[.N,end := as.IDate( Sys.Date() )]


# Average length of the Knesset. Until the 21st Knesset ---------

df1[,k_len := mean(end[knesset<21]- start[knesset<21])]

#  Calculation of the end of Knesset according to the average length ----

df1[, avr_end := as.IDate(start+k_len)]


# Calculation of the years of the country's place until today in a sequence of five years --

years_seq <- seq.Date(from = as.Date("1947-01-01"),to = as.Date("2025-01-01"),"5 years")


# Calculation of the expected year for the 25th Knesset according to the average----


k_lm <-   lm(knesset~start,df1[knesset<21])

d_end <- (25-k_lm$coefficients[1])/k_lm$coefficients[2]


k_25_expc <- as.IDate(d_end+k_lm$coefficients[1])

# add to years seq--------

years_seq_2 <- c(years_seq,k_25_expc)


# Tools for graphs ----
smooth_seq <- seq(min(df1$start),k_25_expc,length.out = 80)

smooth_seq <- as.numeric(smooth_seq)


# english graph ------

ggplot(data = df1) +
  geom_textvline(label = "today", xintercept = as.Date(Sys.Date()), linetype = 3, size = 10, family = "Heebo-Medium") +
  geom_textsmooth(data = df1[knesset < 21], aes(x = start, y = knesset),
                  method = "lm", fullrange = TRUE, color = "#6A6599FF",
                  linetype = 1, se = FALSE, linewidth = 1.2, xseq = smooth_seq,
                  label = "A new Knesset every 1282 days", vjust = -0.2, size = 10, family = "Heebo-Medium") +
  geom_rect(
    aes(xmin = start, xmax = end, ymin = knesset - 0.1, ymax = knesset + 0.1),
    fill = "#DF8F44FF", color = "black", linewidth = 0.4
  ) +
  geom_rect(
    aes(xmin = start, xmax = avr_end, ymin = knesset - 0.2, ymax = knesset - 0.2),
    color = "black"
  ) +
  geom_rect(
    aes(xmin = start, xmax = avr_end, ymin = knesset + 0.2, ymax = knesset + 0.2),
    color = "black"
  ) +
  scale_x_date(breaks = years_seq_2, date_labels = "%Y", date_minor_breaks = "year") +
  scale_y_continuous(breaks = 1:25) +
  xlab("date") +
  theme_pubclean(base_size = 16, base_family = "Heebo-Medium")


file_name <- 
  paste0("plot/","knesset_english.png")

ggsave(file_name,plot = last_plot(),device = ragg::agg_png,width = 12,height = 10,bg = "white",)



# graph in Hebrew ----------

## Dealing with difficulties in combining Hebrew letters and numbers -------

new_knesset <- 
  "ימים\b 1282 \n כנסת חדשה כל "

new_knesset  <- paste("\u202B", new_knesset)

# the graph  -------

ggplot(data = df1) +
  geom_textvline(label = "היום", xintercept = as.Date(Sys.Date()), linetype = 3, size = 10, family = "Heebo-Medium") +
  geom_textsmooth(data = df1[knesset < 21], aes(x = start, y = knesset),
                  method = "lm", fullrange = TRUE, color = "#6A6599FF",
                  linetype = 1, se = FALSE, linewidth = 1.2, xseq = smooth_seq,
                  label = new_knesset, vjust = -0.2, size = 10, family = "Heebo-Medium") +
  geom_rect(
    aes(xmin = start, xmax = end, ymin = knesset - 0.1, ymax = knesset + 0.1),
    fill = "#DF8F44FF", color = "black", linewidth = 0.4
  ) +
  geom_rect(
    aes(xmin = start, xmax = avr_end, ymin = knesset - 0.2, ymax = knesset - 0.2),
    color = "black"
  ) +
  geom_rect(
    aes(xmin = start, xmax = avr_end, ymin = knesset + 0.2, ymax = knesset + 0.2),
    color = "black"
  ) +
  scale_x_date(breaks = years_seq_2, date_labels = "%Y", date_minor_breaks = "year") +
  scale_y_continuous(breaks = 1:25) +
  xlab("") +
  ylab("כנסות") +
  theme_pubclean(base_size = 16, base_family = "Heebo-Medium")


file_name <- 
  paste0("plot/","knesset_hebrew.png")

ggsave(file_name,plot = last_plot(),device = ragg::agg_png,width = 12,height = 10,bg = "white",)




