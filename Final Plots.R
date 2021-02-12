# Global Options

options(stringsAsFactors = FALSE)

# Load Packages

library(vars)
library(readxl)
library(tidyverse)
library(svars)

# Load Dataset

BRICS = read_excel("BRICS.xlsx")

# Exploratory Figures

BRICS_Wide = data.frame(
  Date = as.Date(format(
    strptime(as.character(BRICS$Date), "%Y-%m-%d"), "%Y-%m-%d"
  )),
  Country = as.factor(BRICS$Country),
  GrowthC = as.double(BRICS$Gc),
  GrowthR = as.double(BRICS$Gr),
  GrowthD = as.double(BRICS$Gd),
  GrowthER = as.double(BRICS$Gex),
  GrowthSV = as.double(BRICS$Gsv)
)

XMat = as.matrix.data.frame(BRICS_Wide[, -(1:2)])
XMat = scale(XMat)

BRICS_Wide[, -(1:2)] = XMat

BRICS_Long = BRICS_Wide %>% gather(Variable, Value, GrowthC:GrowthSV)
BRICS_Long$Variable = factor(
  BRICS_Long$Variable,
  levels = c("GrowthC",
             "GrowthD",
             "GrowthR",
             "GrowthER",
             "GrowthSV")
)

Figure.1 = ggplot(data = BRICS_Long, mapping = aes(x = Date, y = Value)) +
  geom_line(mapping = aes(group = Country, color = Country)) + facet_wrap(vars(Variable), nrow = 1) +
  ylim(-2, 2) + scale_x_date(date_labels = "%d %b", date_breaks  = "15 days") +
  theme_classic() +
  theme(text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90)) +
  ggtitle(label = "Summary of Growth Variables (Scaled and Centered)")

ggsave(
  filename = "Figure 1.pdf",
  plot = Figure.1,
  dpi = 360,
  width = 15,
  height = 8
)

ggsave(
  filename = "Figure 1.png",
  plot = Figure.1,
  width = 15,
  height = 8
)

# Model Fitting Figures

BRICS_Wide = na.omit(BRICS_Wide)

Country.Dummies = model.matrix(~ BRICS_Wide$Country)
colnames(Country.Dummies) = levels(BRICS_Wide$Country)
BRICS_Wide = data.frame(BRICS_Wide, Country.Dummies)

Lag.Choose = VARselect(BRICS_Wide[, 3:7],
                       lag.max = 30,
                       type = "both",
                       exogen = BRICS_Wide[, 8:12])

Lags = sort(unique(Lag.Choose[["selection"]]))
## Choices of lag: 2 Days, 1 Week, approx. 1 Fortnight.

AMat = diag(1, 5)
AMat[lower.tri(AMat)] <- NA

## Only doing irf as of now. Can do other diagnostics (ARCH, normality, serial correlation, FEVD, Prediction, Stability).
## But since this is a smaller paper and the IRFs will yield a lot of discussion, not caring too much about those now.

for (i in 1:length(Lags))
{
  Lag = Lags[i]
  
  VAR = VAR(BRICS_Wide[, 3:7],
            p = Lag,
            type = "both",
            exogen = BRICS_Wide[, 8:12])
  SVAR = id.ngml(VAR)
  IRF = irf(SVAR)
  
  Figure.Temp = plot(IRF) +
    theme_classic() +
    theme(text = element_text(face = "bold")) +
    ggtitle(label = paste0("Impulse Response Functions from SVAR Model (Lag = ", Lag, ")"))
  
  ggsave(
    filename = paste0("Figure ", i + 1, ".pdf"),
    plot = Figure.Temp,
    dpi = 360,
    width = 9,
    height = 9
  )
  
  ggsave(
    filename = paste0("Figure ", i + 1, ".png"),
    plot = Figure.Temp,
    width = 9,
    height = 9
  )
}

# Saving Results

save.image(file = "Results.rda")