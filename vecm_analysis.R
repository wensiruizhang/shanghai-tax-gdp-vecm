library(readxl)
library(urca)


data <- read_excel("shanghai_data.xlsx")

data$lnGDP <- log(data$GDP)
data$lnTAX <- log(data$TAX)


vars <- data.frame(lnGDP = data$lnGDP, lnTAX = data$lnTAX)


get_desc_stats <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  
  c(
    N = length(na.omit(x)),               
    Mean = mean_val,                      
    Std_Dev = sd_val,                     
    Min = min(x, na.rm = TRUE),          
    Max = max(x, na.rm = TRUE),          
    CV = sd_val / mean_val                
  )
}


stats_result <- t(sapply(vars, get_desc_stats))
stats_result <- round(stats_result, 3)
print(stats_result)

stats_result <- t(sapply(vars, get_desc_stats))


stats_result <- round(stats_result, 3)


print(stats_result)
library(ggplot2)
p <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = lnGDP, color = "Log GDP"), linewidth = 1.2) +
  geom_line(aes(y = lnTAX, color = "Log Tax Revenue"), linewidth = 1.2) +
  labs(
    title = "Long-term Trend of Tax Revenue and Economic Growth in Shanghai",
    x = "Year",
    y = "Log Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

print(p)



adf_lnGDP <- ur.df(data$lnGDP, type="trend", lags=2, selectlags="BIC")
summary(adf_lnGDP)


adf_lnTAX <- ur.df(data$lnTAX, type="trend", lags=2, selectlags="BIC")
summary(adf_lnTAX)

d_lnGDP <- na.omit(diff(data$lnGDP))
d_lnTAX <- na.omit(diff(data$lnTAX))



adf_d_lnGDP <- ur.df(d_lnGDP, type="drift", lags=2, selectlags="BIC")
summary(adf_d_lnGDP)


adf_d_lnTAX <- ur.df(d_lnTAX, type="drift", lags=2, selectlags="BIC")
summary(adf_d_lnTAX)



library(vars)


var_data <- data.frame(lnGDP = data$lnGDP, lnTAX = data$lnTAX)


lag_selection <- VARselect(var_data, lag.max = 4, type = "both")


criteria_matrix <- t(lag_selection$criteria)


academic_lag_table <- data.frame(
  Lag = 1:4,
  AIC = criteria_matrix[, 1],
  HQ  = criteria_matrix[, 2],
  SC  = criteria_matrix[, 3],
  FPE = criteria_matrix[, 4]
)


academic_lag_table[, 2:5] <- round(academic_lag_table[, 2:5], 4)


print(academic_lag_table, row.names = FALSE)

jo_test <- ca.jo(var_data, type = "trace", ecdet = "const", K = 2) 
summary(jo_test)

vecm_model <- cajorls(jo_test, r = 1)
vecm_to_var <- vec2var(jo_test, r = 1)
summary(vecm_model$rlm)

serial_test <- serial.test(vecm_to_var, lags.pt = 10, type = "PT.asymptotic")
print(serial_test)

arch_test <- arch.test(vecm_to_var, lags.multi = 5)
print(arch_test)
summary(vecm_model$rlm)
library(vars)

library(vars)
library(ggplot2)
library(patchwork) 

irf_result <- irf(vecm_to_var, n.ahead = 15, boot = TRUE, runs = 1000)

periods <- 0:15 
df_A <- data.frame(
  Horizon = periods,
  Response = irf_result$irf$lnTAX[, "lnGDP"],
  Lower = irf_result$Lower$lnTAX[, "lnGDP"],
  Upper = irf_result$Upper$lnTAX[, "lnGDP"]
)

df_B <- data.frame(
  Horizon = periods,
  Response = irf_result$irf$lnGDP[, "lnTAX"],
  Lower = irf_result$Lower$lnGDP[, "lnTAX"],
  Upper = irf_result$Upper$lnGDP[, "lnTAX"]
)

plot_A <- ggplot(df_A, aes(x = Horizon)) +
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E41A1C", linewidth = 0.8) +
  
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#377EB8", alpha = 0.2) +
  
  geom_line(aes(y = Response), color = "#08306B", linewidth = 1.2) +
 
  geom_point(aes(y = Response), color = "#08306B", size = 2) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), # Add black border
    plot.title = element_text(hjust = 0.5, face = "bold") # Center and bold title
  ) +
  labs(title = "(a): Response of lnGDP to lnTAX", x = "Horizon", y = "Response")


plot_B <- ggplot(df_B, aes(x = Horizon)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E41A1C", linewidth = 0.8) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#377EB8", alpha = 0.2) +
  geom_line(aes(y = Response), color = "#08306B", linewidth = 1.2) +
  geom_point(aes(y = Response), color = "#08306B", size = 2) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(title = "(b): Response of lnTAX to lnGDP", x = "Horizon", y = "Response")


final_plot <- plot_A + plot_B
print(final_plot)

library(vars)
vd_result <- fevd(vecm_to_var, n.ahead = 10)

print(round(vd_result$lnGDP * 100, 2))

print(round(vd_result$lnTAX * 100, 2))

data_gdp <- t(vd_result$lnGDP) * 100
data_tax <- t(vd_result$lnTAX) * 100


my_colors <- c("gray50", "gray90")


par(mfrow = c(2, 1), mar = c(4, 4, 3, 8), bg = "white", xpd = TRUE)


bp_gdp <- barplot(data_gdp, col = my_colors,
                  main = "(a): FEVD for lnGDP",
                  xlab = "Horizon", ylab = "Percentage (%)",
                  names.arg = 1:10, border = "black") 


legend("topright", inset = c(-0.2, 0), legend = rownames(data_gdp),
       fill = my_colors, bty = "n", cex = 1)


y_bottom_gdp <- data_gdp[1, ] / 2
y_top_gdp <- data_gdp[1, ] + data_gdp[2, ] / 2
text(bp_gdp, y_bottom_gdp, labels = ifelse(data_gdp[1,] > 1, round(data_gdp[1,], 1), ""), col = "white", cex = 0.85)
text(bp_gdp, y_top_gdp, labels = ifelse(data_gdp[2,] > 1, round(data_gdp[2,], 1), ""), col = "black", cex = 0.85)



bp_tax <- barplot(data_tax, col = my_colors,
                  main = "(b): FEVD for lnTAX",
                  xlab = "Horizon", ylab = "Percentage (%)",
                  names.arg = 1:10, border = "black") 


legend("topright", inset = c(-0.2, 0), legend = rownames(data_tax),
       fill = my_colors, bty = "n", cex = 1)


y_bottom_tax <- data_tax[1, ] / 2
y_top_tax <- data_tax[1, ] + data_tax[2, ] / 2
text(bp_tax, y_bottom_tax, labels = ifelse(data_tax[1,] > 1, round(data_tax[1,], 1), ""), col = "white", cex = 0.85)
text(bp_tax, y_top_tax, labels = ifelse(data_tax[2,] > 1, round(data_tax[2,], 1), ""), col = "black", cex = 0.85)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)

