# toy code for getting use to Chaisemartin work


# DGP ---------------------------------------------------------------------

library(dplyr)
set.seed(123)
TT <- 20; GG <- 5
df <- data.frame(id = 1:(GG*TT))
df$G <- ((df$id-1) %% GG)+1
df$T <- floor((df$id-1)/GG)
df$id <- NULL
df <- df[order(df$G, df$T), ]
df$D <- 0
for (v in c(2,3,4,5)) {
  df$D <- ifelse(df$G == v & df$T == v+2, 1, df$D)
}
df <- df %>% group_by(.data$G) %>% 
  mutate(D_stag = cumsum(.data$D)) %>% ungroup()
df$Y <- ifelse(df$T %% 4 == 0, 
               runif(n = nrow(df)) * (1 + 100*df$D_stag), NA)
df$D_stag <- NULL

# Data Adjustment ---------------------------------------------------------

## Whether treatment has changed within any group --------------------------
df$D0 <- df$D[(df$G-1)*length(levels(factor(df$T)))+1]
df$D_change <- as.numeric(abs(df$D - df$D0) != 0)
df <- df %>% group_by(.data$G) %>% 
  mutate(at_least_one_D_change = cumsum(.data$D_change)) %>% ungroup()

## When did it changed for the first time ----------------------------------

df <- df %>% group_by(.data$G) %>% 
  mutate(never_treated = as.numeric(sum(.data$D_change, na.rm = TRUE) == 0)) %>%
  mutate(F_g = ifelse(.data$never_treated == 1, max(df$T, na.rm = TRUE) +1, 
                      min(ifelse(.data$D_change == 0, NA, .data$T * .data$D_change), na.rm = TRUE))) %>% 
  ungroup()
df$subsample <- (4 - (df$F_g %% 4)) * (df$F_g %% 4 != 0) + 1
df$subsample <- df$subsample * df$at_least_one_D_change

## Only keep non missing outcome variable ----------------------------------

df <- subset(df, !is.na(df$Y))

# Estimation --------------------------------------------------------------
library(DIDmultiplegtDYN)

did_multiplegt_dyn(df, "Y", "G", "T", "at_least_one_D_change", graph_off = TRUE, effects = 1)

effects <- 2
table <- NULL
for (j in 1:4) {
  temp <- did_multiplegt_dyn(
    subset(df, df$subsample %in% c(0, j)), "Y", "G", "T", "at_least_one_D_change", 
    graph_off = TRUE, effects = effects)
  rownames(temp$results$Effects) <- 
    sapply(1:temp$results$N_Effects, function(x) paste0("Effect_",  j + (x-1) * 4))
  table <- rbind(table, temp$results$Effects)
}
rown <- unlist(strsplit(rownames(table), "_")) 
table <- cbind(table, as.numeric(rown[rown != "Effect"]))
print(table[order(table[,ncol(table)]),1:(ncol(table)-1)])

# Output ------------------------------------------------------------------
library(ggplot2)
table <- table[order(table[,ncol(table)]), ]
table <- rbind(rep(0, ncol(table)), table)
colnames(table)[ncol(table)] <- "Time"
table <- as.data.frame(table)
out_plot <- ggplot(table, aes(x = .data$Time, y = .data$Estimate, group = 1)) + 
  geom_line(colour = "blue") +
  geom_errorbar(data = ~dplyr::filter(.x, table$Estimate != 0), aes(ymin = .data[["LB CI"]], ymax = .data[["UB CI"]]), 
                position=position_dodge(0.05), width = 0.2, colour = "red") + 
  geom_point(colour = "blue") + 
  ggtitle("DID, from last period before treatment changes (t=0) to t") + 
  xlab("Relative time to last period before treatment changes (t=0)") +
  theme(plot.title = element_text(hjust = 0.5))
print(out_plot)


