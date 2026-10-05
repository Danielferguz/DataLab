library("shrink")
library("survival")

# Load data -------------------------------------------------------
data(deepvein)

levels(deepvein$sex) <- paste(".", levels(deepvein$sex), sep = "")
levels(deepvein$loc) <- paste(".", levels(deepvein$loc), sep = "")
levels(deepvein$fiimut) <- paste(".", levels(deepvein$fiimut), sep = "")
levels(deepvein$fvleid) <- paste(".", levels(deepvein$fvleid), sep = "")

# Set the reference level for all categorical variables------------
deepvein$sex <- relevel(deepvein$sex, ref = ".female")
deepvein$fiimut <- relevel(deepvein$fiimut, ref = ".absent")
deepvein$fvleid <- relevel(deepvein$fvleid, ref = ".absent")
deepvein$loc <- relevel(deepvein$loc, ref = ".PE")

# EPV --------------------------------------------------------------
pred <- c("sex", "loc", "log2ddim", "durther", "fvleid", "fiimut", "age", "bmi")
epv <- sum(deepvein$status) / length(pred)
epv

# descriptive analysis ---------------------------------------------
median_foup <- survfit(Surv(time, abs(1-status)) ~ 1, data = deepvein)
median_foup

cont_pred <- c("log2ddim", "durther", "age", "bmi")
cat_pred <- c("sex", "loc", "fvleid", "fiimut")

summary(deepvein[, cont_pred])
sapply(deepvein[, cat_pred], function(x) round(prop.table(table(x)) * 100, 2))

# Estimate full model ----------------------------------------------
formula <-  paste("Surv(time, status) ~", paste(pred, collapse = "+"))
full_mod <- coxph(formula(formula), data = deepvein, x = TRUE)
coef_names <- names(full_mod$coefficients)
summary(full_mod)

# Selected Model ---------------------------------------------------
sel_mod <- step(coxph(formula(formula), data = deepvein),
                direction = "backward", trace = 0)
summary(sel_mod)
sel_est <- sel_mod$coefficients[names(full_mod$coefficients)]
sel_est[is.na(sel_est)] <- 0
sel_se <- coef(summary(sel_mod))[, "se(coef)"][coef_names]
sel_se[is.na(sel_se)] <- 0
sel_var <- grepl(paste(attr(terms(sel_mod), "term.labels"), 
                       collapse = "|"), pred) * 1
names(sel_var) <- pred

# Bootstrap --------------------------------------------------------
bootnum <- 1000
boot_est <- boot_se <- matrix(0, ncol = length(coef_names), nrow = bootnum,
                              dimnames = list(NULL, coef_names))
boot_var <- matrix(0, ncol = length(pred), nrow = bootnum, 
                   dimnames = list(NULL, pred))

set.seed(4135)
for (i in 1:bootnum) {
  data_id <- sample(1:dim(deepvein)[1], replace = T)
  boot_mod <- step(coxph(formula(formula), data = deepvein[data_id,]),
                   direction = "backward", trace = 0,
                   scope = list(upper = formula, 
                                lower = formula(Surv(time, status) ~ sex)))
  boot_est[i, names(boot_mod$coefficients)] <- boot_mod$coefficients
  boot_se[i, names(boot_mod$coefficients)] <- coef(summary(boot_mod)
                                                   )[, "se(coef)"]
  boot_var[i, attr(terms(boot_mod), "term.labels")] <- 1
}
boot_01 <- (boot_est != 0) * 1
boot_inclusion <- apply(boot_01, 2, function(x) sum(x) / length(x) * 100)

# Overview of estimates and measures --------------------------------
# Supplementary Table 4 in supporting information -------------------
sqe <- (t(boot_est) - full_mod$coefficients) ^ 2
rmsd <- apply(sqe, 1, function(x) sqrt(mean(x)))
rmsdratio <- rmsd / coef(summary(full_mod))[, "se(coef)"]
boot_mean <- apply(boot_est, 2, mean)
boot_meanratio <- boot_mean / full_mod$coefficients
boot_relbias <- (boot_meanratio / (boot_inclusion / 100) - 1) * 100
boot_median <- apply(boot_est, 2, median)
boot_025per <- apply(boot_est, 2, function(x) quantile(x, 0.025))
boot_975per <- apply(boot_est, 2, function(x) quantile(x, 0.975))

overview <- cbind(full_est = full_mod$coefficients,
                  full_se = coef(summary(full_mod))[,"se(coef)"],
                  boot_inclusion, sel_est, sel_se, rmsdratio, boot_relbias,
                  boot_median, boot_025per, boot_975per)
overview <- round(overview[order(overview[, "boot_inclusion"], 
                                 decreasing = T),], 4)
overview

# Model frequency ---------------------------------------------------
# Supplementary Table 5 in supporting information --------------------
boot_var_count <- cbind(boot_var, count = rep(1, times = bootnum))
boot_modfreq <- aggregate(count ~ ., data = boot_var_count, sum)
boot_modfreq[, "percent"] <- boot_modfreq$count / bootnum * 100
boot_modfreq <- boot_modfreq[order(boot_modfreq[, "percent"], decreasing = T),]
boot_modfreq[, "cum_percent"] <- cumsum(boot_modfreq$percent)
boot_modfreq <- boot_modfreq[boot_modfreq[, "cum_percent"] <= 80,]
if (dim(boot_modfreq)[1] > 20) boot_modfreq <- boot_modfreq[1:20,]

cbind("Predictors"= apply(boot_modfreq[,c(1:8)], 1, 
                          function(x) paste(names(x[x==1]), collapse=" ")),
      boot_modfreq[,c("count", "percent", "cum_percent")])

# Model frequency in % of selected model ----------------------------
sel_modfreq <- sum(apply(boot_var, 1, function(x)
  identical(sel_var, x))) / bootnum * 100

sel_modfreq

# Pairwise inclusion frequency in % ----------------------------------
# Supplementary Table 6 in supporting information --------------------
pval <- 0.01
boot_pairfreq <- matrix(100, ncol = length(pred), nrow = length(pred),
                        dimnames = list(pred,pred))

expect_pairfreq <- NULL
combis <- combn(pred, 2)

for (i in 1:dim(combis)[2]) {
  boot_pairfreq[combis[1, i], combis[2, i]] <-
    sum(apply(boot_var[, combis[, i]], 1, sum) == 2) / bootnum * 100
  expect_pairfreq[i] <-
    boot_inclusion[grepl(combis[1, i], names(boot_inclusion))][1] *
    boot_inclusion[grepl(combis[2, i], names(boot_inclusion))][1] / 100
  boot_pairfreq[combis[2, i], combis[1, i]] <- 
    ifelse(is(suppressWarnings(try(chisq.test(boot_var[, combis[1, i]],
                                              boot_var[, combis[2, i]]),
                                   silent = T)), "try-error"),
           NA, ifelse(suppressWarnings(
             chisq.test(boot_var[, combis[1, i]],
                        boot_var[, combis[2, i]])$p.value) > pval,
             "", ifelse(as.numeric(boot_pairfreq[combis[1, i], combis[2, i]]) < 
                          expect_pairfreq[i], "-", "+")))
}
diag(boot_pairfreq) <- apply(boot_var, 2, function(x) sum(x) / length(x) * 100)

print(boot_pairfreq, quote = F)
  
