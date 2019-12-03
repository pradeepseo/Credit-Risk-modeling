require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)

## one at a time, table apply, pared, and public
lapply(dat[, c("apply", "pared", "public")], table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = dat))

summary(dat$gpa)

sd(dat$gpa)

ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

## fit ordered logit model and store results 'm'
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs

(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality

## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))


sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf)))

glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)

glm(I(as.numeric(apply) >= 3) ~ pared, family="binomial", data = dat)

