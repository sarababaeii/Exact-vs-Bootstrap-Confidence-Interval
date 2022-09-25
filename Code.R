student_number = 9713005
b = 10000
n = 16

set.seed(student_number)
x = rnorm(n, 5, 2)

sample_mean = mean(x)
sample_med = median(x)

boot_samples = matrix(sample(x, size=b*n, replace=TRUE), nrow=B, ncol=n)

boot_sample_mean = rep(0, b)
boot_sample_med = rep(0, b)

for (i in 1:b) {
	boot_sample_mean[i] = mean(boot_samples[i,])
	boot_sample_med[i] = median(boot_samples[i,])
}

boxplot(boot_sample_mean, las=1, xlab="feed", main="Weight by Feed")

sample_mean
sample_med

mean(boot_sample_mean >= sample_mean)
mean(boot_sample_med >= sample_med)

quantile(x, prob=0.025)
quantile(x, prob=0.975)

# 1. percentile bootsrtap confidence interval

#for the difference in means
quantile(boot_sample_mean, prob=0.025)
quantile(boot_sample_mean, prob=0.975)

#for the difference in medians
quantile(boot_sample_med, prob=0.025)
quantile(boot_sample_med, prob=0.975)

# 2. bootsrtap confidence interval
sort(boot_sample_mean)[b * 0.5]
sort(boot_sample_mean)[b * (1 - 0.5)]


# bootsrtap variance
boot_variance = function (b, n, boot_samples) {
	var = (b)
	for (i in 1:b) {
		var[i] = var(boot_samples[i,])
	}
	return (var)
}

boot_var = boot_variance(b, n, boot_samples)
mean(boot_var)

sample_var = var(x)
sample_var

sort(boot_var)[b * 0.5]
sort(boot_var)[b * (1 - 0.5)]