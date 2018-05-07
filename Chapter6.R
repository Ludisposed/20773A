# Lesson 1
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
clust <- rxKmeans(~ carat + depth + price,
                  data = ggplot2::diamonds,
                  numClusters = 5, seed = 1979)

# print(clust)
clust$betweenss / clust$totss

# Standerdizing data as it is clustered
clust_stand <- rxKmeans(~ carat_s, depth_s, price_s,
                        data = ggplot2::diamonds,
                        numClusters = 5,
                        seed = 1979,
                        transforms = list(
                          carat_s = (carat - mean(carat)) / sd(carat),
                          depth_s = (depth - mean(depth)) / sd(depth),
                          price_s = (price - mean(price)) / sd(price)
                        )
                       )

clust_stand$betweenss / clust_stand$totss

# Lesson 2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Linear Regression Models