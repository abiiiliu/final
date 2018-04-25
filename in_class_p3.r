  N <- 8
  census <- read.csv('us_census.csv')
  #contiguous <- census
  contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'), ]
  
  #plot(contiguous$longitude, contiguous$latitude, type='p', col=contiguous$state)
  
  chosen_counties <- sample(1:nrow(contiguous), N)
  
  #print(chosen_counties)
  
  centers <- matrix(0, nrow = N, ncol = 2)
  
  #for (i in 1:N) {
  #    centers[i][1] = contiguous[chosen_counties[i],'latitude']
  #    centers[i][2] = contiguous$longitude[chosen_counties[i]]
  #}
  
  centers[, 1] = contiguous$latitude[chosen_counties]
  centers[, 2] = contiguous$longitude[chosen_counties]
  
  #print(centers)
  # as a data frame
  centers_df = contiguous[chosen_counties, 3:4]
  
  #print(contiguous[1,])
  #print(centers[1,])
  
  dist_sq <- function(county, center) {
    deltax <- latitudes[county]- center[1]
    deltay <- longitudes[county] - center[2]
    deltax ^ 2 + deltay ^ 2
  }
  
  #deltax <- contiguous[1, 'latitude'] - centers[1,1]
  #deltay <- contiguous[1, 'longitude'] - centers[1,2]
  #print(dist_sq(contiguous[1,], centers[1,]))
  
  # belongs_to[i] means the ith county belongs_to the cluster at
  # belongs_to[i]
  belongs_to <- rep(0, nrow(contiguous))
  
  # figure out closest cluster
  for (county in 1:nrow(contiguous)) {
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[county, ], centers[1, ])
    
    for (cluster in 2:N) {
      d <- dist_sq(county, centers[cluster, ])
      if (d < closest_distance) {
        closest_distance <- d
        closest_center <- cluster
      }
    }
    belongs_to[county] <- closest_center
  }
  # print(belongs_to)
  plot(contiguous$longitude,
       contiguous$latitude,
       type = 'p',
       col = belongs_to)
  
  clust_of_interest <- contiguous[belongs_to == 1, ]
  total_pop <- sum(clust_of_interest$population)
  # print(sum(clust_of_interest$population))
  new_latitude <-
    sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
  new_longitude <-
    sum(clust_of_interest$longitude * clust_of_interest$population) / total_pop
  centers[1, 1] <- new_latitude
  centers[1, 2] <- new_longitude
