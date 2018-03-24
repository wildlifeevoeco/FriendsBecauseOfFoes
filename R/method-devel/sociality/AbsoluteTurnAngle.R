### Absolute Turn Angle
# Alec Robitaille
# Started March 15 2018

# For calculation of absolute turn angle between 
# point at time and time+1 


# Math:
# arctan(dif-y, dif-x) * 180 / pi


# Pseudo code

# Shift points 1 row

# atan2

# Ensure it is by ID


atan2(dist(x), diff_x(x)) * 180 / pi
locs[1:4, .(EASTING, amt:::diff_rcpp(EASTING),
            NORTHING, amt:::diff_rcpp(NORTHING),
            atan2(amt:::diff_rcpp(NORTHING), amt:::diff_rcpp(EASTING)) * 180 / pi)]
qplot(EASTING, NORTHING, color = V1, data = locs[1:4])

atan2(amt:::diff_rcpp(locs$NORTHING), amt:::diff_rcpp(locs$EASTING)) * 180 / pi

x <- amt::mk_track(locs[COLLAR_ID == 1810], X_COORD, Y_COORD, timeGroup)
amt::as_sp()

library(amt)
xx <- sp::coordinates(as_sp(x))
c((450 + ((360 - geosphere::bearing(xx[-nrow(xx), ], xx[-1, ]))) %% 360) %% 360, NA)

locs[1:4, 
     {print(qplot(EASTING, NORTHING, color = V1))
       atan2(amt:::diff_rcpp(NORTHING), amt:::diff_rcpp(EASTING)) * 180 / pi}
     ]
