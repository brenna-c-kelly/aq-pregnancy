


library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting



no2_dat_small <- no2_dat[c(1:1000), ]


n_rows <- nrow(no2_dat_small)
n_cols <- ncol(no2_dat_small)
df_array <- as.matrix(no2_dat_small)
names(no2_dat_small)

dim1 <- ncdim_def("dim1", "", seq(n_rows))
dim2 <- ncdim_def("dim2", "", seq(n_cols))

fillvalue <- 9999

var_def <- ncvar_def("no2_100101", "", list(dim1, dim2), fillvalue, prec = "double")

# create file
nc <- nc_create("example.nc", list(var_def))
ncvar_put(nc, var_def, df_array)

# Close the file:
nc_close(nc)

# Check the file (retrieve the dimensions and the values of df):
example_nc <- nc_open("example.nc")
ncvar_get(example_nc, "dim1")
ncvar_get(example_nc, "dim2")
ncvar_get(example_nc, "no2_100101")
nc_close(example_nc)

print(example_nc)



#slice
example_nc[, 10]


#nc_create(df_array, names(no2_dat_small), force_v4=FALSE, verbose=FALSE)


ArrayToNc(arrays = as.matrix(no2_dat), file_path = 'example.nc')

as.matrix(no2_dat)

ArrayToNc(arrays = no2_100101, file_path = 'example.nc')

nc_test <- nc_open('example.nc')
plot(nc_test$groups)

nc_test$dim
