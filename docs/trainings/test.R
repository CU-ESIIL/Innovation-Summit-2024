require(terra)
require(tictoc)
system("cp -r ~/data-store/data/iplant/home/shared/earthlab/forest_carbon_codefest/TreeMap ~/TreeMap") #move the data first!!
tictoc::tic()
treemap <- terra::rast("~/TreeMap/treemap2016_southernrockies.tif")
terra::plot(treemap)
tictoc::toc()
