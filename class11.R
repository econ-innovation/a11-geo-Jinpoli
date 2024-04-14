library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(sf)          
library(terra)      
library(spData)        
library(spDataLarge)
library(dplyr)

# Q1
# 读取合肥经济技术开发区地理数据
dv_zone <- read_sf(dsn = "~/bigdata/myRworkingdir/G341022合肥经济技术开发区.txt")
dv_zone <- st_as_sf(dv_zone, crs = 4326)
class(dv_zone)
plot(dv_zone)

# 读取企业地址坐标数据
firmfilepath = "~/bigdata/myRworkingdir/hefei.txt"
firm_add <- read.table(firmfilepath, sep="\t", header = TRUE)
firm_addresses <- st_as_sf(firm_add, coords = c("lng", "lat"), crs = 4326)
class(firm_addresses)

# 筛选位于开发区内的企业
lst = st_intersects(firm_addresses, dv_zone)
lst_logical <- unlist(lst)
firm_addresses_within <- firm_addresses[lst_logical, ]
# 输出位于开发区内的企业数量
num_firm_inside_area <- nrow(firm_addresses_within)

# 创建以开发区边界为中心的缓冲区
dv_zone_buffer1 <- st_buffer(dv_zone, dist = 1000)  # 1km
dv_zone_buffer3 <- st_buffer(dv_zone, dist = 3000)  # 3km
dv_zone_buffer5 <- st_buffer(dv_zone, dist = 5000)  # 5km

# 计算企业地址是否位于缓冲区内
# 1km
lst1 = st_intersects(firm_addresses, dv_zone_buffer1)
lst1_logical <- unlist(lst1)
firm_within_1km <- firm_addresses[lst1_logical, ]
# 3km
lst3 = st_intersects(firm_addresses, dv_zone_buffer3)
lst3_logical <- unlist(lst3)
firm_within_10km <- firm_addresses[lst3_logical, ]
# 5km
lst5 = st_intersects(firm_addresses, dv_zone_buffer5)
lst5_logical <- unlist(lst5)
firm_within_5km <- firm_addresses[lst5_logical, ]

# 计算开发区内的企业数量
num_firm_within_1km <- nrow(firm_within_1km)
num_firm_within_3km <- nrow(firm_within_3km)
num_firm_within_5km <- nrow(firm_within_5km)

# 输出结果
cat("Number of enterprises inside the development area: ", num_firm_inside_area, "\n")
cat("Number of enterprises within 1km: ", num_firm_within_1km, "\n")
cat("Number of enterprises within 3km: ", num_firm_within_3km, "\n")
cat("Number of enterprises within 5km: ", num_firm_within_5km, "\n")


#Q2
library(ggplot2)
# 读取每个开发区的地理边界数据并转换为sf对象
dv1_zone <- read_sf(dsn = "~/bigdata/myRworkingdir/G342020合肥高新技术产业开发区区块一.txt")
dv2_zone <- read_sf(dsn = "~/bigdata/myRworkingdir/G342020合肥高新技术产业开发区区块二.txt")

dv1_zone <- st_as_sf(dv1_zone, crs = 4326)
dv2_zone <- st_as_sf(dv2_zone, crs = 4326)

# 合并所有开发区的sf对象
all_development_zones <- bind_rows(dv1_zone, dv2_zone)
class(all_development_zones)
plot(all_development_zones)

# 筛选位于开发区内的企业
intersects = st_intersects(firm_addresses, all_development_zones)

intersects_logical <- sapply(intersects, function(x) length(x) > 0)

firm_addresses_intersects <- firm_addresses[intersects_logical, ]
# 输出位于开发区内的企业数量
print(firm_addresses_intersects)


# 创建地图可视化
ggplot() +
  geom_sf(data = all_development_zones, fill = "lightblue") +  # 绘制开发区边界
  geom_sf(data = firm_addresses_intersects, color = "red", shape = 16, size = 0.1) +  # 绘制企业地址
  labs(title = "Development Zones in Baoding City with Enterprises") +
  theme_minimal()


# Q3
# 读入数据
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
rast_template = rast(ext(srtm), res = 0.01)
srtm_resampl1 = resample(srtm, y = rast_template, method = "bilinear")
srtm_resampl2 = resample(srtm, y = rast_template, method = "near")
srtm_resampl3 = resample(srtm, y = rast_template, method = "cubic")
srtm_resampl4 = resample(srtm, y = rast_template, method = "cubicspline")
srtm_resampl5 = resample(srtm, y = rast_template, method = "lanczos")

# 合并展示
srtm_resampl_all = c(srtm_resampl1, srtm_resampl2, srtm_resampl3,
                     srtm_resampl4, srtm_resampl5)
plot(srtm_resampl_all)

# 差异
plot(srtm_resampl_all - srtm_resampl1, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl2, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl3, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl4, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl5, range = c(-300, 300))



# Q4
library(geojsonsf)
library(rjson)
library(rlist)
hefei <- geojson_sf("~/bigdata/myRworkingdir/hefei.json") 

ggplot() +
  geom_sf(data = hefei, fill = "lightblue") +  # 绘制开发区边界
  geom_sf(data = firm_addresses, color = "red", shape = 16, size = 0.1) +  # 绘制企业地址
  labs(title = "firm location in HEFEI") +
  theme_minimal()

