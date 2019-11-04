library(readr)
library(tidyverse)
ventas <- read_csv("retaildataset/sales data-set.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
#View(ventas)

summary(ventas)

#ventas %>% group_by(Date) %>% count(Store)

#ventas %>% ggplot() + geom_bar(mapping = aes(x=Store))

ventas <- ventas %>% select(-IsHoliday)

# Hacemos una tabla pivote con las fechas en las columnas
ventas_g <- ventas %>% 
  spread(key = Date, value= Weekly_Sales) %>% 
  filter(Store<3 ) %>% 
  unite (ID, Store, Dept) %>% 
  #mutate_each( list( interp( ~replace(., is.na(.),0.01) ) ) ) %>% 
  mutate_all(list(~replace_na(., 0)))

# Quitamos el id
prods_ids <- ventas_g$ID
ventas_g$ID <- NULL
#str(ventas_g)


#summary(ventas_g)
#view(ventas_g)

#res.dist <- dist(ventas_g, method = "euclidean") #%>% scale()
res.dist <- dist(ventas_g, method = "euclidean") #%>% scale()
#scale(res.dist)
res.hc <- hclust(d = res.dist, method = "ward.D2")
#library("factoextra")
#fviz_dend(res.hc, cex = 0.5)

cut_avg <- cutree(res.hc, k = 16)

plot(res.hc)
rect.hclust(res.hc , k = 16, border = 2:6)
abline(h = 16, col = 'red')

ventas_cl <- mutate(ventas_g, cluster = cut_avg)

tventas <- t(ventas_cl)

cc <- count(ventas_cl, cluster)

ggplot(ventas_cl, aes(x=area, y = perimeter, color = factor(cluster))) 
+ geom_point()


#Example plot

df <- data.frame(date = c("01-04-2001 00:00","01-04-2001 00:00","01-04-2001 00:00",
                          "01-05-2001 00:00","01-05-2001 00:00","01-05-2001 00:00",
                          "01-06-2001 00:00","01-06-2001 00:00","01-06-2001 00:00",
                          "01-07-2001 00:00","01-07-2001 00:00","01-07-2001 00:00"), 
                 id = c(1,2,3,1,2,3,1,2,3,1,2,3), a = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                 b = c(2,2.5,3,3.2,4,4.6,5,5.6,8,8.9,10,10.6))

df2 <- gather(df, variable, value, -date, -id)
vars <- unique(df2$variable)

library(ggplot2)
for (i in 1:length(vars)) {
  ggplot() + 
    geom_line(data = subset(df2, variable == vars[[i]]), 
              aes(date, value, group = id, color = factor(id))) +
    ylab(as.character(vars[[i]])) 
    #ggsave(file = paste0(vars[[i]], ".png"))
}
ggplot() + 
  geom_line(data = subset(df2, variable == vars[[2]]), 
            aes(date, value, group = id, color = factor(id))) +
  ylab(as.character(vars[[i]])) 
