
setwd("C:/Users/14702/OneDrive/Desktop/Emory/Social Network/Final Project")

library(data.table)
library(igraph)
library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
library(gtools)
library(proxy)
library(plyr)
library(MASS)
library(Matrix)
library(plm)

# import data 

df_2018 <- fread("2018.csv")

df_2017 <- fread("2017.csv")

df_2016 <- fread("2016.csv")

df_2015 <- fread("2015.csv")

df_2014 <- fread("2014.csv")

df_2013 <- fread("2013.csv")

df_2012 <- fread("2012.csv")

df_2011 <- fread("2011.csv")

df_2010 <- fread("2010.csv")

df_2009 <- fread("2009.csv")


spotify_df <- rbind(df_2018,df_2017,df_2016,df_2015,df_2014,df_2013,df_2012,df_2011,df_2010,df_2009)
spotify_df <-  spotify_df[,-1]

# test_df = fread("test_10000.csv")
# test_df <-  test_df[,-1]
# test_df <-  test_df[,-5]
# test_df$year=2018

#create empty edgelist data frame  
edgelist<-as.data.frame(matrix(ncol = 4))
colnames(edgelist)<-c('V1','V2','track_id','track_year')

years<- unique(spotify_df $track_year)
  
#get edge list for coreness calculation
for (i in years){
  for (j in unique(spotify_df [track_year==i,track_id])){
    comb<-as.character(unique(spotify_df [track_id==j,artist_id]))
    # put all key words of a film in one row
    if(length(comb)>1){
      edge<-as.data.frame(t(combn(comb,2)))
      # get the words combination by 2 
      # edge$year=i
      
      edge$track_id = j
      edge$track_year = i
      edgelist<-rbind(edgelist,edge)
    }
  }
  print(i) # to track the year 
}
edgelist<-na.omit(edgelist)

# popularity
edgelist$popularity <-NA
edgelist$duration
for (i in 1:nrow(edgelist)){
  edgelist[i,4]= df[test_df$track_name==edgelist[i,3],3] 
  edgelist[i,5]= df[test_df$track_name==edgelist[i,3],6] 
}

# get weight
edgelist_graph <- edgelist
edgelist<-ddply(edgelist,.(V1,V2),summarize,weight=length(V1))

# yearly social network statistics 
sn_degree<-as.data.frame(matrix(ncol = 3))
colnames(sn_degree)<-c("degree","track_year","artist_name")

for (i in 2009:2018){
  edgelist_sub <- edgelist[edgelist$Year==i]
  # edgelist_sub<-unique(edgelist_sub)
  graph_sub<-graph.data.frame(edgelist_sub, vertices = NULL, directed = FALSE)
  # E(graph_sub)$weight<-edgelist_sub$weight
  coreness=eigen_centrality(graph_sub,weight=NULL) 
  # pagerank=page.rank(graph_sub,weight=edgelist_sub$weight)
  v2_stat<-data.frame(degree=degree(graph_sub))
  v2_stat$track_year=i
  v2_stat$artist_name <- row.names(v2_stat)
  rownames(v2_stat)=NULL
  sn_degree<-rbind(sn_degree,v2_stat)
}
sn_degree<-na.omit(sn_degree)

# degree
sn_degree<-edgelist[,-2]
sn_degree<-unique(sn_degree)
colnames(sn_degree)<-c('artist_name','track_year','degree')

sn_core<-as.data.frame(matrix(ncol = 3))
colnames(sn_core)<-c("coreness","track_year","artist_name")

for (i in 2009:2018){
  edgelist_sub <- edgelist[edgelist$Year==i]
  # edgelist_sub<-unique(edgelist_sub)
  graph_sub<-graph.data.frame(edgelist_sub, vertices = NULL, directed = FALSE)
  # E(graph_sub)$weight<-edgelist_sub$weight
  coreness=eigen_centrality(graph_sub,weight=NULL) 
  # pagerank=page.rank(graph_sub,weight=edgelist_sub$weight)
  v2_stat<-data.frame( coreness=coreness$vector)
  v2_stat$track_year=i
  v2_stat$artist_name <- row.names(v2_stat)
  rownames(v2_stat)=NULL
  sn_core<-rbind(sn_core,v2_stat)
}
sn_core<-na.omit(sn_core)

hist(sn_degree$degree)
hist(sn_core$coreness)
hist(log(sn_core$coreness))
hist(log(sn_degree$degree))

# get graph
net1 = graph.data.frame(edgelist, vertices = NULL, directed = FALSE)
E(net1)$weight<-edgelist$weight
is.weighted(net1)

# artist SN statistic
coreness=eigen_centrality(net1,weight=edgelist$weight) 
pagerank=page.rank(net1,weight=edgelist$weight)

artist_stat<-data.frame(degree=degree(net1),
                        closeness=closeness(net1,weight=edgelist$weight),
                        betweenness=betweenness(net1,weight=edgelist$weight),
                        pagerank=pagerank$vector,
                        coreness=coreness$vector
)

artist_stat$artist <- row.names(artist_stat)
rownames(artist_stat)=NULL

# plot

p_mean<- mean(edgelist_graph$popularity)

net2 <- graph.data.frame(edgelist_graph, vertices = NULL, directed = FALSE)

V(net2)$color <- ifelse(edgelist_graph[V(net2), 4] >= p_mean, "darkred", "darkblue" )

plot.igraph(net2,layout=layout.fruchterman.reingold,edge.color="black",
            vertex.size = 5, edge.arrow.size=.3,edge.curved=FALSE,
            edge.labels = NA,labels = NULL,label.font = NULL,label.dist = 0,vertex.label=NA)


# tracks analysis

# NO.1 star according to followers
dt <- data.table(spotify_df)

top_quantile <- quantile(dt$followers,0.75)

dt$star[dt$followers >= top_quantile]=1
dt$star[dt$followers < top_quantile]=0

dt <- dt[, artist_number := length(artist_id) , by = c('track_id')]
dt <- dt[, star_number := sum(star) , by = c('track_id')]

dt<-dt[dt$artist_number>1]

dt$class3<-ifelse(dt$star_number==dt$artist_number,1,0)
dt$class4<-ifelse(dt$star_number==0,1,0)
dt$class5<-ifelse(dt$star_number<dt$artist_number,1,0)

# NO.2 jaccard distance 

dt$artist_genres<-str_sub(dt$artist_genres,2,-2)
dt<-dt[nchar(dt$artist_genres)>2,]
dt<-data.frame(dt)

#list<-as.data.frame(matrix(ncol = 3))
#colnames(list)<-c('genre','track_id','year')
#genre_list<-as.data.frame(matrix(ncol = 3))
#colnames(genre_list)<-c('genre','track_id','year')

split <- strsplit(dt$artist_genres,split=',')

genre_list=vector("list")
track_id_list=vector("list")
year_list=vector("list")

list<-as.data.frame(matrix(ncol = 3))
colnames(list)<-c('genre','track_id','year')
genre_list<-as.data.frame(matrix(ncol = 3))
colnames(genre_list)<-c('genre','track_id','year')

for (i in 1:nrow(dt)){
  for (j in length(split[[i]])){
    #genre_list<-append(genre_list,split[[i]][j])
    #track_id_list<-append(track_id_list,dt[i,2]) # only change this "2"
    #year_list<-append(year_list,dt[i,c('track_year')])
    list$genre<-split[[i]][j]
    list$track_id<-dt[i,3]
    #list$artist_id<-dt[i,12]
    list$year<-dt[i,c('track_year')]
    genre_list<-rbind(genre_list,list)
  }
}

genre_list<-na.omit(genre_list)

genre_list<-data.frame(genre_list)

tracku<-unique(genre_list$track_id)

load('data.RData')
genres_df<-data.frame(genres_df)
genres_df$artist<-rownames(genres_df)
rownames(genres_df)<-NULL

genre_list<-genres_df[ genres_df$artist== dt$artist_name[dt$track_id=="6NtlO37F4rjD1Z11jnV1rJ"  ],]

dt<-unique(dt)

genres_df$artist<-rownames(genres_df)

years<- unique(genre_list$year)
jaccard_df<-as.data.frame(matrix(ncol = 4))
colnames(jaccard_df)<-c("Coordinate1", "Coordinate2","track_id","jaccard_mean")
  
for (i in years){
  genre_list1<-genre_list[genre_list$year==i,]
    
  tracku<-unique(genre_list$track_id)
  genreu<-unique(genre_list$genre)
  
  matrix_artist<-as.data.frame(matrix(nrow = length(tracku),ncol = length(genreu)))
  rownames(matrix_artist)<-tracku
  colnames(matrix_artist)<-genreu
  
  for (i in 1:nrow(genre_list)){
    row=as.character(genre_list[i,2])
    col=as.character(genre_list[i,1])
    matrix_artist[row,col]=1
  }
  
  matrix_artist[is.na(matrix_artist)]=0
  
  
  # get the jaccard distance of the matrix 
  distance <- dist(matrix_artist,"Jaccard")
  # use 'cmdscale' to perform multidimensional scaling
  jaccard <-data.frame(cmdscale(distance))
  # change the name of two columns as "Coordinate1" and "Coordinate2"
  names(jaccard) <- c("Coordinate1", "Coordinate2")
  
  # fill in pindex
  jaccard$track_id=rownames(jaccard)
  rownames(jaccard)=NULL
  
  # get the mean of jaccard distance for later usage
  jacc_matrix<-jaccard_distance(as.matrix(matrix_artist))
  jaccard$jaccard_mean<-colMeans(jacc_matrix)
  
  jaccard_df<rbind(jaccard_df,jaccard)
}

regresst<-merge(jaccard,dt,all.x = TRUE)
regresst$duration<-regresst$duration/60000

regresst<-regresst[,-16]

regresst<-unique(regresst)

regresst<-data.table(regresst)

regresst <- regresst[, mean_pop := mean(artist_popularity) , by = c('track_id')]


  
genre_df<-data.frame(genre=t(data.frame(genre_list)),track_id=t(data.frame(track_id_list)),year=t(data.frame(year_list)))  


# artist jaccard
artist_df<-spotify_df[,c('artist_name','artist_genres')]
artist_df<-dt[,c('artist_name','artist_genres')]
artist_df<-unique(artist_df)
artist_df$artist_genres<-str_sub(artist_df$artist_genres, start = 2, end = -2)
artist_df<-artist_df[nchar(artist_df$artist_genres)>0]

artist_list=vector("list")
genre_list=vector("list")

list<-as.data.frame(matrix(ncol = 2))
colnames(list)<-c('genre','artist')
genre_list<-as.data.frame(matrix(ncol = 2))
colnames(genre_list)<-c('genre','artist')

split_artist <- strsplit(artist_df$artist_genres,split=',')

for (i in 1:nrow(artist_df)){
  for (j in length(split_artist[[i]])){
    #genre_list<-append(genre_list,split[[i]][j])
    #artist_list<-append(artist_list,artist_df[i,1]) # only change this "2"
    list<-as.data.frame(matrix(ncol = 2))
    colnames(list)<-c('genre','artist')
    list$genre<-split[[i]][j]
    list$artist<-artist_df[i,1]  # only change this "2"
    #list$year<-df[i,c('track_year')]
    genre_list<-rbind(genre_list,list)
  }
}

genre_list<-na.omit(genre_list)

artist_genre_df<-data.frame(genre=t(data.frame(genre_list)),artist=t(data.frame(artist_list)))

# define jaccard distance for calculation conveniance
jaccard_distance <- function(m) {
  A <- tcrossprod(m)
  im <- which(A > 0, arr.ind=TRUE, useNames = F)
  b <- rowSums(m)
  Aim <- A[im]
  sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
}


artistu<-unique(genre_list$artist)
genreu<-unique(genre_list$genre)

matrix_artist<-as.data.frame(matrix(nrow = length(artistu),ncol = length(genreu)))
rownames(matrix_artist)<-artistu
colnames(matrix_artist)<-genreu

for (i in 1:nrow(genre_list)){
  row=as.character(genre_list[i,2])
  col=as.character(genre_list[i,1])
  matrix_artist[row,col]=1
}

matrix_artist[is.na(matrix_artist)]=0


# get the jaccard distance of the matrix 
distance <- dist(matrix_artist,"Jaccard")
# use 'cmdscale' to perform multidimensional scaling
jaccard <-data.frame(cmdscale(distance))
# change the name of two columns as "Coordinate1" and "Coordinate2"
names(jaccard) <- c("Coordinate1", "Coordinate2")

# fill in pindex
jaccard$artist=rownames(jaccard)
rownames(jaccard)=NULL

# get the mean of jaccard distance for later usage
jacc_matrix<-jaccard_distance(as.matrix(matrix_artist))
jaccard$jaccard_mean<-colMeans(jacc_matrix)

save(jaccard, file = "artist_jaccard.RData")


# df_u_kw$kw_id <- seq.int(nrow(df_u_kw)) #create id

# regression on all records
regress_artist_1<-regress1[,c('artist_popularity','top_pd_25','compilation' ,'single','genres_count', 'avg.duration' ,'co_level','track_year')]
regress_artist_1<-unique(regress_artist_1)

# histogram 
hist(regress_artist_1$artist_popularity,xlab = "Artist Popularity",ylab = "") 

formula_artist_1 <- "artist_popularity ~ top_pd_25 + compilation + single + genres_count + avg.duration + co_level + factor(track_year)"
# 1
lm(formula_artist_1 ,regress_artist_1)
summary(lm(formula_artist_1 ,regress_artist_1))

regress_artist_2<-merge(regress2,jaccard,all= TRUE)
regress_artist_2<-unique(regress_artist_2)

hist(regress_artist_2$artist_popularity,xlab = "Artist Popularity",ylab = "") 
# 2
lm(formula_artist_1 ,regress_artist_2)
summary(lm(formula_artist_1 ,regress_artist_2))

formula_artist_2 <- "artist_popularity ~ jaccard_mean+coreness+degree+top_pd_25 + compilation + single + genres_count + avg.duration+co_level + factor(track_year)"
# 3
summary(lm(formula_artist_2 ,regress_artist_2))




# glm regression 
dtu<-spotify_df[,c('artist_id','artist_name')]
dtu<-unique(dtu)
dtu<-unique(dt[,c('artist_id','artist_name')]) 
setkey(dtu,'artist_name')
setkey(regress2,'artist_name')
regress2<-merge(regress2,dtu,all.x = TRUE)

regress2<-regress1[regress1$co_level>0]
regress2<-unique(regress2)

regress3<-merge(regress2,dtu,all.x = TRUE)
regress3<-unique(regress3)

setkey(regress3,'artist_id')
jaccard<-data.table(jaccard)
colnames(jaccard)[3] <- "artist_id"
setkey(jaccard,'artist_id')

regress4<-merge(regress3,jaccard)

setkey(regress2,)

x<-merge(regress2,jaccard)

glm(artist_popularity~)

reg_formula <- "artist_popularity ~ jaccard_mean+factor(top_pd_25) + compilation + single + genres_count + avg.duration + co_level + degree  + coreness + factor(track_year)"
glm.nb(reg_formula,regress4,offset(total_song))
lm(reg_formula,regress4)

artist_popularity ~ track_year+ P + H + total_box + # regression for the first measure
  Number_of_years_in_operation + Is_subsidiary + factor(year), regress1, offset(total_films)

# panel regress
regress_panel<-unique(regress1)

follow_data<-spotify_df[,c('track_year','artist_name','followers')]
follow_data<-unique(follow_data)
regress_panel<-merge(regress_panel,follow_data,by=c('track_year','artist_name'),all.x = TRUE)
top_f_quantile <- quantile(regress_panel$followers,0.75,na.rm = TRUE)
regress_panel$star <- ifelse(regress_panel$followers>=top_f_quantile,1,0) 

pgr <- plm.data(regress_panel, index = c("artist_name", "track_year"))
pgr<-pgr[!duplicated(pgr[,c('track_year','artist_name')]),]


# regression
reg_formula <- "artist_popularity ~ degree+eigenvector"
gr_pool <- plm(reg_formula, data = pgr,
               model = "pooling")
gr_fe <- plm(reg_formula, data = pgr,
             model = "within")
gr_re <- plm(reg_formula, data = pgr,
             model = "random", random.method = "swar")

pFtest(gr_fe, gr_pool)
phtest(gr_re, gr_fe)

summary(gr_re)
summary(gr_pool)
summary(gr_fe)

# lm artist
sn_degree=data.frame(track_year=sn_degree$track_year,artist_name=sn_degree$artist_name,degree=sn_degree$degree)
regress_a_1<-regress_panel[regress_panel$degree>1]
regress_a_1<-regress_panel[,-10]
regress_a_1<-regress_a_1[,-12]
regress_a_1<-regress_a_1[,-12]
regress_a_1<-merge(regress_a_1,sn_degree,by=c('artist_name','track_year'),all.x =  TRUE)
regress_a_1<-merge(regress_a_1,sn_core,by=c('artist_name','track_year'),all.x =  TRUE)
regress_a_1<-na.omit(regress_a_1)

regress_a_1<-merge(regress_a_1,jaccard,by=c('artist_name'),all.x =  TRUE)
regress_a_1<-unique(regress_a_1)

regress_a_1$log_coreness<-log(regress_a_1$coreness)
regress_a_1$log_degree<-log(regress_a_1$degree)
regress_a_1$log_coreness[regress_a_1$log_coreness==-Inf]=0
regress_a_1=regress_a_1[regress_a_1$coreness!=-Inf]
hist(regress_a_1$coreness)
hist(sn_core$log_coreness)
sn_core$log_coreness<-log(sn_core$coreness)

regress_a_1$jaccard_mean2<-regress_a_1$jaccard_mean^2

# top degree & coreness
top_degree_quantile <- quantile(regress_a_1$degree,0.75,na.rm = TRUE)
regress_a_1$top_degree <- ifelse(regress_a_1$degree>=5,1,0) 
top_core_quantile <- quantile(regress_a_1$coreness,0.75,na.rm = TRUE)
regress_a_1$top_core <- ifelse(regress_a_1$coreness>=0.5,1,0) 

reg_formula <- "artist_popularity ~ top_pd_25 + compilation + single + genres_count+avg.duration + star + top_degree + top_core+ factor(track_year) "
summary(lm(reg_formula,regress_a_1))

regress_a_1<-regress_a_1[regress_a_1$degree>1]
reg_formula <- "artist_popularity ~ top_pd_25 + compilation + single + genres_count + avg.duration +star+ factor(track_year)"
summary(lm(reg_formula,regress_a_1))

reg_formula <- "artist_popularity ~ jaccard_mean+top_pd_25 + compilation + single + genres_count+avg.duration + star + degree + coreness+ factor(track_year) "
summary(lm(reg_formula,regress_a_1))
lm<-lm(reg_formula,regress_a_1)
step.model <- stepAIC(lm, direction = "both", trace = F)
summary(step.model)

reg_formula <- "artist_popularity ~ jaccard_mean2+jaccard_mean+top_pd_25 + compilation + single + genres_count+avg.duration + star + degree + coreness + factor(track_year)"
summary(lm(reg_formula,regress_a_1))

regress_a_2<-na.omit(regress_a_1)
reg_formula <- "artist_popularity ~ top_pd_25 + compilation + single + genres_count + avg.duration +star"
summary(lm(reg_formula,regress_a_2))

x<-na.omit(regress_a_1)
x<-regress_a_1[regress_a_1$degree>1]
summary(lm(reg_formula,x))


# lm track

regresst$compilation <- ifelse(regresst$album_type=='compilation',1,0)   
regresst$single <- ifelse(regresst$album_type=='single',1,0) 
regresst$album<- ifelse(regresst$album_type=='album',1,0)   
regresst <- regresst[, total_track := length(track_id) , by = c('track_year')]

regresst$jaccard_similarity<-1-regresst$jaccard_mean
regresst$jaccard_similarity2<-regresst$jaccard_similarity^2
regresst$jaccard_similarity3<-regresst$jaccard_similarity^3

# histogram
hist(regresst$track_popularity,xlab = "Track Popularity",ylab = "")

formula_track_1 <- "track_popularity ~ class3 + class4+compilation + single  + duration+ mean_pop+ factor(track_year)"

formula_track_2 <- "track_popularity ~ jaccard_similarity+class3 + class4+compilation + single  + duration+ mean_pop+ factor(track_year)"

formula_track_3 <- "track_popularity ~ jaccard_similarity+jaccard_similarity2+jaccard_similarity3+class3 + class4+compilation + single  + duration+ mean_pop+ factor(track_year)"


summary(lm(formula_track_1,regresst))
summary(lm(formula_track_2,regresst))
summary(lm(formula_track_3,regresst))

# ggplot

ggplot(regress4, aes(jaccard_mean, artist_popularity)) + 
  geom_smooth(method= "loess", se = T) + labs(x = "Average Jaccard distance", y = "Artist Popularity")

ggplot(regresst, aes(jaccard_mean, track_popularity)) + 
  geom_smooth(method= "loess", se = T) + labs(x = "Average Jaccard distance", y = "Track Popularity")

ggplot(regress4, aes(degree, artist_popularity))+geom_point()+labs(x = "Artist Degree Number", y = "Artist Popularity")+
  geom_smooth(method='lm')
regressc<-regress4[regress4$coreness<0.05]
ggplot(regress4, aes(coreness, artist_popularity))+geom_point() + labs(x = "Artist Coreness", y = "Artist Popularity")
  # geom_smooth(method='lm')

ggplot(regressc, aes(coreness, artist_popularity)) + 
  geom_smooth(method= "loess", se = T) + labs(x = "Artist Coreness", y = "Artist Popularity")



######### end




betweenness=betweenness(net1,weight=edgelist$weight)                        
coreness=eigen_centrality(net1,weight=edgelist$weight) 
X<-coreness$vector
View(X)
                        
artist_stat<-cbind(artist_stat,closeness(net1,weight=edgelist$weight),
                   )

artist_stat <- edgelist<-as.data.frame(matrix(ncol = 2))



# Colour negative correlation edges as blue


V(net2)[which(V(net2)$popularity<p_mean)]$color <- "darkblue"

# Colour positive correlation edges as red
V(net2)[which(V(net2)$popularity>p_mean)]$color <- "darkred"

for (j in unique(test_df$track_id)){
  comb<-as.character(unique(test_df[track_id==j,artist_name]))
  # put all key words of a film in one row
  if(length(comb)>1){
    edge<-as.data.frame(t(combn(comb,2)))
    # get the words combination by 2 
    # edge$year=i
    edgelist<-rbind(edgelist,edge)
  }
}



# df_u_kw$kw_id <- seq.int(nrow(df_u_kw)) #create id
