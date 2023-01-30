library(tidyverse)

Levittown <- read_csv("Levittown.csv")

head(Levittown)  #This shows the top 6 (by default) entries in a dataset to see what is there
colnames(Levittown) #allows us to quickly see the column names. Here we note they are in all capital letters

##Not very interested in all 27 variables, so let's filter our dataset to fewer
##We will create a new dataset "Working" to save our filter for future analysis
Working <- Levittown %>% 
  select(`PROPERTY TYPE`, ADDRESS, PRICE, BEDS, BATHS, `SQUARE FEET`, `LOT SIZE`, `YEAR BUILT`)
  #select is the function to choose variables
  #Variables are case sensitive. If there are any special characters (like a space), you must have the ` ` by the 1 on the keyboard


#Distribution of a categorical variable - Bar Graph
Working %>% 
  ggplot(aes(x=`PROPERTY TYPE`))+
  geom_bar()
#Here, we set X to look at PROPERTY TYPE, but we could use any categorical variable name in a dataset
#We see a good chunk of our data is Vacant Land - we'll adjust for this later


##Convert our categorical variable to a binary categorical variable
Working %>% 
  mutate(SingleFam = `PROPERTY TYPE` == "Single Family Residential") %>% 
  select(SingleFam) %>% 
  ggplot(aes(x=SingleFam))+
  geom_bar()
##Here we can see that there are between 3 to 4 times more single family homes than other types of homes



#Distribution of a quantitative variable - Histogram
Working %>% 
  ggplot(aes(x=PRICE))+
  geom_histogram()
#Here, we set X to look at PRICE, but we could use any quantitative variable name in a dataset
#Here, we plot a histogram of the house price in our area



#We may wish to know the average price of this data, or its variation
Working %>% 
  summarize(Mean = mean(PRICE), SD = sd(PRICE))



#Here, we may wish to filter down our data - for example, take away our vacant lots and look at homes under $1,000,000
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>% 
  ggplot(aes(x=PRICE))+
  geom_histogram(bins=50) 
    #Note the histogram defaults to 30 bins. 
    #We may want more or fewer bins to present our data. Here I have 50, but we could try 10 or 100
    #Note that more bins requires more computing power and may slow your computer

#mean price with filtering out vacant lands
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>%
  summarize(Mean = mean(PRICE), SD = sd(PRICE))

#Here, we look at the number of beds - this is coded as a quantitative, use histogram
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>%
  ggplot(aes(x=BEDS))+
  geom_histogram()

#lets look at square feet
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>%
  ggplot(aes(x=`SQUARE FEET`))+
  geom_histogram()

##We note in our Console window that there are rows removed from our data, red text - we should filter these out manually
##We should also save these changes to avoid further errors between graphs
Working <- Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land", BATHS > 0, `SQUARE FEET` > 0, PRICE > 0, BEDS > 0, `YEAR BUILT` < 2023, `YEAR BUILT` > 0, `LOT SIZE`>0)

#just look at single family residential
Working <- Working %>% 
  mutate(SingleFam = `PROPERTY TYPE` == "Single Family Residential")

##Now we'll rerun the previous code with our filtered data to see if errors are gone
#beds
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>%
  ggplot(aes(x=BEDS))+
  geom_histogram()

#price
Working %>% 
  filter(PRICE <= 1000000, `PROPERTY TYPE` != "Vacant Land") %>%
  ggplot(aes(x=`SQUARE FEET`))+
  geom_histogram()




#Compare behavior of two different quantitative variables - Scatterplot
Working %>% 
  ggplot(aes(x= `SQUARE FEET`, y=PRICE))+
  geom_point()
##Here, we are looking to see if square footage increases the price of a house, on average


Working %>% 
  ggplot(aes(x = BEDS, y = BATHS))+
  geom_point()
##Here, we are looking to see if beds and baths have a relationship. 
##Note the dotted pattern - this occurs if you have strictly integer data or categorical data
##To gauge data here, we can generate a density plot to assess behavior


Working %>% 
  ggplot(aes(x=BEDS, y=BATHS))+
  geom_density_2d(bins=50)+
  xlim(c(1,6))+
  ylim(c(1,5.5))
# The density plot allows us to gain histogram-type inference about how two variables interact

Working %>% 
  ggplot(aes(x=BEDS, y=BATHS))+
  geom_density_2d()+
  geom_point()
#could combine the two for context of those we have data for, 
  #this may help identify unusual or outlier combinations of variables

Working %>% 
  ggplot(aes(x=BEDS, y=BATHS))+
  geom_density_2d_filled()+
  geom_point(color="salmon")
#This provides the same analysis with a heat map (color) instead of contour lines


##We can also use this on two regular quantitative as well
Working %>% 
  filter(`LOT SIZE` < 250000) %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`LOT SIZE`))+
  geom_density_2d()



##Compare categorical data with quantitative to see if there are differences - Boxplots
Working %>% 
  ggplot(aes(x=as.factor(BATHS), y = PRICE))+
  geom_boxplot()
##We see differences between some - for example, houses with 1 bathroom are generally cheaper than houses with 4 bathrooms
##However, there seems to be little noticeable difference between 1.5, and 2 baths, on average

Working %>% 
ggplot(aes(x=as.factor(BEDS), y=PRICE))+
  geom_boxplot()

##We see that, in general, there is a difference between more beds and price.
##Let's look at this behavior broken down by property type, looking at more complicated behaviors

Working %>% 
  ggplot(aes(x=as.factor(BEDS), y = PRICE, color=as.factor(`PROPERTY TYPE`)))+
  geom_boxplot()


##General dotplot Sq Ft to Price - note we can add labels if the defined names are unclear
Working %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`))+
  geom_point()+
  labs(x="Sq Ft", y="Price")

#Here we can color our points by category of property. We can adjust that label and a title as well
Working %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`PROPERTY TYPE`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Type of Property", title="Levittown Housing")


#can add + theme_xxx to change background look of a chart
Working %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`PROPERTY TYPE`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Type of Property", title="Levittown Housing")+
  theme_light()   #White background with gray graph lines
#  theme_dark()      #Gray background with dark gray graph lines
#  theme_void()      #No background lines, all white
#  theme_bw()        #White background with gray graph lines, black total outline
# in RStudio, start typing theme_   and let it populate recommendations, then choose one



#Let's instead color by bed

Working %>% 
  filter(PRICE < 1000000, `SQUARE FEET` < 6000) %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Number of Beds", title="Levittown Housing")


#Our labs adds labels - x is the X=label, y is the Y-label, color is the Legend label, title is the Main label
#We can see, as expected, prices generally increase with square footage and generally bigger houses have more beds


#manually change color
Working %>% 
  filter(PRICE < 1000000, `SQUARE FEET` < 6000) %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Number of Beds", title="Levittown Housing")+
  scale_color_manual(values=c("black", "grey", "blue", "purple", "red", "orange", "brown"))
#With multiple categories this may be a hassle, but allows us to force consistency through multiple graphs


##Here, we can save our colors to call back to them easier next time
cols <- c("black", "grey", "blue", "purple", "red", "orange", "brown")

Working %>% 
  filter(PRICE < 1000000, `SQUARE FEET` < 6000) %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Number of Beds", title="Levittown Housing")+
  scale_color_manual(values=cols)

#Let's add a shape to combine previous information to color by bed and apply a shape by property type
Working %>% 
  filter(PRICE < 1000000, `SQUARE FEET` < 6000) %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`), shape=as.factor(`PROPERTY TYPE`)))+
  geom_point()+
  labs(x="Sq Ft", y="Price", color="Number of Beds", shape="Property Type", title="Levittown Housing")

#We decided we want to only look at Single Family Residential Houses
  #let's add `PROPERTY TYPE` to our filter, noting the string "Single Family Residential" is in quotes and must match the case exactly
Working %>% 
  filter(`PROPERTY TYPE` == "Single Family Residential") %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`)))+
  geom_point(shape="square")+
  labs(x="Sq Ft", y="Price", color="Number of Beds", title="Levittown Housing")


#Let's force our axes to begin at (0,0)
Working %>% 
  filter(`PROPERTY TYPE` == "Single Family Residential") %>% 
  ggplot(aes(x=`SQUARE FEET`, y=`PRICE`, color=as.factor(`BEDS`)))+
  geom_point(shape="square")+
  labs(x="Sq Ft", y="Price", color="Number of Beds", title="Levittown Housing")+
  xlim(0,7000)+       #this reads as minimum = 0, maximum = 5500
  ylim(0, 1000000)    #this reads as minimum = 0, maximum = 1000000

#Sometimes this filtering process takes a long time or we want to ensure consistency with other people
  #Let's save our converged data as a csv we can share, or start from at a later time to save time
write_csv(Working, file="file.csv")
  ##this saves the object we defined as Working to our current working directory
  ## it saves the file with the name of "file.csv". You can name it whatever you'd like to find it later

