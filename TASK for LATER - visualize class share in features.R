# Create a function that would plot you a % share of class (binary) across each column of interest


## Plot and visualize by variable and survival
library(ggplot2)
require(gridExtra)

full$AgeCut = cut(full$Age,breaks = seq(0, 100, by = 10))
full$FareCut = cut(full$Fare,10)


cols = c('AgeCut','Sex','Embarked','FareCut','Parch','Pclass','SibSp', 'Cabin2','Title2')
vector = list()


get_plot = function (data, col) {
  data_group = data[,c(col,"Survived")] %>% 
    select(x = col, "Survived") %>%
    drop_na(Survived) %>% 
    group_by(x) %>% 
    summarise(Total = n(),Survived = sum(Survived,na.rm = T), ratio = sum(Survived,na.rm = T)/n())
  
  plot = ggplot(data_group ,aes(x, Survived, label='A')) +
    geom_bar(aes(y=Total), stat="identity", fill="red") +
    geom_bar(aes(y=Survived), stat="identity", fill="lightgreen")  +
    geom_text(aes(label = (round(Survived/Total,4) * 100)), color='blue', vjust = -5.25) + 
    xlab(col)
  
  return(plot)
}

for (col in cols) {
  vector[[col]] = get_plot(full,col)
}

grid.arrange( grobs = vector, ncol = 2)