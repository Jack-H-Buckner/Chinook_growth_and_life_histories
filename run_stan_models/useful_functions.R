

# useful function 
header.true.row.true <- function(df) {
  df$index <- rownames(df)
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


# define a function that extracts the years of data 
growth_years <- function(returns_data, min_age){
  df <- returns_data%>%
    group_by(age, brood_year, release_age)%>%
    dplyr::summarize(v = 1)
 
  years <- c()
  for(i in 1:nrow(df)){
    a <- df$age[i]
  
    ra <- df$release_age[1]

    for(age in min_age:a){ # subtract on for between age increments
      years <- append(years, df$brood_year[i] + age)
    }
  }
  return(list(years = sort(unique(years)),
              n_years = max(years) - min(years)+1, 
              off_set = min(returns_data$brood_year)+min_age - 1
  )
  )
}




# define a function that extracts the years of data 
max_year_stock <- function(data){
  year_zero =  min(data$brood_year)
  year_final = max(data$brood_year) + max(data$age)+1
  max_year = year_final-year_zero-1
  stocks <- c()
  max_years <- c()
  for(s in unique(data$stock)){
    stocks <- append(stocks,s)
    d <- data %>% 
      dplyr::filter(stock == s) %>%
      dplyr::mutate(y = brood_year + age + 1)
    max_years <- append(max_years,max(d$y) - year_zero-1)
  }
  return(list(year_zero = year_zero, year_final = year_final, 
              max_year = max_year, max_years = max_years))
}



