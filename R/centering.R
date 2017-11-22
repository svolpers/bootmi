centering <-
function( data_set, centered_vars) {
  # convert data_set to data frame
data_set = as.data.frame(data_set)
  # center moderators and save as data.frame
  y = data.frame( scale( data_set[ , centered_vars], center = TRUE, scale = FALSE))
# merge centered moderators with other variables of data_set
final_data = merge( data_set[ , !(names(data_set) %in% centered_vars)], y, by=0, all.x=TRUE)
# next three lines needed for getting original sorting
final_data = transform( final_data, Row.names = as.numeric(Row.names))
final_data = final_data[order(final_data$Row.names), ]
final_data = final_data[-1]
  # return data
return(final_data)
}
