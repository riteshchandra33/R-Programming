#How to write a simple function

#General form of a fnction:
MyFunction<-function(argument names go here) {
  output<- (argument)^2  #Raise the emelemt entered as an argument to the power 2
  return(output)
}

#Creatig a function that takes the temperature in Fahrenheit and converts it to Celsius and returns it
temp_fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

temp_fahrenheit_to_celsius(32) #Answer is 0 degree Celsius
temp_fahrenheit_to_celsius(200) #Answer is 93.33 degree Celsius

#Here you can see how we can enter multiple numers at once and get the results using the same function
temp<-c(32, 44, 56, 87, 200)
temp_fahrenheit_to_celsius(temp)

temp1<-32:40
temp_fahrenheit_to_celsius(temp1)

#How to compose a function within another function
#Creatig a function that takes the temperature in Celsius (the previous function) and converts it to Kelvin and returns it
temp_celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

temp_celsius_to_kelvin(10)
