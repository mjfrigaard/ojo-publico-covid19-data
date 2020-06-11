#=====================================================================#
# This is code to create: @mjfrigaard
# Authored by and feedback to:
# MIT License
# Version: 1.0
#=====================================================================#


# packages ----------------------------------------------------------------


library(ggmap)



# add API key -------------------------------------------------------------

# this adds the key to /Users/mjfrigaard/.Renviron
ggmap::register_google(key = "AIzaSyCjKa60nphePifqT8YyWR1VwmworLapnGI", 
                       write = TRUE)