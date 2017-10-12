# Number of cars
cars = 100
# Number of seats in each car
space_in_a_car = 4.0
# Number of drivers
drivers = 30
# Number of passengers
passengers = 90
# Number of cars that don't have a driver
cars_not_driven = cars - drivers
# Number of cars that do have a driver
cars_driven = drivers
# Total number of people that can be taken by all cars being driven
carpool_capacity = cars_driven * space_in_a_car
# Number of people that must go in each car on average
average_passengers_per_car = passengers / cars_driven


print("There are", cars, "cars available.")
print("There are only", drivers, "drivers available.")
print("There will be", cars_not_driven, "empty cars today.")
print("We can transport", carpool_capacity, "people today.")
print("We have", passengers, "to carpool today.")
print("We need to put about", average_passengers_per_car,
      "in each car.")
