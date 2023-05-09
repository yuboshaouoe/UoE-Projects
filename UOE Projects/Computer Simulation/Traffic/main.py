import numpy as np
import random
from matplotlib import pyplot as plt


class Traffic:

    def __init__(self):
        self.__lengthOfRoad = int(input("Length of the road: "))

        # Making sure the density of cars cannot exceeds the length of the road
        self.__densityOfCars = int(input("Density of cars: "))
        while self.__densityOfCars > self.__lengthOfRoad:
            print("The number of cars on the road should be lower than the length of the road!")
            self.__densityOfCars = int(input("Density of cars: "))

        self.__numberOfIter = int(input("Number of iterations: "))

        # Initialize the road
        self.__arr = np.zeros((1, self.__lengthOfRoad))

    # randomly distribute given amount of cars to the initial road
    def __rand_cars_distribution(self, num_cars):
        for i in random.sample(range(self.__lengthOfRoad), num_cars):
            self.__arr[0][i] = 1

    # Modify the 2D road array and return the average speed of one iteration
    def __update_rules(self):

        moved_car = 0
        next_road = np.zeros((1, self.__lengthOfRoad))
        current_road = self.__arr[self.__arr.shape[0] - 1]

        # Same mechanism as given on the website
        for i in range(self.__lengthOfRoad):

            # Uses % to make the road loop itself
            car_behind_index = (i - 1) % self.__lengthOfRoad
            car_front_index = (i + 1) % self.__lengthOfRoad

            if current_road[i] == 1:
                if current_road[car_front_index] == 1:
                    next_road[0][i] = 1
            else:
                if current_road[car_behind_index] == 1:
                    next_road[0][i] = 1
                    moved_car += 1

        # Join the old and new road into one array & update the current array
        self.__arr = np.concatenate((self.__arr, next_road))

        # Return the average speed of this iteration
        return moved_car / self.__densityOfCars

    # Iterate the update loop for given amount of time steps
    def __update_iteration(self):
        i = 1
        average_speeds = []
        while i != self.__numberOfIter:
            average_speeds.append(self.__update_rules())
            i += 1
        # Return an array of average speeds for each time step iteration
        return average_speeds

    # Find the steady state speed of the current system
    def __steady_state_speed(self):
        average_speeds = self.__update_iteration()
        for i in range(1, len(average_speeds)):
            # until consecutive average speeds are the same
            if average_speeds[i] == average_speeds[i-1]:
                # THEN round to 2 sig figs
                steady_state = round(self.__update_iteration()[i], 2)
                return steady_state

    def __array_of_steady_state_speed(self):
        speed_list = []
        # from 0 car to completely jammed road
        for i in range(self.__lengthOfRoad + 1):
            self.__arr = np.zeros((1, self.__lengthOfRoad))
            self.__rand_cars_distribution(i)
            speed_list.append(self.__steady_state_speed())
        return np.array(speed_list)

    # plot steady state speed against car density
    def function_of_speed_plot(self):
        dev_x = np.arange(0, self.__lengthOfRoad + 1, 1)
        dev_y = self.__array_of_steady_state_speed()
        plt.plot(dev_x, dev_y)
        plt.xlabel("Density of Cars")
        plt.ylabel("Steady State Average Speed")
        plt.title("Steady State Average Speed Against the Density of Cars")
        plt.show()

    # Print the list of average speed based on user inputs
    def list_of_average_speed(self):
        self.__arr = np.zeros((1, self.__lengthOfRoad))
        self.__rand_cars_distribution(self.__densityOfCars)
        print(self.__update_iteration())

    # plot the position of cars with respect to time
    def function_of_time_plot(self):
        self.__update_iteration()
        plt.imshow(self.__arr, interpolation='none', origin='lower')
        plt.xlabel("Road Position")
        plt.ylabel("Time Step")
        plt.show()


# Test code
test = Traffic()
test.list_of_average_speed()
test.function_of_time_plot()
test.function_of_speed_plot()
