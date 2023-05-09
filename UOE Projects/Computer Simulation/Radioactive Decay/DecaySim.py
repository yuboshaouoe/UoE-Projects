import numpy as np


class DecaySim:
    # Create the 2D numpy array
    __arr = np.zeros((0, 0))

    # Prompt for the decay constant, 2D array side length, and time step
    # Initializes the 2D array
    def __init__(self):
        self.__decay_constant = float(input("value of the decay constant (min^-1): "))
        self.__arr_length = int(input("side length of the 2D array (int): "))
        self.__time_step = float(input("duration of the time step (min): "))
        self.__arr = np.zeros((self.__arr_length, self.__arr_length))

    # Calculate half of the atoms
    def __half_of_atoms(self):
        return int((np.square(self.__arr_length)) / 2)

    # Calculate probability of decay
    def __probability(self):
        return self.__decay_constant * self.__time_step

    # Calculate actual half-life with given decay constant
    def __actual_hl(self):
        return np.log(2) / self.__decay_constant

    # For each atom in the array, if the random number <= the probability of decay, it decays
    # If an atom has decayed (self.__arr[i, n] != 0) then passes
    def __decay(self):
        for i in range(self.__arr_length):
            for n in range(self.__arr_length):
                result = np.random.random() <= self.__probability()
                if result and self.__arr[i, n] == 0:
                    self.__arr[i, n] = 1

    # Sums the total decayed atoms
    def __sum_decayed(self):
        decayed = np.sum(np.sum(self.__arr, axis=1))
        return decayed

    # Repeat the decay process if sum of decayed atom didn't reach half of the total size
    def __simulated_hf(self):
        simulated_hf = 0.0
        while self.__half_of_atoms() > self.__sum_decayed():
            self.__decay()
            simulated_hf += self.__time_step
        return simulated_hf

    # Display the corresponding calculated data
    def display_result(self):
        print(f"simulated half life: {format(self.__simulated_hf(), '.2f')} min")
        print(f"actual half life: {format(self.__actual_hl(), '.2f')} min")
        print(f"initial undecayed nuclei: {np.square(self.__arr_length)}")
        print(f"final undecayed nuclei: {int(np.square(self.__arr_length) - self.__sum_decayed())}")
        print(self.__arr)


test = DecaySim()
test.display_result()
