import numpy as np
from numpy import multiply as mult
from numpy import divide as div
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import csv


class Model:
    __G = 6.67e-11
    object_data = {}
    position_dict = {}
    energy_dict = {'total_energy': [], 'kinetic_energy': [], 'potential_energy': []}

    def __init__(self):
        self.__read_object_data()

    # ======================================== Data Reading ========================================#

    # Read data from csv file
    def __read_object_data(self):
        with open('object_data.csv', 'r') as object_csv_file:
            csv_reader = csv.DictReader(object_csv_file)
            # Evaluate every possible element
            for line in csv_reader:
                # The first key 'name' is paired to a string,
                # eval(Type: String) would return an error
                for key in list(line.keys())[1:]:
                    if type(eval(line[key])) == tuple:
                        line[key] = np.array(eval(line[key]))
                    else:
                        line[key] = eval(line[key])
                # Read to class dictionary including every given body
                self.object_data[line['name']] = line
                self.position_dict[line['name']] = [tuple(line['position'])]

    # ======================================== Beeman Algorithm ========================================#

    # Update positions to object_data and appending new position to position lists
    def __update_position(self, time_step):
        for key in self.object_data:
            next_pos = self.object_data[key]['position'] + mult(self.object_data[key]['velocity'], time_step) + \
                       mult(1 / 6 * time_step ** 2, mult(4, self.object_data[key]['current_acceleration']) -
                            self.object_data[key]['previous_acceleration'])
            self.object_data[key]['position'] = next_pos
            self.position_dict[key].append(tuple(next_pos))

    # Calculate the acceleration for the next time step
    def __calc_next_acc(self):
        for key in self.object_data:
            other_bodies = list(self.object_data.keys())
            other_bodies.remove(key)
            lst = []
            # Calculation relative to each other body and append each value to lst
            for body in other_bodies:
                distance_between = self.object_data[key]['position'] - self.object_data[body]['position']
                other_body_mass = self.object_data[body]['mass']
                magnitude = np.linalg.norm(distance_between)
                lst.append(mult(div(other_body_mass, magnitude ** 3), distance_between))
            next_acc = mult(-self.__G, sum(lst))
            # Update next acceleration
            self.object_data[key]['next_acceleration'] = next_acc

    # Update the velocity at the next time step
    def __update_velocity(self, time_step):
        for key in self.object_data:
            self.object_data[key]['velocity'] = self.object_data[key]['velocity'] + \
                                                mult(div(time_step, 6),
                                                     mult(2, self.object_data[key]['next_acceleration']) +
                                                     mult(5, self.object_data[key]['current_acceleration']) -
                                                     self.object_data[key]['previous_acceleration'])

    # Update the previous to current acceleration,
    # the current to next acceleration (for the next time step)
    def __update_accs(self):
        for key in self.object_data:
            self.object_data[key]['previous_acceleration'] = self.object_data[key]['current_acceleration']
            self.object_data[key]['current_acceleration'] = self.object_data[key]['next_acceleration']

    # ======================================== Energy Calculation ========================================#

    # Calculate and store kinetic, potential, and total energy of the system
    def __calculate_energies(self):
        E_k_list = []
        E_p_list = []
        for key in self.object_data:
            # Calculate E_k for each body
            E_k_list.append(1 / 2 * self.object_data[key]['mass'] *
                            np.linalg.norm(self.object_data[key]['velocity']) ** 2)
            other_bodies = list(self.object_data.keys())
            other_bodies.remove(key)
            # Calculate E_g for each body twice
            for body in other_bodies:
                distance_between = self.object_data[body]['position'] - self.object_data[key]['position']
                E_p_list.append(self.__G * self.object_data[key]['mass'] * self.object_data[body]['mass']
                                / np.linalg.norm(distance_between))
        ke = sum(E_k_list)
        pe = -1 / 2 * sum(E_p_list)
        te = ke + pe
        # Storing values
        self.energy_dict['kinetic_energy'].append(ke)
        self.energy_dict['potential_energy'].append(pe)
        self.energy_dict['total_energy'].append(te)

    # ======================================== Iteration cycle ========================================#

    # Iterate the Beeman algorithm a given number of time with given dt
    def update_iteration(self, num_of_time_step, time_step):
        for i in range(num_of_time_step):
            # Energy Calculation and saving to self.energy_dict
            self.__calculate_energies()
            # Beeman Methods
            self.__update_position(time_step)
            self.__calc_next_acc()
            self.__update_velocity(time_step)
            self.__update_accs()


class Simulation:
    model = Model()
    __display_data = {}
    __patches = []
    num_of_time_steps = 0
    time_step_length = 0

    def __init__(self):
        self.__read_display_data()
        self.__read_time_step_settings()

    # Initialize position and energy dictionaries
    def initialize_dicts(self):
        self.model.update_iteration(self.num_of_time_steps, self.time_step_length)

    @staticmethod
    def convert_to_days(time_step, num_of_time_step):
        return int(time_step * num_of_time_step / 86400)

    # ======================================== Data Reading ========================================#

    def __read_time_step_settings(self):
        with open('time_step_settings.csv', 'r') as time_step_csv:
            csv_reader = csv.reader(time_step_csv)
            data = next(csv_reader)
            self.num_of_time_steps = eval(data[0])
            self.time_step_length = eval(data[1])

    def __read_display_data(self):
        with open('display_data.csv', 'r') as display_csv_file:
            csv_reader = csv.DictReader(display_csv_file)
            for line in csv_reader:
                line['display_radius'] = eval(line['display_radius'])
                self.__display_data[line['name']] = line

    # ======================================== Orbital Period ========================================#

    # Calculate the angle between 2 positional vectors
    @staticmethod
    def __find_angle_between(a, b):
        a_u = a / np.linalg.norm(a)
        b_u = b / np.linalg.norm(b)
        return np.arccos(np.clip(np.dot(a_u, b_u), -1.0, 1.0))

    # Approximate the orbital period of a given body from its positional data by
    # comparing the angle between the initial position and the position at each time step
    def __find_orbital_period(self, body):
        seconds_in_earth_year = 60 * 60 * 24 * 365
        # Initial position
        a = self.model.position_dict[body][0]
        # Counter used to check if a whole orbit has been committed
        counter = False
        for b in self.model.position_dict[body]:
            angle = self.__find_angle_between(a, b)
            # Confirm that the planet has followed at least half of the orbit
            if angle > (179 / 180) * np.pi:
                counter = True
            # If angle < np.pi / 180 but counter == 0 means the planet just started orbiting
            if angle < np.pi / 360 and counter:
                return round(self.model.position_dict[body].index(b) *
                             self.time_step_length / seconds_in_earth_year, 3)
        return 0

    # Remove unwanted bodies from calculating its orbital period
    def __body_remover(self, unwanted_bodies):
        list_of_bodies = list(self.model.position_dict.keys())
        for key in list_of_bodies:
            for del_key in unwanted_bodies:
                if key == del_key:
                    list_of_bodies.remove(key)
        return list_of_bodies

    # Print the orbital period of wanted bodies
    def print_orbital_period(self, unwanted_bodies):
        list_of_bodies = self.__body_remover(unwanted_bodies=unwanted_bodies)
        for key in list_of_bodies:
            heading = f"Approximated Orbital Period ({key}): "
            orbital_period = self.__find_orbital_period(key)
            # If the angle between the starting position vector and position vector at each time step are all bigger
            # 0.5 degrees or pi/360 radians (given orbit has been followed)
            if orbital_period == 0:
                print(heading + "The given number of time steps is not enough to predict the orbital period.")
            else:
                print(heading + f"{orbital_period} Earth years")
        print('')

    # ======================================== Solar System Animation ========================================#

    # Initialize a Circle object for each body and save them to self.__patches
    def __generate_body(self):
        for key in self.__display_data:
            body = plt.Circle(self.model.position_dict[key][0],
                              self.__display_data[key]['display_radius'],
                              color=self.__display_data[key]['display_color'],
                              animated=True)
            self.__patches.append(body)

    # Return the ith key of a dictionary
    @staticmethod
    def __ix(dic, i):
        try:
            return list(dic)[i]
        except IndexError:
            print("Not enough keys for animation.")

    # At each time step, return the circles with their positions at that time step
    def __animate_func(self, i):
        for patch in self.__patches:
            key = self.__ix(self.model.position_dict, self.__patches.index(patch))
            patch.center = self.model.position_dict[key][i]
        return self.__patches

    # Display the Animation
    def display_simulation(self):
        fig = plt.figure()
        ax = plt.axes()
        self.__generate_body()
        for i in range(0, len(self.__patches)):
            ax.add_patch(self.__patches[i])
        ax.axis('scaled')
        ax.set_xlim(-3e11, 3e11)
        ax.set_ylim(-3e11, 3e11)
        ax.set_xlabel('x displacement (metres)')
        ax.set_ylabel('y displacement (metres)')
        anim = FuncAnimation(fig, self.__animate_func, frames=self.num_of_time_steps,
                             interval=0.2, repeat=True, blit=True)
        plt.show()

    # ======================================== Energy Writing and Plotting ========================================#

    # Writing total energy of the system to a txt file
    def write_te_to_file(self):
        file = open('TotalEnergy.txt', 'w')
        for i in range(self.num_of_time_steps):
            if i % int(self.num_of_time_steps / 10) == 0:
                file.write(f"At time step {i},\n"
                           f"{self.convert_to_days(self.time_step_length, i)} days since the starting point,\n"
                           f"Total Energy = {self.model.energy_dict['total_energy'][i]}\n\n")
        file.close()

    # Plot the energy graph
    def display_energy_graph(self):
        plt.figure()
        ax = plt.axes()
        ax.set_xlim(0, self.num_of_time_steps)
        ax.set_ylim(1.5e34, -1.5e34)
        ax.set_xlabel('Number of time step')
        ax.set_ylabel('Energy(J)')
        x_coordinates = list(range(self.num_of_time_steps))
        for key in self.model.energy_dict:
            # Plot the energies at each time step
            y_coordinates = self.model.energy_dict[key]
            plt.plot(x_coordinates, y_coordinates, marker='.', markersize=1, label=key)
        plt.legend(loc="lower left")
        plt.show()


class Satellite:
    __sim = Simulation()
    init_data = {}

    def __init__(self):
        self.__sim.model.update_iteration(self.__sim.num_of_time_steps, self.__sim.time_step_length)
        self.init_data = self.__sim.model.object_data

    # ======================================== Probe Launching ========================================#

    # Clockwise rotation
    @staticmethod
    def __vector_rotation_origin(vector, radians):
        x, y = vector
        xx = x * np.cos(radians) + y * np.sin(radians)
        yy = -x * np.sin(radians) + y * np.cos(radians)
        return xx, yy

    def __reset_to_init(self):
        self.__sim.model.object_data = self.init_data
        for key in self.__sim.model.position_dict:
            self.__sim.model.position_dict[key] = []

    # Launches the probe from a range of initial velocities to find each viable initial velocity
    def __probe_launch(self, mass, init_pos, min_init_v, max_init_v, v_angle):
        suc_launch = []
        self.__sim.model.object_data['Probe']['mass'] = mass
        self.__sim.model.object_data['Probe']['position'] = init_pos
        # Restart the update iteration for every initial speed
        for v in range(min_init_v, max_init_v):
            print(f"checking outcome for initial launching velocity: {v} m/s")
            self.__sim.model.object_data['Probe']['velocity'] = self.__vector_rotation_origin((v, 0), v_angle)
            self.__sim.model.update_iteration(self.__sim.num_of_time_steps, self.__sim.time_step_length)
            # Check if it's viable
            launch = self.probe_state(v, v_angle)
            # counter == 0 => (failed to reach Mars)
            if launch[4] != 0:
                suc_launch.append(launch)
            self.__reset_to_init()
            print("done!")
        return suc_launch

    # ======================================== Probe State Checking ========================================#

    @staticmethod
    def radians_degrees(radians):
        return round(radians * 180 / np.pi, 2)

    # Check the minimum distance between the probe and Mars
    def min_distance(self, body_a, body_b):
        dl = []
        for i in range(self.__sim.num_of_time_steps):
            pos_a = self.__sim.model.position_dict[body_a][i]
            pos_b = self.__sim.model.position_dict[body_b][i]
            distance = np.linalg.norm(np.subtract(pos_a, pos_b))
            dl.append(distance)
        print(min(dl))

    # Check if the distance between two bodies is within range at the given time step
    def __distance_checker(self, body_a, body_b, min_distance, max_distance, i):
        pos_a = self.__sim.model.position_dict[body_a][i]
        pos_b = self.__sim.model.position_dict[body_b][i]
        distance = np.linalg.norm(np.subtract(pos_a, pos_b))
        if min_distance <= distance <= max_distance:
            return True

    # Checking if the probe has completed the objectives.
    # 0 = Did not reach Mars within given time
    # 1 = Reached Mars but failed to return to Earth
    # 2 = Reached Mars and returned to Earth
    def probe_state(self, init_v, angle):
        counter = 0
        time_taken = -1
        time_difference = -1
        for i in range(self.__sim.num_of_time_steps):
            in_mars_range = self.__distance_checker('Probe', 'Mars', 3.69e6, 4.839e7, i)
            in_earth_range = self.__distance_checker('Probe', 'Earth', 6.678e6, 5.1378e7, i)
            # If Reached Mars
            if in_mars_range and counter == 0:
                counter = 1
                time_taken = self.__sim.convert_to_days(self.__sim.time_step_length, i)
                time_difference = abs(333 - time_taken)
            # If returned to Earth
            if in_earth_range and counter == 1:
                counter = 2
        return init_v, angle, time_taken, time_difference, counter

    # ======================================== Viable Condition Output ========================================#

    # Write successful launches to file
    # Containing the following info:
    # Launching speed,
    # launching direction,
    # time taken to Mars,
    # time difference with the viking 2 probe,
    # whether return to Earth
    def write_suc_launch(self, mass, init_pos, min_init_v, max_init_v, v_angle):
        file = open('Viable Initial Velocities.txt', 'w')
        suc_launch = self.__probe_launch(mass, init_pos, min_init_v, max_init_v, v_angle)
        print(f"\nFound {len(suc_launch)} viable initial v.\n")
        for launch in suc_launch:
            if launch[4] == 1:
                string = "N"
            else:
                string = "Y"
            file.write(f"Launch speed: {launch[0]} m/s\n"
                       f"direction: {launch[1]} radians({self.radians_degrees(launch[1])} degrees) clockwise\n"
                       f"duration: {launch[2]} days to reach Mars\n"
                       f"delta t (viking 2): {launch[3]} days\n"
                       f"Return to Earth?: {string}\n\n")
        file.close()


test = Simulation()
test.initialize_dicts()
# 1.3 Project Task
test.display_simulation()
test.print_orbital_period(unwanted_bodies=['Sun', 'Probe'])
test.write_te_to_file()
# 1.4.1 Energy Conservation
test.display_energy_graph()
# 1.4.2 Satellite to Mars
test1 = Satellite()
# test1.min_distance('Probe', 'Mars')
test1.write_suc_launch(2328, (1.5e11, -7.378e6), 26406, 26410, 0.2709)
