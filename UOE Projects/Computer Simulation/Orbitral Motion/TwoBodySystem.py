import numpy as np
from matplotlib import pyplot as plt
from matplotlib.animation import FuncAnimation


class TwoBodySystem:
    __G = 6.674e-11
    __mars_pos_list = []
    __phobos_pos_list = []
    __init_KE = 0

    def __init__(self):
        # read initial parameters
        f = open("parameters", "r")
        self.__dt = float(f.readline())
        self.__num_time_step = int(f.readline())
        self.__mars_mass = float(f.readline())
        self.__phobos_mass = float(f.readline())
        f.close()

        # read initial vectors
        with open("vectors") as f2:
            my_list = [list(map(float, i.split(','))) for i in f2]
        self.__r1 = np.array(my_list[0])
        self.__v1 = np.array(my_list[1])
        self.__r2 = np.array(my_list[2])
        self.__v2 = np.array(my_list[3])
        self.__init_KE = 0.5 * self.__mars_mass * (np.linalg.norm(self.__v1) ** 2) + \
                     0.5 * self.__phobos_mass * (np.linalg.norm(self.__v2) ** 2)

    def __calculate_r_between(self):
        r12 = self.__r2 - self.__r1
        r21 = self.__r1 - self.__r2
        return r12, r21

    def __calculate_mag(self):
        mag_r12 = np.linalg.norm(self.__calculate_r_between()[0])
        mag_r21 = np.linalg.norm(self.__calculate_r_between()[1])
        return mag_r12, mag_r21

    def __calculate_a(self):
        r_between = self.__calculate_r_between()
        mags = self.__calculate_mag()
        mars_a = np.multiply(r_between[1],
                             -self.__G * self.__phobos_mass / mags[1] ** 3)
        phobos_a = np.multiply(r_between[0],
                               -self.__G * self.__mars_mass / mags[0] ** 3)
        # a1(t), a2(t)
        return mars_a, phobos_a

    def __update_v(self):
        accs = self.__calculate_a()
        self.__v1 = self.__v1 + np.multiply(accs[0], self.__dt)
        self.__v2 = self.__v2 + np.multiply(accs[1], self.__dt)
        # v1(t+dt), v2(t+dt)
        return self.__v1, self.__v2

    def __update_r(self):
        vs = self.__update_v()
        self.__mars_pos_list.append(tuple(self.__r1))
        self.__phobos_pos_list.append(tuple(self.__r2))
        self.__r1 = self.__r1 + np.multiply(vs[0], self.__dt)
        self.__r2 = self.__r2 + np.multiply(vs[1], self.__dt)
        # r1(t+dt), r2(t+dt)

    def __update_iteration(self):
        counter = 0
        print(f"Initial KE = {self.__init_KE}\n")
        while counter < self.__num_time_step:
            counter += 1
            self.__update_r()
            self.__KE_info(counter)

    def __KE_info(self, counter):
        if counter % int(self.__num_time_step / 5) == 0:
            KE_delta = abs(self.__init_KE - self.__current_kinetic_energy())
            print(f"Iteration = {counter}\n" +
                  f"Current KE = {self.__current_kinetic_energy()}\n" +
                  f"KE delta = {KE_delta}\n"
                  f"Error Percentage = {round(100 * KE_delta / self.__init_KE, 3)}%\n")

    def __current_kinetic_energy(self):
        current_KE = 0.5 * self.__mars_mass * (np.linalg.norm(self.__v1) ** 2) + \
                     0.5 * self.__phobos_mass * (np.linalg.norm(self.__v2) ** 2)
        return current_KE

    def data_list(self):
        self.__update_iteration()
        return self.__mars_pos_list, self.__phobos_pos_list

    def get_time_step(self):
        return self.__num_time_step

    def get_mars_pos(self):
        return self.__r1

    def get_phobos_pos(self):
        return self.__r2


class Animation:
    system = TwoBodySystem()

    def __init__(self):
        self.phobos = plt.Circle(tuple(self.system.get_phobos_pos()), 5e5, color='grey', animated=True)
        self.mars = plt.Circle(tuple(self.system.get_mars_pos()), 2e6, color='chocolate', animated=True)
        self.d = self.system.data_list()

    def animate_mars(self, i):
        self.mars.center = self.d[0][i]
        return self.mars,

    def animate_phobos(self, i):
        self.phobos.center = self.d[1][i]
        return self.phobos,

    def display(self):
        fig = plt.figure()
        ax = plt.axes()

        ax.add_patch(self.mars)
        ax.add_patch(self.phobos)

        ax.axis('scaled')
        ax.set_xlim(-1.5e7, 1.5e7)
        ax.set_ylim(-1.5e7, 1.5e7)

        anim_mars = FuncAnimation(fig,
                                  self.animate_mars,
                                  frames=self.system.get_time_step(),
                                  interval=0.2,
                                  repeat=True,
                                  blit=True)

        anim_phobos = FuncAnimation(fig,
                                    self.animate_phobos,
                                    frames=self.system.get_time_step(),
                                    interval=0.2,
                                    repeat=True,
                                    blit=True)
        plt.show()


test = Animation()
test.display()
