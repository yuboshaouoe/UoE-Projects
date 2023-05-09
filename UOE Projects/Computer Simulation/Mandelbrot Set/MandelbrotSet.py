import numpy as np
import matplotlib.pyplot as plt


class Mandelbrot:

    # Initialize the width, height of the image
    # and the min and max for x and y axis.
    def __init__(self, width, height, limits):
        if type(width) != int or type(height) != int:
            raise TypeError("pixel count must be an int")
        else:
            self.__width = width
            self.__height = height
        for limit in limits:
            if type(limit) != int and type(limit) != float:
                raise TypeError("limits must be numeric values")
            elif len(limits) > 4:
                raise AttributeError \
                    ("limits only takes 4 values: xmin, xmax, ymin, ymax")
            else:
                self.__xmin = limits[0]
                self.__xmax = limits[1]
                self.__ymin = limits[2]
                self.__ymax = limits[3]

    # Mandelbrot set definition
    # z0 = 0, if |z| > 2 or z^2 > 4, record the iterations.
    # if after the iteration bound the magnitude is still within 2,
    # return the iteration bound.
    @staticmethod
    def __mandelbrot(re, im, iter_bound):
        c = complex(re, im)
        z = 0
        for num_of_iter in range(iter_bound):
            if np.square(z.real) + np.square(z.imag) > 4:
                return num_of_iter
            z = z * z + c
        return iter_bound

    # Create a 2D numpy array corresponding to the width and height of the image
    # and each element corresponding to 1 pixel of the image.
    # The range of real and imaginary values are equally split by the pixel count
    # and assigned to the mandelbrot method for each element.
    # For all element, set its value to the iteration for magnitude to be > 2
    def __plot(self, width, height):
        iter_grid = np.zeros((height, width))
        for row_index, re in enumerate(np.linspace(self.__xmin, self.__xmax, num=height)):
            for column_index, im in enumerate(np.linspace(self.__ymin, self.__ymax, num=width)):
                iter_grid[row_index, column_index] = self.__mandelbrot(re, im, 255)
        return iter_grid

    # display the 2D array with a heatmap
    def show(self):
        plt.figure(dpi=100)
        plt.imshow(self.__plot(self.__width, self.__height).T,
                   cmap='hot',
                   interpolation='bilinear',
                   extent=[self.__xmin, self.__xmax, self.__ymin, self.__ymax])
        plt.xlabel('Re')
        plt.ylabel('Im')
        plt.show()


test = Mandelbrot(720, 720, [-2.025, 0.6, -1.125, 1.125])
test.show()
