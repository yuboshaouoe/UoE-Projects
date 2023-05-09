from numpy import complex, array, square
from PIL import Image
import colorsys


class FullMandelbrot:

    # noinspection PyRedundantParentheses
    def __mandelbrot(self, x, y, iter_bound):
        c = complex(x, y)
        z = 0
        for num_of_iter in range(iter_bound):
            if square(z.real) + square(z.imag) > 4:
                return self.__iteration_to_color(num_of_iter)
            z = z * z + c
        return (0, 0, 0)

    @staticmethod
    def __iteration_to_color(num_of_iter):
        rgb_values = 255 * array(colorsys.hsv_to_rgb(num_of_iter / 255.0, 1.0, 0.5))
        return tuple(rgb_values.astype(int))

    def __render(self, width):
        img = Image.new('RGB', (width, int(width / 2)))
        pixels = img.load()

        for x in range(img.size[0]):
            print("%.2f %%" % (x / width * 100.0))
            for y in range(img.size[1]):
                pixels[x, y] = self.__mandelbrot((x - (0.75 * width)) / (width / 4),
                                                 (y - (width / 4)) / (width / 4), 255)
        return img

    def display(self, width):
        self.__render(width).show()


test = FullMandelbrot()
test.display(1024)
