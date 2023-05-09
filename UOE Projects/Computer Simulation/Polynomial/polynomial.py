
class Polynomial:
    # Declaration of a list variable to hold the coefficients for P(x)
    __coefficients = []

    # Constructor which doesn't allow empty or non-numeric inputs
    def __init__(self, *args):
        if len(args) == 0:
            raise AttributeError("coefficients can't be empty")
        else:
            for arg in args:
                if type(arg) != int and type(arg) != float:
                    raise TypeError("coefficients must be numeric values")
                else:
                    self.__coefficients = [*args]

    # Public Methods

    # Calculating order of the polynomial
    def order(self):
        if self.__checking_zeros():
            return 0
        else:
            return len(self.__coefficients) - 1

    # Calculating the sum of two polynomials
    def addition(self, other):
        added_polynomial = Polynomial(0)
        added_polynomial.set_coefficients(self.__summation(other))
        return added_polynomial

    # Calculating the first order derivative of a polynomial
    def derivative(self):
        derived_polynomial = Polynomial(0)
        derived_polynomial.set_coefficients(self.__derivation())
        return derived_polynomial

    # Calculating the first order integral of a polynomial
    def integral(self, constant):
        integrated_polynomial = Polynomial(0)
        integrated_polynomial.set_coefficients(self.__adding_constant(constant))
        return integrated_polynomial

    # Prints the polynomial on the terminal
    def print_polynomial(self):
        if self.__checking_zeros():
            print("0")
        else:
            print("P(x) = " + self.__to_string())

    # Setters and Getters
    def set_coefficients(self, coefficients):
        if isinstance(coefficients, list):
            self.__coefficients = coefficients
        else:
            raise TypeError("self.coefficient should be a list.")

    def get_coefficients(self):
        return self.__coefficients

    # Private Methods

    # Compare 2 coefficient lists and calibrate them into the same length
    def __length_change(self, other):
        lengthA = len(self.__coefficients)
        lengthB = len(other.__coefficients)
        dif = abs(lengthA - lengthB)
        if lengthA > lengthB:
            lstB = other.__coefficients + dif * [0]
            return [self.__coefficients, lstB]
        elif lengthB > lengthA:
            lstA = self.__coefficients + dif * [0]
            return [lstA, other.__coefficients]
        else:
            return [self.__coefficients, other.__coefficients]

    # Sums the calibrated coefficient lists together
    def __summation(self, other):
        totalSum = []
        container = self.__length_change(other)
        for i in range(len(container[1])):
            totalSum.append(float(container[0][i] + container[1][i]))
        return totalSum

    # Derives the coefficient list of the first order derivative of a polynomial
    def __derivation(self):
        derivative1 = []
        for i in range(len(self.__coefficients)):
            derivative1.append(float(i * self.__coefficients[i]))
        return derivative1[1:]

    # Integrates the coefficient list of the first order integral of a polynomial
    # (without the constant)
    def __integration(self):
        integral1 = []
        for i in range(len(self.__coefficients)):
            integral1.append(float(self.__coefficients[i] / (i + 1)))
        return integral1

    # Checking the type and adding the constant to the coefficient list
    def __adding_constant(self, constant):
        if type(constant) != int and type(constant) != float:
            raise TypeError("constant must be a numeric value")
        else:
            base = list(self.__integration())
            base.insert(0, constant)
            return base

    # Check if all coefficients are 0
    # return "0" if true else convert to string
    def __checking_zeros(self):
        first_element = self.__coefficients[0]
        result = all(element == first_element and
                     first_element == 0
                     for element in self.__coefficients)
        return result

    # Convert coefficient list into a string
    # if coefficient < 0 => append "anx^n" to parts
    # if coefficient > 0 => append "+anx^n" to parts
    # otherwise, the term will not be appended
    def __convert(self):
        parts = []
        for i in range(len(self.__coefficients)):
            if self.__coefficients[i] < 0:
                parts.append(f'{self.__coefficients[i]}x^{i}')
            elif self.__coefficients[i] > 0:
                parts.append(f'+{self.__coefficients[i]}x^{i}')
            else:
                pass
        return parts

    # join the terms together and change "a0x^0" into "a0", change "a1x^1" into "a1x"
    # Also get rid of the + sign at the beginning of the string if the first term is positive
    def __to_string(self):
        string = "".join(self.__convert()).replace("^1", "").replace("x^0", "")
        if string[0] == "+":
            return string[1:]
        else:
            return string
