
class PolynomialPrinting:

    # Constructor that takes a PolynomialCalculation object
    def __init__(self, output_polynomial):
        self.output_polynomial = output_polynomial

    # Public Method

    # Prints our the polynomial on the terminal
    def print_polynomial(self):
        return "P(x) = " + self.__checking_zeros()

    # Private Method

    # Check if all coefficients are 0
    # return "0" if true else convert to string
    def __checking_zeros(self):
        first_element = self.output_polynomial.__coefficients[0]
        result = all(element == first_element and
                     first_element == 0
                     for element in self.output_polynomial.__coefficients)
        if result:
            return "0"
        else:
            return self.__to_string()

    # Convert coefficient list into a string
    # if coefficient < 0 => append "anx^n" to parts
    # if coefficient > 0 => append "+anx^n" to parts
    # otherwise, the term will not be appended
    def __convert(self):
        parts = []
        for i in range(len(self.output_polynomial.__coefficients)):
            if self.output_polynomial.__coefficients[i] < 0:
                parts.append(f'{self.output_polynomial.__coefficients[i]}x^{i}')
            elif self.output_polynomial.__coefficients[i] > 0:
                parts.append(f'+{self.output_polynomial.__coefficients[i]}x^{i}')
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
