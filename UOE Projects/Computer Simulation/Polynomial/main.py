from polynomial import Polynomial


def main():
    # *args are the coefficient of the polynomial corresponding to the index of the list
    # for example, a0 == polynomialA.coefficient[0]
    polynomialA = Polynomial(2, 0, 4, -1, 0, 6)
    polynomialB = Polynomial(-1, -3, 0, 4.5)

    # Q1
    print(polynomialA.order())

    # Q2
    polynomialA.addition(polynomialB).print_polynomial()

    # Q3
    polynomialA.derivative().print_polynomial()

    # Q4
    polynomialA.derivative().integral(2).print_polynomial()


main()
