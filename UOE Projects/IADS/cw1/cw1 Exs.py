import functools
import time


def ex1():
    return [x for x in list(range(5, 134)) if x % 2 == 0]


def ex2_1():
    return ["Python"[0:i] for i in range(len("Python"))]


def ex2_2():
    return ["Python"[i:5] for i in range(len("Python"))]


def ex2_3():
    string = "Python"
    return [string[i:j] for i in range(len(string)) for j in range(i+1, len(string)+1)]


def ex3():
    return [(x, y, 10-x-y) for x in range(1, 9) for y in reversed(range(1, 10-x))]


def ex4_1():
    return [x for x in range(1, 1000) if x % 5 == 0]


def ex4_2():
    return [x * 5 for x in range(1, 200)]


def ex5(n):
    lst = [[i, n//i] for i in range(2, int(n**0.5)+1) if n % i == 0]
    if not lst:
        return []
    return list(set(functools.reduce(list.__add__, lst)))


def ex6():
    return [n for n in range(2, 1000) if not ex5(n)]


t0 = time.time()
print(ex6())
t1 = time.time()
print("time taken:", t1-t0)
