lst = list(range(1, 401))
counter = 0
for x in lst:
    if x % 2 == 0 and x % 11 ==0:
        counter += 1
print(counter)