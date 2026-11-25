b = 106700
c = 123700
h = 0
while True:
    f = 1
    d = 2
    while True: 
        e = 2
        while True:
            g = d*e-b
            if g == 0:
                f = 0
            e = e + 1
            g = e - b
            if g == 0:
                break
        d = d + 1
        g = d - b
        if g == 0:
            print("outer")
            break
    if f == 0:
        h = h + 1
        print(h)
    g = b - c
    if g == 0:
        break
    b = b + 17

print(h)
