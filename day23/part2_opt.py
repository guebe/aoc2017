b = 106700
c = 123700
h = 0
while True:
    f = 1
    d = 2
    while True: 
        e = 2
        while True:
            if d*e == b:
                f = 0
            if d*e > b: #optimization 1
                break 
            e = e + 1
            if e == b:
                break
        d = d + 1
        if f == 0: #optimization 2
            break
        if d == b:
            break
    if f == 0:
        h = h + 1
        print(h)
    if b == c:
        break
    b = b + 17

print(h)
