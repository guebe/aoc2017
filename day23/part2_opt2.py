h = 0
for b in range(106700, 123700+17, 17):
    f = 1
    for d in range(2, b+1):
        for e in range(2, b+1):
            if d*e == b:
                f = 0
            if d*e > b: #optimization 1
                break 
        if f == 0: #optimization 2
            break
    if f == 0:
        h = h + 1
        print(h)

