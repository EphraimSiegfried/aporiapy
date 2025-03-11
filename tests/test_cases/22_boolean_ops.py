a = True
if a:
    b = False if 2 > 0 else a
    print(1 if b and (a or b) else 0)