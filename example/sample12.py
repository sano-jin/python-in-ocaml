# nonlocal y
def hoge ():
    y = 0
    def hige ():
        nonlocal y
        print(y)
        y = 2
        print(y)
    return hige
h = hoge()
h ()
h ()
