y = 0
def hoge ():
    def hoge ():
        nonlocal y
        y = 1
    nonlocal y
    y = 1
hoge()
print(y)
