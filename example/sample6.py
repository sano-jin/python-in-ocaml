y = 0
def hoge (y):
    def hige ():
        print(y)
    y = 1
    def fuga ():
        nonlocal y
        y = y + 2
        print(y)
        return hige
    return fuga
h = hoge(3)
h()
h()()
h()()
print(y)
