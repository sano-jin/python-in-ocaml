y = 0
def hoge ():
    def hige ():
        print(y)
    y = 1
    return hige
h = hoge()
h() # ---> 1
