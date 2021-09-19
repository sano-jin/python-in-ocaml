def hoge ():
    y = 0
    def fuga ():
        print (y)
        def hige ():
            nonlocal y
            y = 1
        return hige
    return fuga
h = hoge () 
h ()() # ---> 0
h ()() # ---> 1
