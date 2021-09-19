y = 0
def hoge ():
    nonlocal y
    y = 1
hoge()
print(y)
