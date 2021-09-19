
# test 2
X = 4
Y = 1
Z = 0
def f (x, y):
  return lambda z:
    Z = 2
    return x + y * z
    Z = 5 # Never reaches here
  


while Z < X:
  Y = 2 * Y
  print (Y)
  Z = Z + 1

print (f (Y, 7)(3))
# let Z = 3
print (Z)
