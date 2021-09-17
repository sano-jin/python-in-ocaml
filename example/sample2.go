
# test 2
let X = 4;
let Y = 1;
let Z = 0;
func f (x, y) {
  return func (z) {
    Z := 2;
    return x + y * z;
    Z := 5; # Never reaches here
  };
}
while (Z < X) {
  Y := 2 * Y;
  print Y;
  Z := Z + 1;
}
print f (Y, 7)(3);
# let Z = 3;
print Z;