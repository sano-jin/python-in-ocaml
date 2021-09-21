class MyClass:
    name = 'Hoge'
    def __init__(self, hige):
        self.hige = hige

class MySubClass(MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        super().__init__(fuga)
        self.piyo = piyo
        
obj = MySubClass(1, 2)
print(obj)
print(dir(obj))
print(obj.name)
print(obj.hige)
print(obj.piyo)
print(dir(obj.__class__))
print(obj.__class__.__bases__)
