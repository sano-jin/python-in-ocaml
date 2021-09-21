class object:
    def __init__(self):

class MyClass(object):
    name = 'Hoge'
    def __init__(self, hige):
        super().__init__()
        self.hige = hige

class Greeting(object):
    message = 'Hi! I am MyClass2'
    def __init__(self, location):
        super().__init__()
        self.location = location

    def greet(self):
        print(self.message)
        print('I am from', self.location)
        
class MySubClass(Greeting, MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        super().__init__(fuga)
        self.piyo = piyo
        
obj = MySubClass('Tokyo', 2)
# print(obj)
# print(obj.__class__.__mro__)
print(obj.name)
print(obj.hige)
print(obj.piyo)
# print(obj.__class__)
print(obj.location)

obj.greet(obj)
