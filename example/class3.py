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
        print('---------- enter MyClass.__init__ ----------')
        print('     ----- super().__init__ is ', super().__init__)
        super().__init__()
        self.location = location

    def greet(self):
        print(self.message)
        print('I am from', self.location)
        
class MySubClass(Greeting, MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        print('---------- enter MySubClass.__init__ ----------')
        print('     ----- super().__init__ is ', super().__init__)
        super().__init__(fuga)
        print('---------- exit MySubClass.__init__ ----------')
        self.piyo = piyo

print('Hoge')        
obj = MySubClass('Tokyo', 2)
print('hige')
# print(obj)
# print(obj.__class__.__mro__)
print(obj.name)
print(obj.hige)
print(obj.piyo)
# print(obj.__class__)
print(obj.location)

obj.greet()
