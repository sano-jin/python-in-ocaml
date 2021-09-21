# class object:
#     def __init__(self):
#         pass
#         
class MyClass():
    name = 'Hoge'
    def __init__(self, hige):
        super().__init__()
        self.hige = hige

class Greeting(object):
    message = 'Hi! I am MyClass2'
    def __init__(self, location):
        super(Greeting, self).__init__(3)
        print(self.__class__.__mro__)
        self.location = location

    def greet(self):
        print(self.message)
        print('I am from', self.location)
        
class MySubClass(Greeting, MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        super().__init__(fuga)
        self.piyo = piyo

print('Hoge')        
obj = MySubClass('Tokyo', 2)
print('hige')
print(obj.name)
print(obj.hige)
print(obj.piyo)
print(obj.location)
obj.greet()
