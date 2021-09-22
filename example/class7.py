class MyClass():
    name = 'Hoge'
    def __init__(self, hige):
        self.hige = hige

class Greeting(object):
    message = 'Hi! I am MyClass2'
    def __init__(self, location):
        print('hoge!!')
        print(self.__class__.__mro__)
        self.location = location

    def greet(self):
        print(self.message)
        print('I am from', self.location)
        
class MySubClass(Greeting, MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        print('Hige!!')
        print(Greeting)
        print(Greeting.__init__)
        print('Hige!!')
        Greeting.__init__(self, fuga)
        self.piyo = piyo

obj = MySubClass('Tokyo', 2)

print(obj.name)
print(obj.hige)
print(obj.piyo)
print(obj.location)

obj.greet()
