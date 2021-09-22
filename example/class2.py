class object:
    def __init__(self):
        print('---------- enter object.__init__ ----------')
        print('---------- exit object.__init__ ----------')



class MyClass(object):
    name = 'Hoge'
    def __init__(self2, hige):
        print('---------- enter MyClass.__init__ ----------')
        print('     ----- super().__init__ is ', super().__init__)
        super().__init__()
        print('---------- exit MyClass.__init__ ----------')
        self2.hige = hige

class MySubClass(MyClass):
    hige = 'Hige'
    def __init__(self, fuga, piyo):
        print('---------- enter MySubClass.__init__ ----------')
        print('     ----- super().__init__ is ', super().__init__)
        super().__init__(fuga)
        print('---------- exit MySubClass.__init__ ----------')
        self.piyo = piyo
        
obj = MySubClass(1, 2)
# print(obj)
print(obj.__class__.__mro__)
print(' ---- ')
print(obj.name)
print(obj.hige)
print(obj.piyo)
# print(obj.__class__)
