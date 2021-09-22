class object:
    def __init__(self):
        pass
        
class MyClass(object):
    def __init__(self, hige):
        pass

class MySubClass1(object):
    def __init__(self, location):
        print('>>> __mro__ is ', self.__class__.__mro__)
        # ---> MySubClass2, MySubClass1, MyClass, object
        super().__init__()
        # When this is called from the initializer of the MySubClass2,
        # The initializer of the MyClass (not object) is called !!!
        # Thus the argument is needed fot this time.
        
class MySubClass2(MySubClass1, MyClass):
    def __init__(self, fuga, piyo):
        super().__init__(fuga)
        # Calls the initializer of the MySubClass2.

obj = MySubClass2('Tokyo', 2)
# obj = MySubClass1('Tokyo')
## THIS YIELDS ERROR because of the arity of the initializer (super().__init__(3))
## since the super().__init__ refers to the initializer of the object class this time.


