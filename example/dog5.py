class Dog:
    kind = 'canine'
    
    def __init__(self, name):
        self.name = name

    def bark(self):
        print(self.name)
        
    def bark2(_):
        print('hoge')
        
    def bark3(self):
        print(self.name)

print(Dog)
        
d = Dog('Fido')
e = Dog('Buddy')



print(e)
print(d)

d.bark()
# ---> 'hoge'
e.bark()
# ---> 'Buddy'

e.bark2()
# ---> 'hoge'

Dog.bark3(d)

print(e.__class__.__name__)
# ---> 'hoge'
