class Dog:
    kind = 'canine'         # class variable shared by all instances
    
    def __init__(self, name):
        self.name = name    # instance variable unique to each instance

    def bark(self):
        print(self.name)
        
d = Dog('Fido')
e = Dog('Buddy')

print(d.kind, e.kind)       # shared by all dogs
# ---> 'canine', 'canine'
print(d.name, e.name)       # unique to d and e
# ---> 'Fido', 'Buddy'

e.kind = 'hige'             # shared by all dogs
d.name = 'hoge'             # unique to e

print(d.kind, e.kind)       # shared by all dogs
# ---> 'canine', 'hige'
print(d.name, e.name)       # unique to d and e
# ---> 'hoge', 'Buddy'

print(e)
print(d)

d.bark()
# ---> 'hoge'
e.bark()
# ---> 'Buddy'
