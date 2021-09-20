class Dog:
    kind = 'canine'         # class variable shared by all instances
    
    def __init__(self, name):
        self.name = name    # instance variable unique to each instance

    def bark(self):
        print(self.name)
        
d = Dog('Fido')
e = Dog('Buddy')

print(d.kind, e.kind)                  # shared by all dogs
# ---> 'canine', 'canine'
print(d.name, e.name)                  # unique to d
# ---> 'Fido', 'Buddy'

print(e)
print(d)


print()

d.name = 'hoge'

print(d.kind, e.kind)                  # shared by all dogs
# ---> 'canine', 'canine'
print(d.name, e.name)                  # unique to d
# ---> 'Fido', 'Buddy'

print(e)
print(d)

print()

e.kind = 'hige'

print(d.kind, e.kind)                  # shared by all dogs
# ---> 'canine', 'canine'
print(d.name, e.name)                  # unique to d
# ---> 'Fido', 'Buddy'

print(e)
print(d)


e.bark ()

d.bark ()
