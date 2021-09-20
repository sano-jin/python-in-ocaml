class Dog:
    kind = 'canine'         # class variable shared by all instances
    
    def __init__(self, name):
        self.name = name    # instance variable unique to each instance

d = Dog('Fido')
e = Dog('Buddy')

print(d.kind)                  # shared by all dogs
# ---> 'canine'
print(e.kind)                  # shared by all dogs
# ---> 'canine'
print(d.name)                  # unique to d
# ---> 'Fido'
print(e.name)                  # unique to e
# ---> 'Buddy'

print(e, d)
print()
print()

d.name = 'hoge'

print(d.kind)                  # shared by all dogs
# ---> 'canine'
print(e.kind)                  # shared by all dogs
# ---> 'canine'
print(d.name)                  # unique to d
# ---> 'Fido'
print(e.name)                  # unique to e
# ---> 'Buddy'

print()

e.kind = 'hige'

print(d.kind)                  # shared by all dogs
# ---> 'canine'
print(e.kind)                  # shared by all dogs
# ---> 'canine'
print(d.name)                  # unique to d
# ---> 'Fido'
print(e.name)                  # unique to e
# ---> 'Buddy'

