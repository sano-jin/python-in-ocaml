class Dog:
    kind = 'canine'
    
    def __init__(self, name):
        self.name = name

    def bark(self):
        print(self.name)
        
d = Dog('Fido')
e = Dog('Buddy')

print(e)
print(d)

d.bark()
# ---> 'hoge'
e.bark()
# ---> 'Buddy'
