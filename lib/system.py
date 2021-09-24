class None():
    pass

class Exception():
    pass


class int():
    def __add__(self, other):
        return self + other

    def __mul__(self, other):
        return self * other

    def __lt__(self, other):
        return self < other

    def __gt__(self, other):
        return self + other

    def __eq__(self, other):
        return self == other

    def __neq__(self, other):
        return self != other

    def __str__(self, other):
        return str(self)

    def __int__(self, other):
        return bool(self)

    def __bool__(self, other):
        return bool(self)



class boolean():
    def __not__(self, other):
        return not self

    def __eq__(self, other):
        return self == other

    def __neq__(self, other):
        return self != other

    def __str__(self, other):
        return str(self)

    def __int__(self, other):
        return bool(self)

    def __bool__(self, other):
        return bool(self)


class string(): 
    def __add__(self, other):
        return self + other

    def __mul__(self, other):
        return self * other

    def __lt__(self, other):
        return self < other

    def __gt__(self, other):
        return self + other

    def __eq__(self, other):
        return self == other

    def __neq__(self, other):
        return self != other

    def __str__(self, other):
        return str(self)

    def __int__(self, other):
        return int(self)

    def __bool__(self, other):
        return bool(self)




    


    
