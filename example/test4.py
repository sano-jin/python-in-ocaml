class Hoge:
    fuga = 1
    def __init__ (self):
        self.hogehoge = 3
        print(self)
    def hige (self, piyo):
        print(self)
        self.hogehoge = 4
        print(self.fuga)
        print(piyo)
        print(self.hogehoge)
        print(self)

hoge = Hoge ()

hoge.hige (2)
