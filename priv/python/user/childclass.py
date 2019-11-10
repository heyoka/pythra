from user.baseclass import Base


class Child(Base):

    def __init__(self, value1, value2):
        Base.__init__(self, value1, value2)
        self.value2 = value2

    def get_value2(self):
        return self.value2
