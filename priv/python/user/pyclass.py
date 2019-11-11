from erlport.erlterms import Map


class Pyclass:

    def __init__(self, value):
        print("pyClass __init__")

        print(value)
        print(type(value))
        # if isinstance(value, list):
        #     myval = list()
        #     for d in value:
        #         myval.append(dict(d))
        # elif isinstance(value, dict):
        #     myval = dict(value)
        # print("myval", myval)
        self.value = value

    def get_value(self):
        return self.value
