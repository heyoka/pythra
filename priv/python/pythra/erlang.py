from erlport.erlterms import List as ErlPortList


class List(ErlPortList):

    def __init__(self, data):
        ret_data = data
        if isinstance(ret_data, str):
            ret_data = self.convert_string(ret_data)
        super().__init__(ret_data)

    @staticmethod
    def convert_string(string):
        return [ord(x) for x in string]
