
class TokenError:
    def __init__(self, tipo, descripcion, line, column):
        self.tipo = tipo
        self.descripcion = descripcion
        self.line = line
        self.column = column