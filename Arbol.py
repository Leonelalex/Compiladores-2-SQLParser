class NodoGramatical:
    def __init__(self, produccion):
        self.produccion = produccion
        self.reglas = []
    def add(self,regla):
        self.reglas.append(regla)

class Nodo:
    def __init__(self,nodoast,nodog):
        self.instruccion = nodoast
        self.nodo = nodog

class NodoG:
    def __init__(self,indic, nombre, childs = []):
        self.index = indic
        self.nombre = nombre
        self.childs= childs
    def add(self, child):
        self.childs.append(child)

