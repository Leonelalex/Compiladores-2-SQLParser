class Instruccion():
    def __init__(self, tipo, instruccion):
        self.tipo = tipo 
        self.instruccion = instruccion

class Select():
    def __init__(self, dist, selcol, fromcol, joins, order, conditions):
        self.dist = dist
        self.selcol = selcol
        self.fromcol = fromcol
        self.joins = joins
        self.order = order
        self.conditions = conditions

    def execute():
        #Llamar metodo que realizara el select
        print('ejecutando select')

class AlterTable():
    def __init__():
        print('init')

    def execute():
        print('ejecutando alter table')

class CreateDB():
    def __init__(self, id, owner, mode):
        self.id = id
        self.owner = owner
        self.mode = mode

    def execute(self):
        print('Ejecutando Create DB')
        print('db id : ' + str(self.id))
        print('owner : ' + str(self.owner))
        print('mode : ' + str(self.mode))

class ShowDB():
    def __init__(self):
        print('show')
    def execute():
        print('Ejecutando ShowDB')

class Drop():
    def __init__(self, id):
        self.id = id

    def execute(self):
        print('Ejecutando Drop')
        print('id : ' + self.id)

class CreateTable():
    def __init__(self, id, cols, inh):
        self.id = id
        self.cols = cols
        self.inh = inh
        
    def execute(self):
        print('Ejecutando Creare Table')
        print('id : ' + str(self.id))

        for col in self.cols :
            print('col id : ' + str(col[0]))
            print('col type : ' + str(col[1]))


        if self.inh != None :
            print('Inherit : ' + self.inh)


class Insert():
    def __init__():
        print('init')
    def execute():
        print('Ejecutando Insert')

class UseDB():
    def __init__(self, id):
        self.id = id

    def execute(self):
        print('Ejecutando Use DB')
        print('id : ' + self.id)
