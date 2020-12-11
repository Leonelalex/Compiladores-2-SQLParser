import ply.yacc as yacc
import ply.lex as lex
import subprocess
from Recolector import TokenError

index = 0
lista_errores = []


def cmd(commando):
    subprocess.run(commando, shell=True)

class NodoGrammatical:

    def __init__(self, produccion):
        self.produccion = produccion
        self.reglas = []

    def add(self, regla):
        self.reglas.append(regla)

class Nodo:

    def __init__(self, nodoast, nodog):
        self.instruccion = nodoast
        self.nodo = nodog

class NodoG:

    def __init__(self, indic, nombre, childs = []):
        self.index = indic
        self.nombre = nombre
        self.childs = childs

    def add(self, child):
        self.childs.append(child)


def agregar_error(tipo, descripcion, line, column):
    global lista_errores
    new_error = TokenError(tipo, descripcion, line, column)
    lista_errores.append(new_error)


def graficar_errores():
    global lista_errores


lista_gramaticales = []

reservadas = {
    'select': 'SELECT',
    'from': 'FROM',
    'order': 'ORDER',
    'by': 'BY',
    'asc': 'ASC',
    'desc': 'DESC',
    'where': 'WHERE',
    'join': 'JOIN',
    'inner': 'INNER',
    'left': 'LEFT',
    'right': 'RIGHT',
    'full': 'FULL',
    'outer': 'OUTER',
    'and': 'AND',
    'or' : 'OR'
}

tokens = [
    'COMA',
    'PYCOMMA',
    'ASTERISCO',
    'IGUAL',
    'ID',
    'CADENA',
    'ENTERO',
    'DECIMAL',
    'MAYORIGUAL',
    'MENORIGUAL',
    'DIFERENTE',
    'MAS',
    'MENOS',
    'MULTIPLICACION',
    'DIVICION',
    'COLID',
    'TABLAID',
    'MAYOR',
    'MENOR'
] + list(reservadas.values())

t_ignore = ' \t'

t_PYCOMA = r';'
t_COMA = r','
t_ASTERISCO = r'*'
t_IGUAL = r'='
t_MAYOR = r'>'
t_MENOR = r'<'
t_DIFERENTE = r'!='
t_MAYORIGUAL = r'>='
t_MENORIGUAL = r'<='
t_MAS = r'+'
t_MENOS = r'-'
t_MULTIPLICACION = r'*'
t_DIVICION = r'/'

def t_DECIMAL(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Error: %d ", t.value)
        t.value = 0
    return t

def t_ENTERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("valor entero muy largo %d", t.value)
        t.value = 0
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    t.type = reservadas.get(t.value.lower(), 'ID')
    return t

def t_COLID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    t.type = tokens.get(t.value.lower(), 'COLID')
    return t

def t_TABLAID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    t.type = tokens.get(t.value.lower(), 'TABLAID')
    return t

def t_CADENA(t):
    r'\'.*?\''
    t.value = t.value[1:-1]
    return t

def t_nuevalinea(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def getIndex():
    global index
    index = index + 1
    return index

def p_script(p):
    '''script : queries'''
    print('script')
    p[0] = Nodo(p[1], "script")

def p_queries(p):
    '''queries : selectcon'''
    print('queries')
    p[0] = Nodo(p[1], "queries")

def p_select(p):
    '''selectcon : SELECT ASTERISCO FROM TABLAID '''
    print('select')
    p[0] = Nodo(p[1], "select")

def t_error(t):
    #agregarError('Lexico', "Caracter \'{0}\' ilegal".format(t.value[0]), t.lexer.lineno + 1, find_column(t))
    t.lexer.skip(1)

def p_error(p):
    global input
    global parser
    #agregarError("Sintactico","Sintaxis no reconocida \"{0}\"".format(p.value),p.lineno+1, find_column(p))

    while True:
        tok = parser.token()
        if not tok or tok.type == 'PYCOMA':
            break
        parser.errorok()
        return tok


parser = yacc.yacc(write_tables=False)
input = ""

def parse(entrada):
    global index
    global parser
    global input
    lexer = lex.lex()
    lexer.lineno = 0
    input = entrada
    index = 0
    return parser.parse(entrada, lexer=lexer)

def restart():
    global parser
    parser.restart()

def find_column(token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1