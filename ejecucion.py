import ply.yacc as yacc
import ply.lex as lex
import re

import instrucciones as ins 

ast_graph = Digraph(comment='AST', engine='dot')

errores_lexicos = ""        #Variable para concatenar los errores lexicos y luego agregarlos al archivo de errores
errores_sintacticos = ""    #Variable para concatenar los errores sintacticos y luego agregarlos al archivo de errores
cont_error_lexico = 0
cont_error_sintactico = 0
linea = 1
columna = 1

i = 0
lst_instrucciones = []

def inc_index():
    global i
    i += 1
    return i

# Lista de palabras reservadas
reservadas = {
    'create' : 'CREATE',
    'type' : 'TYPE',
    'as' : 'AS',
    'enum' : 'ENUM',
    'replace' : 'REPLACE',
    'database' : 'DATABASE',
    'if' : 'IF',
    'not' : 'NOT',
    'exists' : 'EXISTS',
    'or' : 'OR',
    'owner' : 'OWNER',
    'mode' : 'MODE',
    'show' : 'SHOW',
    'like' : 'LIKE',
    'databases' : 'DATABASES',
    'rename' : 'RENAME',
    'currente_user' : 'CURRENT_USER',
    'session_user' : 'SESSION_USER',
    'text' : 'TEXT',
    'numeric' : 'NUMERIC',
    'integer' : 'INTEGER',
    'alter' : 'ALTER',
    'to' : 'TO',
    'drop'      : 'DROP',
    'table'     : 'TABLE',
    'default'   : 'DEFAULT',
    'primary'   : 'PRIMARY',
    'key'       : 'KEY',
    'foreign'   : 'FOREIGN',
    'null'      : 'NULL',
    'constraint' : 'CONSTRAINT',
    'unique'    : 'UNIQUE',
    'check'     : 'CHECK',
    'references': 'REFERENCES',
    'smallint'  : 'SMALLINT',
    'bigint'    : 'BIGINT',
    'decimal'   : 'DECIMAL',
    'real'      : 'REAL',
    'double'    : 'DOUBLE',
    'precision' : 'PRECISION',
    'money'     : 'MONEY',
    'character' : 'CHARACTER',
    'varying'   : 'VARYING',
    'varchar'   : 'VARCHAR',
    'char'      : 'CHAR',
    'timestamp' : 'TIMESTAMP',
    'data'      : 'DATA',
    'time'      : 'TIME',
    'interval'  : 'INTERVAL',
    'with'      : 'WITH',
    'without'   : 'WITHOUT',
    'zone'      : 'ZONE',
    'column'    : 'COLUMN',
    'add'       : 'ADD',
    'delete'    : 'DELETE',
    'from'      : 'FROM',
    'where'     : 'WHERE',
    'insert'    : 'INSERT',
    'into'      : 'INTO',
    'values'    : 'VALUES',
    'update'    : 'UPDATE',
    'set'       : 'SET',
    'and'       : 'AND',
    'sum'       : 'SUM',
    'avg'       : 'AVG',
    'max'       : 'MAX',
    'pi'        : 'PI',
    'power'     : 'POWER',
    'sqrt'      : 'SQRT',
    'select'    : 'SELECT',
    'inner'     : 'INNER',
    'left'      : 'LEFT',
    'right'     : 'RIGHT',
    'full'      : 'FULL',
    'outer'     : 'OUTER',
    'on'        : 'ON',
    'join'      : 'JOIN',
    'order'     : 'ORDER',
    'by'        : 'BY', 
    'asc'       : 'ASC',
    'desc'      : 'DESC',
    'inherits'  : 'INHERITS',
    'distinct'  : 'DISTINCT'
}

# Lista de tokens
tokens = [
    'COMA',
    'PARIZQ',
    'PARDER',
    'PTCOMA',
    'MAYIG',
    'MENIG',
    'DIFEQ',
    'MAYOR',
    'MENOR',
    'IGUAL',
    'MULTI',
    'MENOS',
    'SUMAS',
    'DIVIS',
    'POTEN',
    'CADENA',
    'ID',
    'DECIMA',
    'ENTERO',
    'PUNTO'
] + list(reservadas.values())

# Expresiones regulares par los tokens
t_COMA = r','
t_PARIZQ = r'\('
t_PARDER = r'\)'
t_PTCOMA = r';'
t_MAYIG = r'>='
t_MENIG = r'<='
t_DIFEQ = r'<>'
t_MAYOR = r'>'
t_MENOR = r'<'
t_IGUAL = r'='
t_MULTI = r'\*'
t_MENOS = r'-'
t_SUMAS = r'\+'
t_DIVIS = r'/'
t_POTEN = r'\^'
t_PUNTO = r'.'

t_ignore = " \t"

def t_COMENTARIO_S(t):
    r'--.*\n'
    global linea, columna
    linea = linea + 1
    columna = 1
    t.lexer.lineno += 1

def t_COMENTARIO_M(t):
    r'/\*(.|\n)*?\*/'
    global linea, columna
    linea = linea + t.value.count('\n')
    columna = 1
    t.lexer.lineno += t.value.count('\n')

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reservadas.get(t.value.lower(),'ID')    # Revisa las palabras reservadas 
     return t

def t_CADENA(t):
    r'([\"]|[\']).*?([\"]|[\'])'
    t.value = t.value[1:-1] # Remueve las comillas
    return t 

def t_DECIMA(t):
    r'\d+[.]\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Float value too large %d", t.value)
        t.value = 0
    return t

def t_ENTERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_nuevalinea(t):
    r'\n+'
    global linea, columna
    linea = linea + t.value.count('\n')
    columna = 1
    t.lexer.lineno += t.value.count("\n")

# Errores léxicos
def t_error(t):
    global linea, columna
    lex_error(t.value[0], linea, t.lexpos)
    t.lexer.skip(1)


lexer = lex.lex(reflags=re.IGNORECASE)

# Asociación de operadores y precedencia
precedence = (
    ('left','SUMAS','MENOS'),
    ('left','MULTI','DIVIS'),
    ('left','POTEN'),
    ('right','UMENOS', 'USUMAS'),
    ('left','MAYIG','MENIG','IGUAL','DIFEQ','MAYOR','MENOR'),
    ('right','NOT'),
    ('left','AND'),
    ('left','OR'),
    ) 

# Definir gramática

def p_entrada(p):
    '''entrada : entrada create_type
                | entrada create_db
                | entrada show_db
                | entrada alter_db
                | entrada drop_db
                | entrada create_table
                | entrada drop_table
                | entrada alter_table
                | entrada s_delete
                | entrada s_insert
                | entrada s_update
                | entrada s_select
                | create_type
                | create_db
                | show_db
                | alter_db 
                | drop_db 
                | create_table
                | drop_table
                | alter_table
                | s_delete
                | s_insert
                | s_update
                | s_select'''

    #AST graphviz
    try:



    except IndexError:
        print('')

#region 'Select Analisis'

def p_s_select(p):
    '''s_select : SELECT dist list_cols FROM list_from PTCOMA'''
    ins.Select(p[2], p[3], p[5], None, None, None).ejecutar()
    

def p_s_select2(p):
    '''s_select : SELECT dist list_cols FROM list_from list_conditions PTCOMA'''
    ins.Select(p[2], p[3], p[5], None, None, p[6]).ejecutar()
    
def p_s_select3(p):
    '''s_select : SELECT dist list_cols FROM list_from list_order PTCOMA'''
    ins.Select(p[2], p[3], p[5], None, p[6], None).ejecutar()

def p_s_select4(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins PTCOMA'''
    ins.Select(p[2], p[3], p[5], p[6], None, None).ejecutar()

def p_s_select5(p):
    '''s_select : SELECT dist list_cols FROM list_from list_conditions list_order PTCOMA'''
    ins.Select(p[2], p[3], p[5], None, p[7], p[6]).ejecutar()

def p_s_select6(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_conditions PTCOMA'''
    ins.Select(p[2], p[3], p[5], p[6], None, p[7]).ejecutar()

def p_s_select7(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_order PTCOMA'''
    ins.Select(p[2], p[3], p[5], p[6], p[7], None).ejecutar()

def p_s_select8(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_conditions list_order PTCOMA'''
    ins.Select(p[2], p[3], p[5], p[6], p[8], p[7]).ejecutar()

def p_dist(p):
    '''dist : DISTINCT
             | '''
    try:
        if p[1]:
            p[0] = p[1]
        else: 
            p[0] = ' '
    except IndexError:
        print('out of range')

def p_list_cols(p):
    '''list_cols :  DISTINCT list_alias
                  | MULTI
                  | list_alias'''
    
    #AST graphviz
    try:

    except IndexError:
        print('out of range')
    


def p_list_alias(p):
    '''list_alias : list_alias COMA sel_id
                  | sel_id'''
    
    #AST graphviz
    try: 

    except IndexError:
        print('')

def p_sel_id(p):
    ''' sel_id : ID PUNTO ID AS ID
                  | ID PUNTO ID
                  | ID AS ID
                  | ID'''

    #AST graphviz
    try:


    except IndexError:
        print('')

def p_list_from(p):
    '''list_from :  list_from COMA from_id
                  | from_id'''
    
    #AST graphviz
    try:

    except IndexError:
        print('')

def p_from_id(p):
    '''from_id : ID AS ID
                | ID'''

    #AST graphviz
    try:

    except IndexError:
        print('')

    

def p_list_joins(p):
    '''list_joins : list_joins join_type JOIN ID join_conditions 
                  | list_joins JOIN ID join_conditions 
                  | join_type JOIN ID join_conditions
                  | JOIN ID join_conditions
                  | JOIN ID'''
    
    #AST graphviz
    try:

    except IndexError:
        print('')

    


def p_join_type(p):
    '''join_type : LEFT OUTER
                 | RIGHT OUTER
                 | FULL OUTER
                 | LEFT
                 | RIGHT
                 | FULL
                 | INNER'''
    #AST graphviz
    try:

    except IndexError:
        print('')


def p_join_conditions(p):
    '''join_conditions : ON expresion'''
    #AST graphviz
    try:



    except IndexError:
        print('')


def p_list_conditions(p):
    '''list_conditions : WHERE expresion'''

    #AST graphviz
    try:



    except IndexError:
        print('')

def p_list_order(p):
    '''list_order : ORDER BY ID ASC
                  | ORDER BY ID DESC'''

    #AST graphviz
    try:

    except IndexError:
        print('')

#end region 

def p_create_type(p):
    'create_type : CREATE TYPE ID AS ENUM PARIZQ lista1 PARDER PTCOMA'

    #AST graphviz
    try:
        


    except IndexError:
        print('')

    

def p_lista1(p):
    '''lista1 : lista1 COMA CADENA
            | CADENA'''
    
    #AST Graphviz
    try:

    
    except IndexError:
        print('')

def p_data_type(p):
    '''data_type : NUMERIC
            | INTEGER
            | TEXT
            | SMALLINT 
            | BIGINT
            | DECIMAL
            | REAL
            | DOUBLE PRECISION
            | MONEY
            | CHARACTER VARYING PARIZQ ENTERO PARDER
            | VARCHAR PARIZQ ENTERO PARDER
            | CHARACTER PARIZQ ENTERO PARDER
            | CHAR PARIZQ ENTERO PARDER
            | TIMESTAMP
            | TIMESTAMP time_zone
            | DATA
            | TIME
            | TIME time_zone
            | INTERVAL
            | ID'''
    try:


    except IndexError:
        print('')

    

def p_time_zone(p):
    '''time_zone    : WITH TIME ZONE
                    | WITHOUT TIME ZONE'''

    try:



    except IndexError:
        print('')

def p_create_db(p):
    '''create_db : CREATE DATABASE c_db PTCOMA
                 | CREATE OR REPLACE DATABASE c_db PTCOMA'''

    try:



    except IndexError:
        print('')


def p_c_db(p):
    '''c_db : IF NOT EXISTS c_db1
            | c_db1'''

    try:



    except IndexError:
        print('')

def p_c_db1(p):
    '''c_db1 : ID owner_mode
             | ID'''

    try: 

    except IndexError:
        print('')


def p_owner_mode(p):
    '''owner_mode : owner_mode OWNER igual_id 
                  | owner_mode MODE igual_int
                  | OWNER igual_id 
                  | MODE igual_int'''

    try:


    except IndexError:
        print('')

def p_igual_id(p):
    '''igual_id : IGUAL ID
                | ID'''

    try:



    except IndexError:
        print('')

def p_igual_int(p):
    '''igual_int : IGUAL ENTERO
                | ENTERO'''
    try:



    except IndexError:
        print('')

def p_show_db(p):
    '''show_db : SHOW DATABASES PTCOMA'''

    try:

    except IndexError:
        print('')

def p_alter_db(p):
    '''alter_db : ALTER DATABASE ID al_db PTCOMA'''

    try: 


    except IndexError:
        print('')
     

def p_al_db(p):
    '''al_db : RENAME TO ID
            | OWNER TO owner_db'''
    
    try:


    except IndexError:
        print('')

def p_owner_db(p):
    '''owner_db : ID
                | CURRENT_USER
                | SESSION_USER'''
    
    try:


    except IndexError:
        print('')

def p_drop_db(p):
    '''drop_db  : DROP DATABASE ID PTCOMA
                | DROP DATABASE IF EXISTS ID PTCOMA'''

    try: 


    except IndexError:
        print('')

def p_create_table(p): 
    '''create_table   : CREATE TABLE ID PARIZQ valores PARDER PTCOMA
                      | CREATE TABLE ID PARIZQ valores PARDER INHERITS PARIZQ ID PARDER PTCOMA'''

    try: 



    except IndexError:
        print('')

def p_valores(p):
    '''valores  : colum_list
                | colum_list COMA const_keys '''
    try: 



    except IndexError:
        print('')

def p_colum_list(p):
    '''colum_list   : colum_list COMA ID data_type 
                    | colum_list COMA ID data_type const
                    | ID data_type
                    | ID data_type const'''

    try:



    except IndexError:
        print('')

def p_const_keys(p):
    '''const_keys   : const_keys COMA PRIMARY KEY PARIZQ lista_id PARDER
                    | const_keys COMA FOREIGN KEY PARIZQ lista_id PARDER REFERENCES ID PARIZQ lista_id PARDER
                    | PRIMARY KEY PARIZQ lista_id PARDER
                    | FOREIGN KEY PARIZQ lista_id PARDER REFERENCES ID PARIZQ lista_id PARDER'''

    try:



    except IndexError:
        print('')


def p_const(p):
    '''const    : const DEFAULT valores
                | const NOT NULL
                | const NULL
                | const CONSTRAINT ID  UNIQUE
                | const CONSTRAINT ID  UNIQUE PARIZQ lista_id PARDER
                | const UNIQUE
                | const CONSTRAINT ID CHECK PARIZQ expresion PARDER
                | const CHECK PARIZQ expresion PARDER
                | const PRIMARY KEY
                | const REFERENCES ID PARIZQ lista_id PARDER
                | DEFAULT valores
                | NOT NULL
                | NULL
                | CONSTRAINT ID UNIQUE
                | CONSTRAINT ID  UNIQUE PARIZQ lista_id PARDER
                | UNIQUE
                | CONSTRAINT ID CHECK PARIZQ expresion PARDER
                | CHECK PARIZQ expresion PARDER
                | PRIMARY KEY
                | REFERENCES ID PARIZQ lista_id PARDER'''

    try:



    except IndexError:
        print('')

def p_lista_id(p):
    '''lista_id : lista_id COMA ID
                | ID'''

    try: 


    except IndexError:
        print('')

def p_drop_table(p):
    'drop_table : DROP TABLE ID PTCOMA'

    try:

    except IndexError:
        print('')

def p_alter_table(p):
    'alter_table    : ALTER TABLE ID acciones PTCOMA'
    try:


    except IndexError:
        print('')

def p_acciones(p):
    '''acciones : ADD acc
                | ADD COLUMN ID data_type
                | ALTER COLUMN ID TYPE data_type
                | ALTER COLUMN ID SET const
                | DROP CONSTRAINT ID
                | DROP COLUMN ID
                | RENAME COLUMN ID TO ID'''

    try:



    except IndexError:
        print('')

def p_acc(p):
    '''acc  : const
            | const_keys'''

    try:



    except IndexError:
        print('')

def p_delete(p):
    '''s_delete : DELETE FROM ID PTCOMA
                | DELETE FROM ID WHERE expresion PTCOMA '''

    try:



    except IndexError:
        print('')

def p_insert(p):
    '''s_insert : INSERT INTO ID PARIZQ lista_id PARDER VALUES lista_values PTCOMA
                | INSERT INTO ID VALUES lista_values PTCOMA '''
                #| INSERT INTO ID PARIZQ lista_id PARDER s_select
                #| INSERT INTO ID s_select''' 

    try:



    except IndexError:
        print('')

def p_lista_values(p):
    '''lista_values : lista_values COMA PARIZQ lista_valores PARDER
                     | PARIZQ lista_valores PARDER'''

    try:




    except IndexError:
        print('')

def p_lista_valores(p):
    '''lista_valores : lista_valores COMA valores
                     | valores'''
    
    try:



    except IndexError:
        print('')

def p_valores(p):
    '''valores : CADENA
               | ENTERO
               | DECIMA'''

    try:



    except IndexError:
        print('')

def p_s_update(p):
    '''s_update : UPDATE ID SET lista_asig PTCOMA
                | UPDATE ID SET lista_asig WHERE expresion PTCOMA'''

    try:



    except IndexError:
        print('')

def p_lista_asig(p):
    '''lista_asig : lista_asig COMA ID IGUAL valores
                  | ID IGUAL valores'''

    try:



    except IndexError:
        print('')


def p_expresion(p):
    '''expresion : NOT expresion
                 | expresion OR expresion
                 | expresion AND expresion
                 | expresion MAYOR expresion
                 | expresion MENOR expresion
                 | expresion MAYIG expresion
                 | expresion MENIG expresion
                 | expresion IGUAL expresion
                 | expresion DIFEQ expresion
                 | MENOS expresion %prec UMENOS
                 | SUMAS expresion %prec USUMAS
                 | expresion POTEN expresion
                 | expresion MULTI expresion
                 | expresion DIVIS expresion
                 | expresion SUMAS expresion
                 | expresion MENOS expresion
                 | PARIZQ expresion PARDER
                 | SUM PARIZQ expresion PARDER
                 | AVG PARIZQ expresion PARDER
                 | MAX PARIZQ expresion PARDER
                 | PI
                 | POWER PARIZQ expresion PARDER
                 | SQRT PARIZQ expresion PARDER
                 | ID
                 | ID PUNTO ID
                 | valores'''

    try:


    
    except IndexError:
        print('')


def p_error(p):
    global linea, columna
    sin_error(p.type, p.value, linea, p.lexpos)
    columna = columna + len(p.value)


# Construyendo el analizador sintáctico
parser = yacc.yacc()

#Funcion para concatenar los errores léxicos en una variable
def lex_error(lex, linea, columna):
    global cont_error_lexico, errores_lexicos
    cont_error_lexico = cont_error_lexico + 1
    errores_lexicos = errores_lexicos + "<tr><td align=""center""><font color=""black"">" + str(cont_error_lexico) + "<td align=""center""><font color=""black"">" + str(lex) + "</td><td align=""center""><font color=""black"">" + str(linea) + "</td><td align=""center""><font color=""black"">" + str(columna) + "</td></tr>" + '\n'


def ejecutar(entrada):
    global parser, linea, columna
    linea = 1
    columna = 1
    parse_result = parser.parse(entrada)

    return parse_result