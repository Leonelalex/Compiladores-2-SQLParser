import ply.yacc as yacc
import ply.lex as lex
import re

import instrucciones as ins 

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


#region 'Select Analisis'

def p_s_select(p):
    '''s_select : SELECT dist list_cols FROM list_from PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], None, None, None)
    lst_instrucciones.append(cosn)
    

def p_s_select2(p):
    '''s_select : SELECT dist list_cols FROM list_from list_conditions PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], None, None, p[6])
    lst_instrucciones.append(cons)
    
def p_s_select3(p):
    '''s_select : SELECT dist list_cols FROM list_from list_order PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], None, p[6], None)
    lst_instrucciones.append(cons)

def p_s_select4(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], p[6], None, None)
    lst_instrucciones.append(cons)

def p_s_select5(p):
    '''s_select : SELECT dist list_cols FROM list_from list_conditions list_order PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], None, p[7], p[6])
    lst_instrucciones(cons)

def p_s_select6(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_conditions PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], p[6], None, p[7])
    lst_instrucciones.append(cons)

def p_s_select7(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_order PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], p[6], p[7], None)
    lst_instrucciones.append(cons)

def p_s_select8(p):
    '''s_select : SELECT dist list_cols FROM list_from list_joins list_conditions list_order PTCOMA'''
    cons = ins.Select(p[2], p[3], p[5], p[6], p[8], p[7])
    lst_instrucciones.append(cons)

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
    
    


def p_list_alias(p):
    '''list_alias : list_alias COMA sel_id
                  | sel_id'''
    

def p_sel_id(p):
    ''' sel_id : ID PUNTO ID AS ID
                  | ID PUNTO ID
                  | ID AS ID
                  | ID'''

    #AST graphviz
    try:
        p[0] = p[1]

    except IndexError:
        print('')

def p_list_from(p):
    '''list_from :  list_from COMA from_id
                  | from_id'''
    

def p_from_id(p):
    '''from_id : ID AS ID
                | ID'''

    

def p_list_joins(p):
    '''list_joins : list_joins join_type JOIN ID join_conditions 
                  | list_joins JOIN ID join_conditions 
                  | join_type JOIN ID join_conditions
                  | JOIN ID join_conditions
                  | JOIN ID'''
    

def p_join_type(p):
    '''join_type : LEFT OUTER
                 | RIGHT OUTER
                 | FULL OUTER
                 | LEFT
                 | RIGHT
                 | FULL
                 | INNER'''



def p_join_conditions(p):
    '''join_conditions : ON expresion'''


def p_list_conditions(p):
    '''list_conditions : WHERE expresion'''


def p_list_order(p):
    '''list_order : ORDER BY ID ASC
                  | ORDER BY ID DESC'''


#end region 

def p_create_type(p):
    '''create_type : CREATE TYPE ID AS ENUM PARIZQ lista1 PARDER PTCOMA'''

    

def p_lista1(p):
    '''lista1 : lista1 COMA CADENA'''
    p[0] = p[3].append(p[1])

def p_lista1_2(p):
    '''lista1 : CADENA'''
    p[0] = p[1]
    

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
    

def p_time_zone(p):
    '''time_zone    : WITH TIME ZONE
                    | WITHOUT TIME ZONE'''


def p_create_db(p):
    '''create_db : CREATE DATABASE c_db db_owner db_mode PTCOMA'''
    CreateDB(p[2], p[3], p[4])


def p_create_db_2(p):
    '''create_db : CREATE OR REPLACE DATABASE c_db db_owner db_mode PTCOMA'''
    CreateDB(p[2], p[3], p[4])

def p_c_db(p):
    '''c_db : db_exist c_db1'''

    p[0] = p[2]

def p_c_dc_exist(p):
    '''db_exist : IF NOT EXISTS
                | '''

def p_c_db1(p):
    '''c_db1 : ID '''

    p[0] = p[1]


def p_db_owner(p):
    '''db_owner : OWNER igual_id 
                  |'''
    p[0] = p[2]

def p_db_mode(p):
    '''db_mode : MODE igual_int
                    |'''
    p[0] = p[2]

def p_igual_id(p):
    '''igual_id : IGUAL ID
                | ID'''


def p_igual_int(p):
    '''igual_int : IGUAL ENTERO
                | ENTERO'''


def p_show_db(p):
    '''show_db : SHOW DATABASES PTCOMA'''
    #ShowDB()

def p_alter_db(p):
    '''alter_db : ALTER DATABASE ID al_db PTCOMA'''
     

def p_al_db(p):
    '''al_db : RENAME TO ID
            | OWNER TO owner_db'''
    
    p[0] = p[3]

def p_owner_db(p):
    '''owner_db : ID
                | CURRENT_USER
                | SESSION_USER'''
    
    p[0] = p[1]

def p_drop_db(p):
    '''drop_db  : DROP DATABASE ID PTCOMA'''
    Drop(p[3])

def p_drop_db_2(p):
    '''drop_db  : DROP DATABASE IF EXISTS ID PTCOMA'''
    Drop(p[5])

def p_create_table(p): 
    '''create_table   : CREATE TABLE ID PARIZQ valores PARDER PTCOMA'''

def p_create_table_2(p):
    '''create_table   : CREATE TABLE ID PARIZQ valores PARDER INHERITS PARIZQ ID PARDER PTCOMA'''

def p_valores_2(p):
    '''valores  : colum_list
                | colum_list COMA const_keys'''
    p[0] = p[1]

def p_colum_list(p):
    '''colum_list   : ID data_type const'''
    p[0] = p[1]

def p_colum_list_2(p):
    '''colum_list   : colum_list COMA ID data_type const'''
    p[0] = p[3].append(p[1])


def p_const_keys(p):
    '''const_keys   : const_keys COMA PRIMARY KEY PARIZQ lista_id PARDER
                    | const_keys COMA FOREIGN KEY PARIZQ lista_id PARDER REFERENCES ID PARIZQ lista_id PARDER
                    | PRIMARY KEY PARIZQ lista_id PARDER
                    | FOREIGN KEY PARIZQ lista_id PARDER REFERENCES ID PARIZQ lista_id PARDER'''


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
                | REFERENCES ID PARIZQ lista_id PARDER
                |'''


def p_lista_id(p):
    '''lista_id : lista_id COMA ID'''
    p[0] = p[3].append(p[1])

def p_lista_id_2(p):
    '''lista_id : ID'''
    p[0] = p[1]

def p_drop_table(p):
    'drop_table : DROP TABLE ID PTCOMA'
    Drop(p[3])


def p_alter_table(p):
    'alter_table    : ALTER TABLE ID acciones PTCOMA'


def p_acciones(p):
    '''acciones : ADD acc
                | ADD COLUMN ID data_type
                | ALTER COLUMN ID TYPE data_type
                | ALTER COLUMN ID SET const
                | DROP CONSTRAINT ID
                | DROP COLUMN ID
                | RENAME COLUMN ID TO ID'''


def p_acc(p):
    '''acc  : const
            | const_keys'''


def p_delete(p):
    '''s_delete : DELETE FROM ID PTCOMA
                | DELETE FROM ID WHERE expresion PTCOMA '''


def p_insert(p):
    '''s_insert : INSERT INTO ID PARIZQ lista_id PARDER VALUES lista_values PTCOMA
                | INSERT INTO ID VALUES lista_values PTCOMA '''



def p_lista_values(p):
    '''lista_values : lista_values COMA PARIZQ lista_valores PARDER
                     | PARIZQ lista_valores PARDER'''


def p_lista_valores(p):
    '''lista_valores : lista_valores COMA valores
                     | valores'''
    

def p_valores(p):
    '''valores : CADENA
               | ENTERO
               | DECIMA'''


def p_s_update(p):
    '''s_update : UPDATE ID SET lista_asig PTCOMA
                | UPDATE ID SET lista_asig WHERE expresion PTCOMA'''



def p_lista_asig(p):
    '''lista_asig : lista_asig COMA ID IGUAL valores
                  | ID IGUAL valores'''



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



def p_error(p):
    print('error')


# Construyendo el analizador sintáctico
parser = yacc.yacc()

#Funcion para concatenar los errores léxicos en una variable
def lex_error(lex, linea, columna):
    print('error')


def ejecutar(entrada):
    global parser
    parse_result = parser.parse(entrada)

    return parse_result