from ply import lex

# List of token names
tokens = [
    'PLUS', 'MINUS', 'MULTIPLY', 'DIVISION', 'MOD', 'DOUBLE_EQ', 'AND', 'OR',
    'SEMICOLON', 'COMMA', 'LPAREN', 'RPAREN', 'GREATER_THAN', 'LESS_THAN',
    'EQUALS', 'GTEQ', 'LTEQ', 'NOT_EQUAL', 'NOT_UNARY', 'ARRAY', 'TRUE',
    'FALSE', 'WHILE', 'IF', 'ELSE', 'ELSIF', 'DEFINE', 'RETURN', 'VINT',
    'VFLOAT', 'VSTRING', 'VLIST', 'VBOOLEAN', 'VCHAR', 'VARRAY', 'INT',
    'FLOAT', 'CHAR', 'LIST', 'TUPLE', 'MUTABLE', 'TRY', 'CATCH', 'LET',
    'WRITE', 'STRING', 'LBRACE', 'RBRACE', 'HASH', 'THROW', 'string_constant',
    'IN', 'IDENTIFIER', 'BOOLEAN', 'DOT', 'LSQUARE', 'RSQUARE', 'QUOTATIONS',
    'ADD', 'EDIT', 'GET', 'LEN', 'L_ADD', 'L_CUT', 'SLICE', 'AADD', 'PLUSPLUS',
    'MINUSMINUS', 'CUT', 'FOR'
]

keywords = {
    'true': 'TRUE',
    'false': 'FALSE',
    'loopy': 'WHILE',
    'repeat': 'FOR',
    'if': 'IF',
    'in': 'IN',
    'elsif': 'ELSIF',
    'else': 'ELSE',
    'submit': 'RETURN',
    'define': 'DEFINE',
    'add': 'ADD',
    'cut': 'CUT',
    'get': 'GET',
    'edit': 'EDIT',
    'try': 'TRY',
    'catch': 'CATCH',
    'let': 'LET',
    'write': 'WRITE',
    'len': 'LEN',
    'ladd': 'L_ADD',
    'lcut': 'L_CUT',
    'slice': 'SLICE',
    'aadd': 'AADD'
}

# Regular expression rules for tokens
t_PLUS = r'\+'
t_PLUSPLUS = r'\+\+'
t_MINUS = r'-'
t_MINUSMINUS = r'--'
t_MULTIPLY = r'\*'
t_DIVISION = r'/'
t_MOD = r'%'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_QUOTATIONS = r'"'
t_COMMA = r','
t_DOT = r'\.'
t_LBRACE = r'{'
t_RBRACE = r'}'
t_LSQUARE = r'\['
t_RSQUARE = r']'
t_EQUALS = r'='
t_NOT_EQUAL = r'!='
t_DOUBLE_EQ = r'=='
t_AND = r'&&'
t_OR = r'\|\|'
t_GREATER_THAN = r'>'
t_LESS_THAN = r'<'
t_GTEQ = r'>='
t_LTEQ = r'<='
t_NOT_UNARY = r'!'

t_ignore = ' \t'
t_ignore_COMMENT = r'\#.*'


def t_VSTRING(t):
    r'"(([a-zA-Z]*)|[a-zA-Z]+([\\][a-zA-Z]+)+)"'
    return t

def t_VCHAR(t):
    r'"([a-zA-Z]+)"'
    return t


def t_IDENTIFIER(t):
  r'[a-zA-Z_][a-zA-Z0-9_]*'
  t.type = keywords.get(t.value.lower(), 'IDENTIFIER')
  return t


def t_VARRAY(t):
    r'\{.*?\}'  # Matches anything inside curly braces
    t.value = eval(t.value)  # Convert the string representation to an actual array
    return t


def t_VLIST(t):
    r'\[.*?\]'  # Matches anything inside square brackets
    t.value = eval(t.value)  # Convert the string representation to an actual list
    return t


def t_VFLOAT(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t


def t_VINT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_VBOOLEAN(t):
    r'True|False'
    t.value = (t.value == 'True')
    return t

def t_INT(t):
    r'\#I'
    return t


def t_FLOAT(t):
    r'\#F'
    return t


def t_ARRAY(t):
    r'\#A'
    return t


def t_STRING(t):
    r'\#S'
    return t


def t_LIST(t):
    r'\#L'
    return t


def t_TUPLE(t):
    r'\#P'
    return t


def t_BOOLEAN(t):
    r'\#B'
    return t


def t_MUTABLE(t):
    r'\#M'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

# Parsing rules
precedence = (
       
        ('left', 'COMMA'),
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'EQUALS', 'NOT_EQUAL'),
        ('left', 'GTEQ', 'GREATER_THAN'),
        ('left', 'LTEQ', 'LESS_THAN'),
        ('left', 'PLUS', 'MINUS', 'PLUSPLUS', 'MINUSMINUS'),
        ('left', 'MULTIPLY', 'DIVISION', 'MOD'),
        ('left', 'LBRACE', 'RBRACE'),
        ('left', 'LSQUARE', 'RSQUARE'), 
        ('left', 'LPAREN', 'RPAREN'), 
    )

def p_program(p):
  '''program : statements'''
  pass


def p_statements(p):
  '''statements : statement statements
                | empty'''
  pass


def p_statement(p):
  '''statement : declaration_statement SEMICOLON 
               | assignment_statement SEMICOLON 
               | if_statement 
               | while_loop
               | for_loop
               | print_statement SEMICOLON        
               | function_definition 
               | function_call SEMICOLON 
               | operational_statements SEMICOLON 
               | mutable_variable SEMICOLON 
               | exception_handling SEMICOLON 
               | let_expression
               | expression SEMICOLON '''
  pass


def p_declaration_statement(p):
  '''declaration_statement : D assignment_statement'''


def p_operational_statements(p):
  '''operational_statements : IDENTIFIER DOT ADD LPAREN STRING RPAREN 
                            | IDENTIFIER DOT CUT LPAREN INT COMMA INT RPAREN 
                            | IDENTIFIER DOT EDIT LPAREN INT COMMA INT COMMA QUOTATIONS STRING QUOTATIONS RPAREN 
                            | IDENTIFIER DOT GET LPAREN INT COMMA INT RPAREN 
                            | IDENTIFIER DOT LEN LPAREN RPAREN 
                            | IDENTIFIER DOT L_ADD LPAREN D RPAREN  
                            | IDENTIFIER DOT L_CUT LPAREN INT COMMA INT RPAREN
                            | IDENTIFIER LSQUARE INT RSQUARE 
                            | IDENTIFIER DOT SLICE LPAREN INT COMMA INT RPAREN 
                            | IDENTIFIER DOT AADD LPAREN INT RPAREN'''


def p_D(p):
  '''D : INT
       | FLOAT
       | ARRAY
       | STRING
       | LIST
       | TUPLE
       | BOOLEAN
       | MUTABLE
       | CHAR'''
  pass

def p_iee(p):
  '''iee : IDENTIFIER EQUALS expression'''
  pass

def p_assignment_statement(p):
  '''assignment_statement : iee
                        | IDENTIFIER EQUALS function_call'''
  pass


def p_constant(p):
  '''constant : VINT
              | VCHAR
              | VFLOAT
              | VSTRING
              | VARRAY
              | VBOOLEAN
              | VLIST
              | boolean_constant'''
  pass


def p_MA(p):
  '''MA : LPAREN condition RPAREN'''
  pass


def p_if_statement(p):
  '''if_statement : IF MA LBRACE statements RBRACE elsif_statement else_statement'''


def p_elsif_statement(p):
  '''elsif_statement : ELSIF MA LBRACE statements RBRACE elsif_statement
                     | empty'''


def p_else_statement(p):
  '''else_statement : ELSE LBRACE statements RBRACE
                    | empty'''


def p_while_loop(p):
  '''while_loop : WHILE MA LBRACE statements RBRACE'''
  pass

def p_for_loop(p):
  '''for_loop : FOR MB LBRACE statements RBRACE'''
  pass

def p_MB(p):
  '''MB : LPAREN assignment_statement SEMICOLON unary_operations IDENTIFIER SEMICOLON condition RPAREN'''
  pass

def p_print_statement(p):
  '''print_statement : WRITE LPAREN Pexpression RPAREN '''
  pass

def p_Pexpression(p):
  '''Pexpression : constant
                  | IDENTIFIER
                  | QUOTATIONS identifiers QUOTATIONS
                  | Pexpression COMMA Pexpression'''
  
  pass

def p_identifiers(p):
  '''identifiers : IDENTIFIER identifiers
                  | IDENTIFIER'''
  
  pass

def p_function_definition(p):
  '''function_definition : DEFINE IDENTIFIER LPAREN parameter_list RPAREN LBRACE statements RETURN expression SEMICOLON RBRACE'''
  pass


def p_function_call(p):
  '''function_call : IDENTIFIER LPAREN expression RPAREN '''
  pass


def p_parameter_list(p):
  '''parameter_list : D IDENTIFIER cp
                    | empty'''
  pass


def p_cp(p):
  '''cp : COMMA parameter_list
        | empty'''
  pass


def p_mutable_variable(p):
  '''mutable_variable : HASH iee'''
  pass


def p_exception_handling(p):
  '''exception_handling : TRY LBRACE statements THROW HASH IDENTIFIER LPAREN string_constant RPAREN SEMICOLON RBRACE catch_statement'''
  pass


def p_catch_statement(p):
  '''catch_statement : CATCH LPAREN HASH IDENTIFIER IDENTIFIER RPAREN LBRACE statements RBRACE
                     | empty'''
  pass


def p_let_expression(p):
  '''let_expression : LET iee IN LBRACE statements RETURN expression SEMICOLON RBRACE'''
  pass


def p_condition(p):
  '''condition : expression comparison_operator expression'''
  pass


def p_comparison_operator(p):
  '''comparison_operator : DOUBLE_EQ
                          | NOT_EQUAL
                          | GREATER_THAN
                          | LTEQ
                          | GTEQ
                          | LESS_THAN
                          | AND
                          | OR'''
  pass


def p_expression(p):
  '''expression : expression binary_operations T 
                  | T'''

  pass

def p_T(p):
  '''T : factor
        | unary_operations T'''

  pass

def p_factor(p):
  '''factor : constant
        | IDENTIFIER
        | LPAREN expression RPAREN'''

  pass

def p_binary_operations(p):
  '''binary_operations : PLUS
                         | MINUS
                         | MULTIPLY
                         | DIVISION
                         | MOD'''
  pass


def p_unary_operations(p):
  '''unary_operations : PLUSPLUS
                       | MINUSMINUS
                       | NOT_UNARY
                       | MINUS '''
  pass


def p_boolean_constant(p):
  '''boolean_constant : TRUE
                      | FALSE'''
  pass


def p_empty(p):
  '''empty : '''
  pass


def p_error(p):
  if p is not None:
      print(
          f"Syntax error at line {p.lineno}, position {p.lexpos}: Unexpected token '{p.value}'"
      )
  else:
      print("Syntax error: Unexpected end of input")
  pass


# Build the lexer and parser

import ply.yacc as yacc

parser = yacc.yacc()

try:
  text = open("D:\Downloads\Test_7.ht", "r").read()

  p = parser.parse(text)
except EOFError:
  print("File could not be opened!")
