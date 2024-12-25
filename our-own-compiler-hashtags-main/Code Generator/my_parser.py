import ply.yacc as yacc
from pprint import pprint

from ply import lex

# List of token names
tokens = [
    'PLUS', 'MINUS', 'MULTIPLY', 'DIVISION', 'MOD', 'DOUBLE_EQ', 'AND', 'OR',
    'SEMICOLON', 'COMMA', 'LPAREN', 'RPAREN', 'GREATER_THAN', 'LESS_THAN',
    'EQUALS', 'GTEQ', 'LTEQ', 'NOT_EQUAL', 'NOT_UNARY', 'ARRAY', 'TRUE',
    'FALSE', 'WHILE', 'IF', 'ELSE', 'ELSIF', 'DEFINE', 'RETURN', 'VINT',
    'VFLOAT', 'VSTRING', 'VLIST', 'VBOOLEAN', 'VCHAR', 'VARRAY', 'INT',
    'FLOAT', 'CHAR', 'LIST', 'TUPLE', 'MUTABLE', 'TRY', 'CATCH', 'LET',
    'WRITE', 'STRING', 'LBRACE', 'RBRACE', 'HASH', 'THROW',
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
    'addy': 'ADD',
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
    'throw': 'THROW',
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
t_RSQUARE = r'\]'
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
t_HASH = r'\#'

t_ignore = ' \t'
t_ignore_COMMENT = r'//.*'


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
    r'\#T'
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

from myAST import (
    Number,String,Id,Program,Statements,Declaration,Assignment,Type,MA,IfStatement,
    ElsifStatement,ElseStatement,WhileStatement,ForStatement,MB,PrintStatement,LAT,
    Pexpression,Identifiers,FunctionDefinition,FunctionCall,ParameterList,Cp,MutableVariable,
    ExceptionHandling,CatchStatement,LetExpression,Condition,ComparisonOperator,Expression,
    T,Factor,BinaryOperator,UnaryOperator,BooleanConstant,OperationalStatements_ADD,OperationalStatements_CUT,
    OperationalStatements_EDIT,OperationalStatements_GET,OperationalStatements_LEN,OperationalStatements_L_ADD,
    OperationalStatements_L_CUT,OperationalStatements_SLICE,OperationalStatements_AADD,Empty
) 

# Tokens definition and other lexer rules...

# Grammar rules with actions constructing AST nodes
def p_program(p):
    '''program : statements'''
    p[0] = Program(p[1])

def p_statements(p):
    '''statements : statements statement
                  | empty'''
    
    if len(p) == 3:
      if isinstance(p[1], Statements):
          p[0] = Statements(p[1].statements + [p[2]])
      else:
          p[0] = Statements([p[2]])
    else:
        p[0] = Statements([])

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
    p[0] = p[1]

def p_declaration_statement(p):
    '''declaration_statement : D assignment_statement'''
    p[0] = Declaration(p[1], p[2])

def p_operational_statements(p):
    '''operational_statements : IDENTIFIER DOT ADD LPAREN VSTRING RPAREN 
                            | IDENTIFIER DOT CUT LPAREN VINT COMMA VINT RPAREN 
                            | IDENTIFIER DOT EDIT LPAREN VINT COMMA VINT COMMA QUOTATIONS STRING QUOTATIONS RPAREN 
                            | IDENTIFIER DOT GET LPAREN VINT COMMA INT RPAREN 
                            | IDENTIFIER DOT LEN LPAREN RPAREN 
                            | IDENTIFIER DOT L_ADD LPAREN constant RPAREN  
                            | IDENTIFIER DOT L_CUT LPAREN VINT COMMA VINT RPAREN 
                            | IDENTIFIER DOT SLICE LPAREN VINT COMMA VINT RPAREN
                            | IDENTIFIER LSQUARE VINT RSQUARE 
                            | IDENTIFIER DOT AADD LPAREN VINT RPAREN'''    

    if len(p) == 7:
        p[0] = OperationalStatements_L_ADD(p[1],p[3], p[5])

    elif len(p) == 9:
        p[0] = OperationalStatements_CUT(p[1],p[3],p[5],p[7])

    elif len(p) == 6:
        p[0] = OperationalStatements_LEN(p[1],p[3])

    elif len(p) == 10:
        p[0] = OperationalStatements_L_CUT(p[1],p[3], p[5],p[7])
    
    elif len(p) == 13:
        p[0] = OperationalStatements_EDIT(p[1],p[3], p[5],p[7],p[10])


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
    p[0] = p[1]

def p_LAT(p):
    '''LAT : IDENTIFIER LPAREN LPAREN VINT RPAREN RPAREN'''

    p[0] = LAT(p[1],p[4])

def p_assignment_statement(p):
    '''assignment_statement : IDENTIFIER EQUALS expression
                            | IDENTIFIER EQUALS function_call
                            | IDENTIFIER EQUALS LAT'''

    p[0] = Assignment(p[1], p[2], p[3])

def p_constant(p):
    '''constant : VINT
                | VCHAR
                | VFLOAT
                | VSTRING
                | VARRAY
                | VBOOLEAN
                | VLIST
                | boolean_constant'''
        
    p[0] = p[1]

def p_MA(p):
    '''MA : LPAREN condition RPAREN'''
    p[0] = MA(p[2])

def p_if_statement(p):
    '''if_statement : IF MA LBRACE statements RBRACE elsif_statement else_statement'''
    p[0] = IfStatement(condition=p[2], statements=p[4], elsif=p[6], elses=p[7])

def p_elsif_statement(p):
    '''elsif_statement : ELSIF MA LBRACE statements RBRACE elsif_statement
                       | '''
    if len(p) == 7:
        p[0] =  [(p[2], p[4])] + p[6]
    else:
        p[0] = None

def p_else_statement(p):
    '''else_statement : ELSE LBRACE statements RBRACE
                      | '''
    if len(p) == 5:
        p[0] = p[3]
    else:
        p[0] = None

def p_while_loop(p):
    '''while_loop : WHILE MA LBRACE statements RBRACE'''
    p[0] = WhileStatement(p[2], p[4])

def p_for_loop(p):
    '''for_loop : FOR MB LBRACE statements RBRACE'''
    p[0] = ForStatement(p[2], p[4])

def p_MB(p):
    '''MB : LPAREN assignment_statement SEMICOLON condition SEMICOLON unary_operations IDENTIFIER RPAREN'''
    p[0] = MB(p[2], p[4], p[6], p[7])

def p_print_statement(p):
    '''print_statement : WRITE LPAREN Pexpression RPAREN '''
    p[0] = PrintStatement(p[3])

def p_Pexpression(p):
    '''Pexpression : constant
                   | IDENTIFIER
                   | QUOTATIONS identifiers QUOTATIONS
                   | Pexpression COMMA Pexpression'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[1] == "":
        p[0] = p[2]
    else:
        p[0] = [p[1],p[3]]

def p_identifiers(p):
    '''identifiers : IDENTIFIER identifiers
                   | IDENTIFIER'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Identifiers(p[1], p[2])

def p_function_definition(p):
    '''function_definition : DEFINE D IDENTIFIER LPAREN parameter_list RPAREN LBRACE statements RETURN expression SEMICOLON RBRACE'''
    p[0] = FunctionDefinition(p[2], p[3], p[5], p[8], p[10])

def p_function_call(p):
    '''function_call : IDENTIFIER LPAREN Pexpression RPAREN '''
    p[0] = FunctionCall(p[1], p[3])

def p_parameter_list(p):
    '''parameter_list : D IDENTIFIER cp
                      | empty'''
    if len(p) == 4:
        p[0] = ParameterList([(p[1], p[2])] + p[3])
    else:
        p[0] = Empty()

def p_cp(p):
    '''cp : COMMA D IDENTIFIER cp
          | empty'''
    if len(p) == 5:
        p[0] = [(p[2], p[3])] + p[4]
    else:
        p[0] = []


def p_mutable_variable(p):
    '''mutable_variable : HASH IDENTIFIER EQUALS expression'''
    p[0] = MutableVariable(p[2])

def p_exception_handling(p):
    '''exception_handling : TRY LBRACE statements THROW LPAREN IDENTIFIER COMMA IDENTIFIER RPAREN SEMICOLON RBRACE catch_statement'''
    p[0] = ExceptionHandling(p[3],p[6], p[8],p[12])

def p_catch_statement(p):
    '''catch_statement : CATCH LPAREN IDENTIFIER COMMA IDENTIFIER RPAREN LBRACE statements RBRACE
                       | empty'''
    if len(p) == 10:
        p[0] = CatchStatement(p[3], p[5], p[8])
    else:
        p[0] = Empty()

def p_let_expression(p):
    '''let_expression : LET IDENTIFIER EQUALS expression IN LBRACE statements RETURN expression SEMICOLON RBRACE'''
    p[0] = LetExpression(p[2], p[3], p[4], p[7], p[9])

def p_condition(p):
    '''condition : expression comparison_operator expression'''
    p[0] = Condition(p[1], p[2], p[3])

def p_comparison_operator(p):
    '''comparison_operator : DOUBLE_EQ
                            | NOT_EQUAL
                            | GREATER_THAN
                            | LTEQ
                            | GTEQ
                            | LESS_THAN
                            | AND
                            | OR'''
    p[0] = ComparisonOperator(p[1])

def p_expression(p):
    '''expression : expression binary_operations T 
                  | T'''
    if len(p) == 4:
        p[0] = Expression((p[1], p[2], p[3]))
    else:
        p[0] = p[1]

def p_T(p):
    '''T : factor
         | unary_operations T'''
    if len(p) == 3:
        p[0] = T(p[2], p[1])
    else:
        p[0] = p[1]

def p_factor(p):
    '''factor : constant
              | IDENTIFIER
              | LPAREN expression RPAREN'''
    if len(p) == 2:
        p[0] = Factor(p[1])
    else:
        p[0] = Factor(p[2])

def p_binary_operations(p):
    '''binary_operations : PLUS
                          | MINUS
                          | MULTIPLY
                          | DIVISION
                          | MOD'''
    p[0] = BinaryOperator(p[1])

def p_unary_operations(p):
    '''unary_operations : PLUSPLUS
                         | MINUSMINUS
                         | NOT_UNARY
                         | MINUS '''
    p[0] = UnaryOperator(p[1])

def p_boolean_constant(p):
    '''boolean_constant : TRUE
                        | FALSE'''
    p[0] = BooleanConstant(p[1])

def p_empty(p):
    '''empty : '''
    p[0] = Empty()

def find_column(input, p):
    line_start = input.rfind('\n', 0, p.lexpos) + 1
    return (p.lexpos - line_start) + 1

def p_error(p):
  if p is not None:
      column = find_column(text, p)        
      print("Found unexpected character '%s' at line '%s' and column '%s'" % (p.value, p.lineno, column))
  else:
      print("Unexpected end of input!Empty file or syntax error at EOF!")
parser = yacc.yacc()


try:
    text = open("D:\Downloads\Test_4.ht", "r").read()
    ast = parser.parse(text)
    pprint(ast)
except EOFError:
    print("File could not be opened!")
    
