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

from ASTT import (
    Number,String,Id,Program,Statements,Declaration,Assignment,Type,MA,IfStatement,
    ElsifStatement,ElseStatement,WhileStatement,ForStatement,MB,PrintStatement,LAT,
    Pexpression,Identifiers,FunctionDefinition,FunctionCall,ParameterList,Cp,MutableVariable,
    ExceptionHandling,CatchStatement,LetExpression,Condition,ComparisonOperator,Expression,
    T,Factor,BinaryOperations,UnaryOperations,BooleanConstant,OperationalStatements_ADD,OperationalStatements_CUT,
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
    p[0] = BinaryOperations(p[1])

def p_unary_operations(p):
    '''unary_operations : PLUSPLUS
                         | MINUSMINUS
                         | NOT_UNARY
                         | MINUS '''
    p[0] = UnaryOperations(p[1])

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

class Scope:
    def  __init__(self, parent=None):
      self.dict = dict()
      self.parent = parent

    def put(self, name, symbol):
      self.dict[name] = symbol

    def get(self, name):
      if name in self.dict:
          return self.dict[name]
      if self.parent is None:
          return None
      return self.parent.get(name)

    def push_scope(self):
      return Scope(self)

    def pop_scope(self):
      return self.parent
  
class Id:
    def __init__(self, id):
      self.id = id

class Symbol:
    def  __init__(self, name, type):
      self.name = name
      self.type = type

class VariableSymbol(Symbol):
    def  __init__(self, name, type):
      super(). __init__(name, type)

class FunctionSymbol(Symbol):
    def  __init__(self, name, type, parameters):
      super(). __init__(name, type)
      self.parameters = parameters

class SymbolTable:
    def  __init__(self):
      self.symbols = {}
      self.current_scope = Scope()

    def add_symbol(self, name, symbol):
      if name in self.symbols:
          raise Exception(f"Duplicate symbol found: {name}")
      else:
          self.symbols[name] = symbol

    def lookup(self, name):
      return self.symbols.get(name)


class SemanticAnalyzer:
    def  __init__(self):
      self.symbol_table = SymbolTable()
      self.current_scope = Scope()

    def visit_Number(self, node):
        return VariableSymbol(node.value, "#I")

    def visit_String(self, node):
        return VariableSymbol(node, "#S")

    def visit_Float(self,node):
        return VariableSymbol(node.value, "#F")

    def visit_bool(self,node):
        return VariableSymbol(None, "#B")

    def analyze(self, ast):
        self.visit(ast)

    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'  # Use __name instead of name
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f"No visit_{type(node).__name__} method defined")

    def visit_Program(self, node):
        self.visit(node.statement_list)

    def visit_Statements(self, node):
        for statement in node.statements:
            self.visit(statement)
    
    def visit_Declaration(self, node):
        type = node.type
        identifier = node.assignment.identifier
        self.symbol_table.add_symbol(identifier, VariableSymbol(identifier, type))
        valuev = node.assignment.value.value
        valuev_type = None  # Initialize valuev_type to None
    
        # Check for function call
        if isinstance(node.assignment.value, FunctionCall):
            lier = self.symbol_table.lookup(node.assignment.value.identifier)
            if lier.type != type:
                raise Exception("Function type does not match with declared type.")

        # Type checking for basic types
        if isinstance(valuev, int):
            valuev_type = '#I'
        elif isinstance(valuev, float):
            valuev_type = '#F'
        elif isinstance(valuev, str):
            valuev_type = '#S'
        elif isinstance(valuev, bool):
            valuev_type = '#B'
        elif isinstance(valuev, list):
            valuev_type = '#L'

        # Check if types match
        if type != valuev_type:
            raise Exception(f"Type not matching")
            

    def visit_Id(self, node):
        symbol = self.symbol_table.lookup(node.id)  # Lookup the symbol directly by identifier
        if symbol is None:
            raise Exception(f"Variable '{node.id}' not found in symbol table")
        return symbol

    
    def visit_Assignment(self, node):
        identifier = node.identifier  # Extract the identifier from the node
        symbol = self.symbol_table.lookup(identifier)

        if isinstance(node.value,Expression):
            self.visit(node.value)
        
        elif isinstance(node.value,FunctionCall):
            self.visit(node.value)
        else:
            valuev = node.value.value
            valuev_type = None
            
            if isinstance(valuev, int):
                valuev_type = '#I'
            elif isinstance(valuev, float):
                valuev_type = '#F'
            elif isinstance(valuev, str):
                valuev_type = '#S'
            elif isinstance(valuev, bool):
                valuev_type = '#B'
            
            

        
            # Check if the symbol exists in the symbol table
            if symbol is None:
                raise Exception(f"Variable '{identifier}' not found in symbol table")
            
            # Check if symbol.type matches the valuev_type
            if valuev_type != symbol.type:
                raise Exception(f"type of {identifier} {symbol.type} {valuev_type} not matching")

            # Add the symbol to the current scope
            self.current_scope.put(identifier, symbol)
            self.visit(node.value)  # Visit the assignment value


       

    def visit_FunctionDefinition(self, node):
        name = node.identifier
        type_ = node.type
        self.current_scope = self.current_scope.push_scope()
        parameters = None

        if node.parameter_list is not None:
            parameters = node.parameter_list.parameters
            for param_type, param_name in parameters:
                self.symbol_table.add_symbol(param_name, VariableSymbol(param_name, param_type))
        
        self.symbol_table.add_symbol(name, FunctionSymbol(name, type_, parameters))
        # Visit the function body
        self.visit(node.statements)
        self.visit(node.return_expression)

        if isinstance(node.return_expression, FunctionCall):
            kier = self.symbol_table.lookup(node.return_expression.identifier)
            if kier.type != type_:
                raise Exception("Return type and the function type do not match.")
            
        elif any(op in node.return_expression.value for op in ['+', '-', '*', '/', '%']):
            if type_ != 'int':
                raise Exception("Return type and the function type do not match.")
            
        else:
            hier = self.symbol_table.lookup(node.return_expression.value)
            if hier.type != type_:
                raise Exception("Return type and the function type do not match.")

        if parameters is not None:
            for i in parameters:
                self.symbol_table.symbols.pop(i[1], None)

        # Pop the function scope
        self.current_scope = self.current_scope.pop_scope()
        

    def visit_FunctionCall(self, node):

        name = node.identifier
        symbol = self.symbol_table.lookup(name)

        if symbol is None:
            raise Exception(f"Function '{name}' not declared")

        parameters = symbol.parameters
        
    
        if parameters is None:
            raise Exception(f"Function doesn't have any parameters")            

        k = node.expression

        def typeflasher(valuev):
            if isinstance(valuev, int):
                return '#I'
            elif isinstance(valuev, float):
                return'#F'
            elif isinstance(valuev, str):
                return '#S'
            elif isinstance(valuev, bool):
                return '#B'

        # Check if the number of arguments matches the number of parameters expected by the function
        if len(parameters) != len(k):
            raise Exception(f"Function '{name}' expects {len(parameters)} arguments, but {len(k)} provided")
        
        else:
            j = 0
            # Iterate through parameters and arguments to check type matching
            for i in parameters:
                if i[0] != typeflasher(k[j]):
                    raise Exception(f"Parameter type does not match for function '{name}'")
                else:
                    j = j + 1

    def visit_Expression(self, node): #have to write for Pexpression

        if isinstance(node.expression, tuple):
            operator = node.expression[1].operator
            right = node.expression[2].value
            try:
                left = node.expression[0].value
                symbol1 = self.symbol_table.lookup(node.expression[0].value)
                symbol2 = self.symbol_table.lookup(right)
                
                if symbol1 is None and not isinstance(left, (int, float)):
                    raise Exception(f"invalid variable detected")
                
                if symbol2 is None and not isinstance(right, (int, float)):
                    raise Exception(f"invalid variable detected{right}")
                
                valid_operations = {'+', '-', '*', '/', '%'}
                if operator in valid_operations:

                    if symbol1 is not None:
                        if (symbol1.type != '#I' and symbol1.type != '#F'):
                            raise Exception(f"Operands of arithmetic operator '{operator}' must be of type 'int'")
                        
                    if symbol2 is not None:
                        if (symbol2.type != '#I' and symbol2.type != '#F'):
                            raise Exception(f"Operands of arithmetic operator '{operator}' must be of type 'int'")               
                    
                    # Assign the result type of the arithmetic expression as 'int'
                    return VariableSymbol(None, 'int')

            except AttributeError:
                self.visit(node.expression[0])        

        
    def visit_tuple(self, node):
        # Handle logic for visiting tuple nodes
        # For example, you can recursively visit each element of the tuple
        for element in node:
            self.visit(element)

    def visit_NoneType(self, node):
        pass

    def visit_Factor(self, node):
        if isinstance(node, tuple):
            # Handle tuple nodes separately
            factor_type = node[0]
            if factor_type == 'identifier':
                symbol = self.symbol_table.lookup(node[1])
                if symbol is None:
                    raise Exception(f"Variable '{node[1]}' not declared")
                return symbol
            elif factor_type == 'integer':
                return VariableSymbol(None, '#I')
            elif factor_type == 'string':
                return VariableSymbol(None, '#S')
            elif factor_type == 'true' or factor_type == 'false':
                return VariableSymbol(None, '#B')
        else:
            # Handle single nodes
            if isinstance(node, Id):
                return self.visit_Id(node)
            elif isinstance(node, int):
                return self.visit_Integer(node)
            elif isinstance(node, str):
                return self.visit_str(node)
            elif isinstance(node, bool):
                return self.visit_boolean(node)

    def visit_string(self, node):
        # Handle logic for visiting string nodes
        # For example, you can validate the string or perform any necessary checks
        # In this example, we'll simply return 'str' as the type of the string node
        return VariableSymbol(None, '#S')

    def visit_Condition(self, node):

        if node.left_expression is None or node.right_expression is None:
            raise Exception("Invalid condition: Both left and right expressions must be provided")

        left_result = node.left_expression.value
        right_result = node.right_expression.value

        def is_constant(expression):
            return isinstance(expression, (int, float)) 
        
        if (self.symbol_table.lookup(left_result) is None and not is_constant(left_result)):
            raise Exception(f"Invalid variable in condition '{left_result}'")
        elif (not self.symbol_table.lookup(left_result) is None and not is_constant(left_result)):
            left_type = self.symbol_table.lookup(left_result).type
        else:
            if isinstance(left_result, int):
                left_type = '#I'
            elif isinstance(left_result, float):
                left_type = '#F'
            elif isinstance(left_result, str):
                left_type = '#S'
            elif isinstance(left_result, bool):
                left_type = '#B'
            

        if (self.symbol_table.lookup(right_result) is None and not is_constant(right_result)):
            raise Exception("Invalid variable in condition")
        elif (not self.symbol_table.lookup(right_result) is None and not is_constant(right_result)):
            right_type = self.symbol_table.lookup(right_result).type
        else:
            if isinstance(right_result, int):
                right_type = '#I'
            elif isinstance(right_result, float):
                right_type = '#F'
            elif isinstance(right_result, str):
                right_type = '#S'
            elif isinstance(right_result, bool):
                right_type = '#B'

        if left_type != right_type:
            raise Exception("Type mismatch in condition: Left and right operands must have the same type")

    def visit_MB(self, node):
        self.visit(node.assignment_statement)
        self.visit(node.condition)
        
        valid_unary_ops = {'++', '--', '!', '-'}
        if str(node.unary_ops.operator) not in valid_unary_ops:
            raise Exception("Invalid unary operator")
        #here have to check whether the identifier is already in symbol table or not if not error should be reported and also have to check if it is a integer or not if not error
        symbol = self.symbol_table.lookup(node.identifier)
        if symbol is None:
            raise Exception(f"Variable '{node.identifier}' not found in symbol table")
    
        if symbol.type != '#I' and symbol.type != '#F':
            raise Exception(f"Invalid operation on the variable{symbol.type}")


    def visit_IfStatement(self, node):
        self.visit(node.condition)
        self.visit(node.statements)
        if node.elsif is not None:
            for condition, statements in node.elsif:
                self.visit(condition)
                self.visit(statements)
        if node.elses is not Empty:
            self.visit(node.elses)

    def visit_WhileStatement(self, node):
        self.visit(node.condition)
        self.visit(node.statement_list)

    def visit_ForStatement(self, node):
        self.visit(node.for_condition) 
        self.visit(node.statement_list)    

    def visit_ExceptionHandling(self, node):
        self.visit(node.try_block)
        if(isinstance(node.CatchS,CatchStatement)):
            self.visit(node.CatchS.Statements)

    def visit_PrintStatement(self, node):
        for value in node.printing_data:
            # Check if the value is enclosed in double quotes
            if not (value.startswith('"') and value.endswith('"')):
                # Proceed further only if the value is not enclosed in double quotes
                look_value = self.symbol_table.lookup(value)
                if look_value is None:
                    raise Exception(f"Invalid variable: {value}")

        
    def visit_MA(self, node):
        self.visit(node.condition)

parser = yacc.yacc()

try:
    text = open("D:\Downloads\Test_4.ht", "r").read()
    ast = parser.parse(text)
    #pprint(ast)
    semantic_analyzer = SemanticAnalyzer()
    semantic_analyzer.analyze(ast)
    print("Semantic Analysis Successful!!")
except EOFError:
    print("File could not be opened!")
