#tokens
from dataclasses import dataclass
import sys

# punctuators
EOF           = 'EOF'
IDENTIFIER    = '<identifier>'
INTEGER_CONST = '<integer>'
LPARENTHESIS  = '<parenthesis>'
RPARENTHESIS  = '<parenthesis>'
LSQUARE       = '<parenthesis>'
RSQUARE       = '<parenthesis>'
LFLOWER       = '<parenthesis>'
RFLOWER       = '<parenthesis>' 
QUOTATIONS    = '<quotations>'
SEMICOLON     = '<end_of_stmt>'
COMMA         = '<comma>'

# operators
Assign        = '<operator>'
Plus          = '<operator>'
Minus         = '<operator>'
Multiply      = '<operator>'
Division      = '<operator>'
Reminder      = '<operator>'
Double_eq     = '<operator>'
Notequal      = '<operator>'
Greaterthan   = '<operator>'
Lessthan      = '<operator>'
Gteq          = '<operator>'
Lteq          = '<operator>'
Modulo        = '<operator>'
And           = '<operator>'
Or            = '<operator>'   
Dot           = '<dot>'

# keywords
Integer       = '#I'
Float         = '#F'
Character     = '#C'
String        = '#S'
Array         = '#A'
List          = '#L'
Tuple         = '#T'
Mutables      = '#M'
Concatinate   = 'add'
Remove        = 'cut'
Obtaining     = 'get'
Replacing     = 'edit'
Size          = 'len'
Boolean       = '#B'
Print         = 'write'
Inserting     = 'ladd'
Remlist       = 'lcut'
Slicing       = 'slice'
Arradding     = 'aadd'
If            = 'if'
Elseif        = 'elsif'
Else          = 'else'
While         = 'loopy'
For           = 'repeat'
Function      = 'define'
Return        = 'submit'
Try           = 'try'
catch         = 'catch'
Let           = 'let'

# literals
TRUE          = "true"
FALSE         = "false"

@dataclass
# class to store the token types and values
class Token:
    type: str
    value: object

    def str(self):
        return '({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def repr(self):
        return self.str()

KEYWORDS = {
    '#I': Token('<key_word>', '#I'),
    '#F': Token('<key_word>', '#F'),
    '#C': Token('<key_word>', '#C'),
    '#S': Token('<key_word>', '#S'),
    '#A': Token('<key_word>', '#A'),
    '#L': Token('<key_word>', '#L'),
    '#T': Token('<key_word>', '#T'),
    '#M': Token('<key_word>', '#M'),
    '#B': Token('<key_word>', '#B'),
    'add': Token('<key_word>', 'add'),
    'cut': Token('<key_word>', 'cut'),
    'get': Token('<key_word>', 'get'),
    'edit': Token('<key_word>', 'edit'),
    'len': Token('<key_word>', 'len'),
    'write': Token('<key_word>', 'write'),
    'ladd': Token('<key_word>', 'ladd'),
    'lcut': Token('<key_word>', 'lcut'),
    'if': Token('<key_word>', 'if'),
    'elsif': Token('<key_word>', 'elsif'),
    'else': Token('<key_word>', 'else'),
    'slice': Token('<key_word>', 'slice'),
    'aadd': Token('<key_word>', 'aadd'),
    'loopy': Token('<key_word>', 'loopy'),
    'repeat': Token('<key_word>', 'repeat'),
    'define': Token('<key_word>', 'define'), 
    'submit': Token('<key_word>', 'submit'),
    'try': Token('<key_word>', 'try'),
    'catch': Token('<key_word>', 'catch'),
    'let': Token('<key_word>', 'let'),
}

#Defining Lexer class
class Lexer(object):
    def __init__(self, text):

        self.text = text                        # Input Stream
        self.Position = 0                       # current position in the stream
        self.LinePosition = 0                   # current position in current line of program
        self.flag = 0
        self.mount = 1
        self.count = 0
        if text == "":
            print("Empty Program")
            self.Character = None

        else:
            self.Character = self.text[self.Position]      # current character in the stream
        self.curLine = self.Character                      # current line of program read till now


    def error(self):
        print("Current Character", self.Character)
        sys.exit('Invalid character')

    def Next_Char(self):                         # advances the position pointer
        self.Position += 1
        self.LinePosition += 1

        if self.Position >= len(self.text):       # end of input stream
            self.Character = None                 
        else:
            self.Character = self.text[self.Position]
            self.curLine += self.Character
            if self.Character == '\n':
                self.curLine=""
                self.LinePosition=0


    def Choose_Next(self): 
        #returns the lookahead character                                
        if self.Position + 1 >= len(self.text):
            return None        
        else:
            return self.text[self.Position + 1]

    def Space_Skip(self):
        #Skips Whitespaces  
        self.Next_Char()                     
        while  self.Character is not None and self.Character.isspace():
            self.Next_Char()

    def Comments_Skip(self):  
        #Skips comments                       
        self.count = 1
        self.Next_Char()
        self.Next_Char()
        while self.count != 0:
            self.Next_Char()
            if self.Character == '\n':
                self.count = 0
                break

    def Multi_comment_skip(self):  
        #Skips Multi line comments  
        #self.Next_Char()
        #self.Next_Char()
        self.mount = 1
        self.Next_Char()
        while self.mount < 2 :
            if self.Character == '!' :
                self.Space_Skip()
                if self.Character == '!':
                    break
            else :
                self.Next_Char()
                continue
        self.Next_Char() 


    def Integer_lexer(self):                           
        #Lexical analysis of all integers
        result = ''
        while self.Character is not None and self.Character.isdigit():
            result += self.Character
            self.Next_Char()

        if self.Character == '.':
            result += self.Character
            self.Next_Char()

            while (
                self.Character is not None and
                self.Character.isdigit()
            ):
                result += self.Character
                self.Next_Char()

            token = Token('FRACTION_CONST', float(result))
        else:
            token = Token('INTEGER_CONST', int(result))

        return token

    def Identifier_lexer(self):
        # Handles identifiers and reserved keywords
        result = ''
        while self.Character is not None and self.Character.isalnum():

            result += self.Character
            self.Next_Char()
        # Get the corresponding token from KEYWORDS dictionary, or create on identifier token 
        token = KEYWORDS.get(result, Token(IDENTIFIER, result))
        return token

    def String_lexer(self):
        # Handles strings
        result = ''
        while self.Character != '"':
            result += self.Character
            self.Next_Char()
        token = Token('STRING', result)
        return token

    def get_token(self):
        # returns the token and token type
        while self.Character is not None:

            if self.Character == '#':
                if self.Choose_Next() == 'I':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#I')
                elif self.Choose_Next() == 'F':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#F') 
                elif self.Choose_Next() == 'C':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#C') 
                elif self.Choose_Next() == 'S':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#S') 
                elif self.Choose_Next() == 'A':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#A') 
                elif self.Choose_Next() == 'L':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#L')
                elif self.Choose_Next() == 'T':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#T') 
                elif self.Choose_Next() == 'M':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#M')
                elif self.Choose_Next() == 'B':
                    self.Next_Char()
                    self.Next_Char()
                    return Token('<key_word>', '#B')               

            if self.Character.isspace():
                self.Space_Skip()
                continue

            if self.Character == '{':
                self.Next_Char()
                return Token(LFLOWER, '{')
            
            if self.Character == '}':
                self.Next_Char()
                return Token(RFLOWER, '}')                

            if self.Character.isalpha():
                return self.Identifier_lexer()

            if self.Character.isdigit():
                return self.Integer_lexer()

            if self.Character == '.':
                self.Next_Char()
                return Token(Dot, '.')
            
            if self.Character == '=':
                if self.Choose_Next() == '=':
                    self.Next_Char()
                    self.Next_Char()
                    return Token(Double_eq, '==')
                self.Next_Char()
                return Token(Assign, '=') 

            if self.Character == "|":
                if self.Choose_Next() == "|":
                    self.Next_Char()
                    self.Next_Char()
                    return Token(Or, "||")

            if self.Character == "&":
                if self.Choose_Next() == "&":
                    self.Next_Char()
                    self.Next_Char()
                    return Token(And, "&&")

            if self.Character == '>':
                if self.Choose_Next() == '=':
                    self.Next_Char()
                    self.Next_Char()
                    return Token(Gteq, '>=')
                else:
                    self.Next_Char()
                    return Token(Greaterthan, '>')

            if self.Character == '!':
                self.Space_Skip()

                if self.Character == '!':
                    self.Multi_comment_skip()
            

                elif self.Choose_Next() == '=':
                    self.Next_Char()
                    self.Next_Char()
                    return Token(Notequal, '!=')
                
                continue

            if self.Character == '<':

                if self.Choose_Next() == '=':
                    self.Next_Char()
                    self.Next_Char()
                    return Token(Lteq, '<=')
                
                else:
                    self.Next_Char()
                    return Token(Lessthan, '<')

            if self.Character == '*':
                self.Next_Char()
                return Token(Multiply, '*')

            if self.Character == "%":
                self.Next_Char()
                return Token(Reminder, '%')
 

            if self.Character == '+':
                self.Next_Char()
                return Token(Plus, '+')

            if self.Character == '-':
                self.Next_Char()
                return Token(Minus, '-')

            if self.Character == '/':

                if self.Choose_Next() == '/':
                    self.Comments_Skip()

                else :
                    self.Next_Char()
                    return Token(Division, '/')
                continue

            if self.Character == '(':
                self.Next_Char()
                return Token(LPARENTHESIS, '(')

            if self.Character == ')':
                self.Next_Char()
                return Token(RPARENTHESIS, ')')

            if self.Character == ';':
                self.Next_Char()
                return Token(SEMICOLON, ';')

            if self.Character == ',':
                self.Next_Char()
                return Token(COMMA, ',')

            if (self.Character == '"' and (self.flag == 0 or self.flag ==2)):
                self.flag += 1
                if(self.flag == 3): 
                    self.flag = 0
                    self.Next_Char()
                return Token(QUOTATIONS, '"')
            
            if self.Character == '"':                
                self.flag += 1
                self.Next_Char()
                return self.String_lexer()
 

            if self.Character == '[':
                self.Next_Char()
                return Token(LSQUARE, '[')   

            if self.Character == ']':
                self.Next_Char()
                return Token(RSQUARE, ']')
            self.error()

        return Token(EOF, None)
