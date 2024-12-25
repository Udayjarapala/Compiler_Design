from Lexer import *

def main():

    text = open("Test_Cases/test1.ht","r").read()    
    lexer = Lexer(text)
    Token = lexer.get_token()
    while Token.type != EOF:
        print(Token.str())
        Token = lexer.get_token()
    print(Token.value, Token.type)         # checks EOF

main()
