from myAST import Node, Id, Number, String, BinaryOperator


class NodeAnalyzer:
    
    def __init__(self):
        self.wat_code = []
        self.indent = 0

    def visit_BinaryOperator(self, node):
        ops = {"+": "i32.add", "-": "i32.sub", "*": "i32.mul", "/": "i32.div_s", "%": "i32.rem_s"}
        if node in ops:
            self.wat_code.append(f"{self.indent_str()}({ops[node]}")
        
    def visit_Number(self, node):
        self.wat_code.append(f'{self.indent_str()}i32.const {node.value}')

    def visit_String(self, node):
        self.wat_code.append(f'{self.indent_str()}(data (i32.const {len(node.value)}) "{node.value}")')

    def visit_Id(self, node):
        self.wat_code.append(f'{self.indent_str()}(local.get ${node.id})')

    def visit_bool(self,node):
        pass

    def analyze(self, ast):
        
        self.wat_code.append("(module")
        self.indent += 1
        self.wat_code.append(f"{self.indent_str()}(memory $mem 1)")
        self.wat_code.append(f"{self.indent_str()}(export \"memory\" (memory $mem))")
        
        self.visit(ast)

        self.indent -= 1
        self.wat_code.append(")")


    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
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
        self.wat_code.append(f'{self.indent_str()};; Declaration')
        self.indent += 1
        self.visit(node.assignment.identifier)
        #self.visit(node.assignment.value.value)
        self.wat_code.append(f"(local ${node.assignment.identifier} {node.type})")
        if node.assignment.assignment_operator == '=':
            #self.visit(node.assignment.value.value)
            self.wat_code.append(f"(set_local ${node.assignment.identifier})")
        self.indent -= 1

    def visit_Assignment(self, node):
        self.wat_code.append(f'{self.indent_str()};; Assignment')
        self.indent += 1
        self.visit(node.identifier)
        self.visit(node.value)
        self.indent -= 1  
    
    def visit_MA(self, node):
        self.wat_code.append(f'{self.indent_str()};; MA')
        self.indent += 1
        self.visit(node.condition)
        self.indent -= 1  
    
    def visit_MB(self, node):
        self.wat_code.append(f'{self.indent_str()};; MB')
        self.indent += 1
        self.visit(node.assignment_statement)
        self.visit(node.condition)
        self.indent -= 1 

    def visit_FunctionDefinition(self, node):
        type_ = node.type
        self.wat_code.append(f'{self.indent_str()};; Function Definition')
        self.indent += 1
        params = ""
        for param_type, param_id in node.parameter_list.parameters:
            params += f"(param ${param_id} {self.get_wat_type(param_type)}) "
        self.wat_code.append(f'(func ${node.identifier} {params}(result {self.get_wat_type(type_)})')
        self.visit(node.statements)
        self.visit(node.return_expression)
        self.wat_code.append(f'{self.indent_str()}return')  # Add return instruction
        self.wat_code.append(f')')
        self.indent -= 1
        self.wat_code.append(f"{self.indent_str()}(export \"{node.identifier}\" (func ${node.identifier}))")

    def get_wat_type(self, param_type):
        if param_type == "#I":
            return "i32"
        elif param_type == "#F":
            return "f32"
        else:
            raise ValueError(f"Unsupported parameter type: {param_type}")

    def visit_FunctionCall(self, node):
        self.wat_code.append(f'{self.indent_str()};; Function Call')
        self.indent += 1
        self.visit(node.identifier)
        if node.expression:
            self.visit(node.expression)
        self.indent -= 1
            
    def visit_Expression(self, node):
        if isinstance(node.expression, tuple):
            operator = node.expression[1].operator
            self.visit_BinaryOperator(operator)
            self.indent += 1
            #left = self.visit(node.expression[0].value)
            self.wat_code.append(f'{self.indent_str()}(local.get ${node.expression[0].value})')
            #right = self.visit(node.expression[2].value)
            self.wat_code.append(f'{self.indent_str()}(local.get ${node.expression[2].value})')
            self.indent -= 1         
            self.wat_code.append(f"{self.indent_str()})")

        else:
            if isinstance(node.expression, Id):
                return self.visit_Id(node.expression)
            elif isinstance(node.expression, Number):
                return self.visit_Number(node.expression)
            elif isinstance(node.expression, String):
                return self.visit_String(node.expression)
            elif isinstance(node.expression, BinaryOperator):
                return self.visit_BinaryOperator(node.expression)
            else:
                return self.visit(node.expression)
         
    def visit_tuple(self, node):
        for element in node:
            self.visit(element)
    def visit_PrintStatement(self, node):
        pass
    def visit_NoneType(self, node):
        pass

    def visit_Factor(self, node):
        pass
          
    def visit_str(self, node):
        pass
  
    def visit_Condition(self, node):
        self.visit(node.left_expression)
        self.visit(node.right_expression)
        self.wat_code.append(f'{self.indent_str()}i32.{node.comparison_operator}')

    def visit_IfStatement(self, node):
        self.wat_code.append(f'{self.indent_str()};; If Statement')
        self.indent += 1
        self.visit(node.condition)
        self.visit(node.statements)

        if node.elsif is not None:
            for condition, statements in node.elsif:
                self.visit(condition)
                self.visit(statements)
        if node.elses:
            self.visit(node.elses)
        self.indent -= 1

    def visit_ForStatement(self, node):
        self.wat_code.append(f'{self.indent_str()};; For Statement')
        self.indent += 1
        self.visit(node.for_condition) 
        self.visit(node.statement_list)  
        self.indent -= 1

    def visit_WhileStatement(self, node):
        self.wat_code.append(f'{self.indent_str()};; While Statement')
        self.indent += 1
        self.visit(node.condition)
        self.visit(node.statement_list)
        self.indent -= 1

    def visit_TryExcept(self, node):
        self.visit(node.try_block)
        self.visit(node.CatchS.Statements)

    def visit_Print(self, node):
        self.visit(node.printing_data)
    
    def indent_str(self):
        return '  ' * self.indent
