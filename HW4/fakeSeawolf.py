#Michael Wang
#109055666
#trueSeawolf

import sys
import ply.lex as lex
import ply.yacc as yacc

############################################################
##########################Lexer#############################
############################################################
reserved = {
	'in': 'IN',
	'not': 'NOT',
	'and': 'AND',
	'or': 'OR',
	'if': 'IF',
	'else': 'ELSE',
	'while': 'WHILE',
	'print': 'PRINT'
}

tokens = [
	'LPAREN',
	'RPAREN',
	'LBRACK',
	'RBRACK',
	'LBRACE',
	'RBRACE',
	'COMMA',
	'SEMICOLON',
	'MULT',
	'DIV',
	'ADD',
	'SUB',
	'MOD',
	'EXP',
	'FLRDIV',
	'ASSIGN',
	'EQUAL',
	'NEQUAL',
	'LESS',
	'MORE',
	'LESSEQUAL',
	'MOREEQUAL',
	'REAL',
	'INT',
	'STRING',
	'VAR'
] + list(reserved.values())

def myLexer():

	# Regular expression rules for simple tokens
	t_LPAREN = r'\('
	t_RPAREN = r'\)'
	t_LBRACK = r'\['
	t_RBRACK = r'\]'
	t_LBRACE = r'\{'
	t_RBRACE = r'\}'
	t_COMMA = r'\,'
	t_SEMICOLON = r'\;'
	t_EXP = r'\*\*'
	t_FLRDIV = r'\/\/'
	t_ASSIGN = r'\='
	t_EQUAL = r'\=\='
	t_NEQUAL = r'\<\>'
	t_ADD = r'\+'
	t_SUB = r'\-'
	t_MULT = r'\*'
	t_DIV = r'\/'
	t_MOD = r'\%'
	t_LESS = r'\<'
	t_MORE = r'\>'
	t_LESSEQUAL = r'\<\='
	t_MOREEQUAL = r'\>\='

	def t_VAR(t):
		r'[a-zA-Z_][a-zA-Z_0-9]*'
		t.type = reserved.get(t.value,'VAR')    # Check for reserved words
		return t

	def t_STRING(t):
		r'\"[^\"]*\"'
		t.value = t.value[1:len(t.value)-1]
		return t

	def t_REAL(t):
		r'\d+\.\d+'
		t.value = float(t.value)
		return t

	def t_INT(t):
		r'\d+'
		t.value = int(t.value)
		return t

	# ignore spaces and tabs
	t_ignore = ' \t'

	# skip errors
	def t_error(t):
		t.lexer.skip(1)

	return lex.lex()

lexer = myLexer() # build lexer

# try lexing some sample text

#data = 'if(1) {1;} not 1'
#lexer.input(data)
#while True:
#	tok = lexer.token()
#	if not tok:
#		break
#	print(tok)

##########################################################
########################Parser############################
##########################################################
global syntaxError
syntaxError = False
global semanticError
semanticError = False

dict = {} # contains the variables

precedence = (
	('right', 'ASSIGN'),
	('left', 'OR'),
	('left', 'AND'),
	('left', 'NOT'),
	('nonassoc', 'EQUAL', 'NEQUAL', 'LESS', 'MORE', 'LESSEQUAL', 'MOREEQUAL'),
	('left', 'IN'),
	('left', 'ADD', 'SUB'),
	('left', 'FLRDIV'),
	('left', 'EXP'),
	('left', 'MOD'),
	('left', 'MULT', 'DIV'),
	('left', 'LBRACK', 'RBRACK'),
	('left', 'LPAREN', 'RPAREN')
)

### Fodor code begin ###

class Node:
    def __init__(self):
        return None
    def evaluate(self):
        return "NaN" # Fodor used return 0
    def execute(self):
    	return "NaN" # I use NaN since 0 can conflict with other evaluates/executes
   
class NumberNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        return self.value
    def execute(self):
        return self.value
 
class StringNode(Node):
    def __init__(self, v):
        self.value = str(v)
        self.value = self.value # to eliminate the left and right double quotes
        # Fodor used self.value[1:-1] but I just use self.value because I already eliminated the double quotes
    def evaluate(self):
        return self.value
    def execute(self):
        return self.value
 
class PrintNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        return 0
    def execute(self):
        print(self.value.evaluate())
 
class IfNode(Node):
    def __init__(self, c, t, e):
        self.condition = c
        self.thenBlock = t
        self.elseBlock= e
    def evaluate(self):
        return 0
    def execute(self):
        if(self.condition.evaluate()):
            self.thenBlock.execute()
        else:
            self.elseBlock.execute()
 
class BlockNode(Node):
    def __init__(self, sl):
        self.statementNodes = sl
    def evaluate(self):
        return 0
    def execute(self):
        for statement in self.statementNodes:
        	statement.execute()

### Fodor code end ###

# just contains the list data and returns it
class ListNode(Node):
	def __init__(self, l):
		self.list = l
	def evaluate(self):
		return self.list
	def execute(self):
		return self.list

# looks in the dictionary for the value for a key
class VarNode(Node):
	def __init__(self, n):
		self.name = n
	def evaluate(self):
		return dict[self.name]
	def execute(self):
		return dict[self.name]

# evaluates the expression
class ExpressionNode(Node):
	# format: (operator, value1, value2), value2 is Node() if one of special cases
	def __init__(self, o, v1, v2):
		self.operator = o
		self.value1 = v1
		self.value2 = v2
	def evaluate(self):
		return self.execute()
	def execute(self):
		try:
			# check if NaN and +/-, means a negative/positive number, not to be confused with +/- as an operator
			if(self.value1.evaluate() == "NaN"):
				if(self.operator == 'not'):
					return not self.value1.evaluate()
				elif(self.operator == '+'):
					return 0 + self.value1.evaluate()
				elif(self.operator == '-'):
					return 0 - self.value1.evaluate()
			else:
				if(self.operator == 'index'):
					return self.value1.evaluate()[self.value2.evaluate()]
				elif(self.operator == 'in'):
					return self.value1.evaluate() in self.value2.evaluate()
				elif(self.operator == 'and'):
					return self.value1.evaluate() and self.value2.evaluate()
				elif(self.operator == 'or'):
					return self.value1.evaluate() or self.value2.evaluate()
				elif(self.operator == '+'):
					return self.value1.evaluate() + self.value2.evaluate()
				elif(self.operator == '-'):
					return self.value1.evaluate() - self.value2.evaluate()
				elif(self.operator == '*'):
					return self.value1.evaluate() * self.value2.evaluate()
				elif(self.operator == '/'):
					return self.value1.evaluate() / self.value2.evaluate()
				elif(self.operator == '%'):
					return self.value1.evaluate() % self.value2.evaluate()
				elif(self.operator == '**'):
					return self.value1.evaluate() ** self.value2.evaluate()
				elif(self.operator == '//'):
					return self.value1.evaluate() // self.value2.evaluate()
				elif(self.operator == '=='):
					return self.value1.evaluate() == self.value2.evaluate()
				elif(self.operator == '<>'):
					return self.value1.evaluate() != self.value2.evaluate()
				elif(self.operator == '<'):
					return self.value1.evaluate() < self.value2.evaluate()
				elif(self.operator == '>'):
					return self.value1.evaluate() > self.value2.evaluate()
				elif(self.operator == '<='):
					return self.value1.evaluate() <= self.value2.evaluate()
				elif(self.operator == '>='):
					return self.value1.evaluate() >= self.value2.evaluate()
		except:
			semanticError = True

# if the name is simply a variable, then fetch from dictionary
# otherwise build a list of indices, and apply to variable before assignment
class AssignNode(Node):
	def __init__(self, n, v):
		self.name = n
		self.value = v
		dict[self.name] = self.value
	def evaluate(self):
		dict[self.name] = self.value.evaluate()
	def execute(self):
		if(type(self.name) is VarNode):
			dict[self.name.name] = self.value.evaluate()
		else:
			assignto = self.name.value1
			indices = [self.name.value2.evaluate()]
			while(type(assignto) is not VarNode):
				indices = indices + [assignto.value2.evaluate()]
				assignto = assignto.value1
			assignto = dict[assignto.name]
			for i in range(len(indices) - 1):
				assignto = assignto[indices[i]]
			assignto[indices[len(indices)-1]] = self.value.evaluate()

class WhileNode(Node):
	def __init__(self, c, b):
		self.condition = c
		self.block = b
	def evaluate(self):
		return 0
	def execute(self):
		while(self.condition.evaluate()):
			self.block.execute()

def p_program(p):
	'''
	program : statement
	'''
	p[0] = p[1]

### Fodor code begin ###

# sample parse rules:
def p_statement_print(p):
    '''
    statement : PRINT LPAREN expression RPAREN SEMICOLON
    '''
    p[0] = PrintNode(p[3]) # create nodes in the tree instead of executing the current expression
 
def p_statement_block(p):
    '''
    statement : LBRACE block RBRACE
    '''
    p[0] = BlockNode(p[2])

### Fodor code end ###

def p_block(p):
	'''
	block : statement_list

	'''
	p[0] = p[1]

def p_statement_list(p):
	'''
	statement_list : statement statement_list
				   | expression SEMICOLON
				   | empty
	'''
	if(len(p) == 2):
		p[0] = [p[1]]
	else:
		if(p[2] == ';'):
			p[0] = [p[1]]
		else:
			p[0] = [p[1]] + p[2]

def p_statement_while(p):
	'''
	statement : WHILE LPAREN expression RPAREN LBRACE block RBRACE
	'''
	p[0] = WhileNode(p[3], BlockNode(p[6]))

def p_statement_assignment(p):
	'''
	statement : VAR ASSIGN expression SEMICOLON
			  | expression ASSIGN expression SEMICOLON
	'''
	p[0] = AssignNode(p[1], p[3])

def p_statement_if(p):
	'''
	statement : IF LPAREN expression RPAREN LBRACE block RBRACE ELSE LBRACE block RBRACE
	   		  | IF LPAREN expression RPAREN LBRACE block RBRACE
	'''
	if(len(p) > 8):
		p[0] = IfNode(p[3], BlockNode(p[6]), BlockNode(p[10]))
	else:
		p[0] = IfNode(p[3], BlockNode(p[6]), Node())

def p_expression(p):
	'''
	expression : expression ADD expression
			   | expression SUB expression
			   | expression MULT expression
			   | expression DIV expression
			   | expression MOD expression
			   | expression EXP expression
			   | expression FLRDIV expression
			   | expression EQUAL expression
			   | expression NEQUAL expression
			   | expression LESS expression
			   | expression MORE expression
			   | expression LESSEQUAL expression
			   | expression MOREEQUAL expression
			   | expression IN expression
			   | LPAREN expression RPAREN
			   | NOT expression
			   | expression AND expression
			   | expression OR expression
			   | expression LBRACK expression RBRACK
			   | ADD expression
			   | SUB expression		   
	'''
	# if length is 3, it is a operation with one operand (not, postive, negative)
	if(len(p) == 3):
		p[0] = ExpressionNode(p[1], p[2], Node())
	# if length is 5, create ExpressionNode with operation 'index'
	elif(len(p) == 5):
		p[0] = ExpressionNode("index", p[1], p[3])
	# if parentheses, then return what's inside
	elif(p[1] == '(' and p[3] == ')'):
		p[0] = p[2]
	# otherwise operation is just p[2]
	else:
		p[0] = ExpressionNode(p[2], p[1], p[3])

# a term is an int, float, str, or list, or var
# we will not build the VarNode here, it will be passed through p[1]
def p_expression_term(p):
	'''
	expression : term
	'''
	if(type(p[1]) is str):
		p[0] = StringNode(p[1])
	elif(type(p[1]) is list):
		p[0] = ListNode(p[1])
	elif(type(p[1]) is int or type(p[1]) is float):
		p[0] = NumberNode(p[1])
	else:
		p[0] = p[1]


def p_term_int_real_string(p):
	'''
		term : INT
		| REAL
		| STRING
		| var
		| LBRACK RBRACK
		| LBRACK expression RBRACK
		| LBRACK head COMMA tail RBRACK
		| LBRACK expression RBRACK LBRACK expression RBRACK
	'''
	if(p[1] == '[' and p[2] == ']'):
		p[0] = []
	elif(p[1] == '[' and p[3] == ']'):
		p[0] = [p[2].evaluate()]
	elif(p[1] == '[' and p[5] == ']'):
		p[0] = [p[2]] + p[4]
	else:
		p[0] = p[1]

def p_var(p):
	'''
	var : VAR
	'''
	p[0] = VarNode(p[1])

def p_head_term(p):
	'''
	head : term
	'''
	if(len(p) == 2):
		p[0] = p[1]

def p_tail(p):
	'''
	tail : term COMMA tail
		 | term
	'''
	if(len(p) == 4):
		p[0] = [p[1]] + p[3]
	else:
		p[0] = [p[1]]

def p_empty(p):
	'''
	empty :
	'''
	p[0] = Node()

def p_error(p):
	global syntaxError
	syntaxError = True

parser = yacc.yacc()
fileName = sys.argv[1]
inf = open(fileName, 'r')
line = inf.read()
# execute the abstract syntax tree for the whole program that you read from the file
ast = parser.parse(line)
ast.execute();