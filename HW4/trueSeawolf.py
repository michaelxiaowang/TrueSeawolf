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

lexer = myLexer()
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

dict = {}

precedence = (
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
	('left', 'LPAREN', 'RPAREN'),
	('right', 'ASSIGN'),
	('left', 'SEMICOLON')
)

def p_program(p):
	'''
	program : statement
	'''
	p[0] = p[1]

def p_statement(p):
	'''
	statement : block statement
			  | assignment statement
			  | if statement
			  | while statement
			  | print statement
			  | expression SEMICOLON
			  | empty
	'''
	p[0] = p[1]

def p_block(p):
	'''
	block : LBRACE statement RBRACE

	'''
	p[0] = p[2]

def p_assignment(p):
	'''
	assignment : VAR ASSIGN expression SEMICOLON
	'''
	p[0] = p[3]
	dict[p[1]]=p[3]

def p_if(p):
	'''
	if : IF LPAREN expression RPAREN block
	   | IF LPAREN expression RPAREN block ELSE block
	'''
	if(p[3]):
		p[0] = p[5]
	elif(len(p) > 6):
		p[0] = p[7]

def p_while(p):
	'''
	while : WHILE LPAREN expression RPAREN block
	'''
	while(p[3]):
		p[1] = p[5]
	p[0] = p[1]

def p_print(p):
	'''
	print : PRINT LPAREN expression RPAREN SEMICOLON
	'''
	print(p[3])

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
			   | NOT expression
			   | expression AND expression
			   | expression OR expression
			   | expression LBRACK expression RBRACK
			   | ADD expression
			   | SUB expression		   
	'''

	if(p[2] == '+'):
		try:
			p[0] = p[1] + p[3]
		except:
			semanticError = True
	elif(p[2] == '-'):
		try:
			p[0] = p[1] - p[3]
		except:
			semanticError = True
	elif(p[2] == '*'):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = p[1] * p[3]
		else:
			semanticError = True
	elif(p[2] == '/'):
		try:
			p[0] = p[1] / p[3]
		except:
			semanticError = True
	elif(p[2] == '%'):
		try:
			p[0] = p[1] % p[3]
		except:
			semanticError = True
	elif(p[2] == '**'):
		try:
			p[0] = p[1] ** p[3]
		except:
			semanticError = True
	elif(p[2] == '//'):
		try:
			p[0] = p[1] // p[3]
		except:
			semanticError = True
	elif(p[2] == '=='):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] == p[3] else 0
		else:
			semanticError = True
	elif(p[2] == '<>'):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] != p[3] else 0
		else:
			semanticError = True
	elif(p[2] == '<'):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] < p[3] else 0
		else:
			semanticError = True
	elif(p[2] == '>'):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] > p[3] else 0
		else:
			semanticError = True
	elif(p[2] == '<='):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] <= p[3] else 0
		else:
			semanticError = True
	elif(p[2] == '>='):
		if((type(p[1]) is int or type(p[1]) is float) and (type(p[3]) is int or type(p[3]) is float)):
			p[0] = 1 if p[1] >= p[3] else 0
		else:
			semanticError = True
	elif(p[2] == 'in'):
		try:
			p[0] = 1 if p[1] in p[3] else 0
		except:
			semanticError = True
	elif(p[1] == 'not'):
		if(type(p[2]) is int):
			p[0] = 1 if not p[2] else 0
		else:
			semanticError = True
	elif(p[2] == 'and'):
		if(type(p[1]) is int and type(p[3]) is int):
			p[0] = 1 if p[1] and p[3] else 0
		else:
			semanticError = True
	elif(p[2] == 'or'):
		if(type(p[1]) is int and type(p[3]) is int):
			p[0] = 1 if p[1] or p[3] else 0
		else:
			semanticError = True
	elif(p[1] == '(' and p[3] == ')'):
		p[0] = p[2]
	elif(p[2] == '[' and p[4] == ']'):
		if((type(p[1]) is str or type(p[1]) is list) and type(p[3]) is int):
			p[0] = (p[1])[(p[3])]
		else:
			semanticError = True
	elif(p[1] == '+' or p[1] == '-'):
		if(p[1] == '+' and (type(p[2]) is int or type(p[2]) is float)):
			p[0] = p[2]
		elif(p[1] == '-' and (type(p[2]) is int or type(p[2]) is float)):
			p[0] = 0 - p[2]
		else:
			semanticError = True

def p_expression_term(p):
	'''
	expression : term
	'''
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
		p[0] = [p[2]]
	elif(p[1] == '[' and p[5] == ']'):
		p[0] = [p[2]] + p[4]
	else:
		p[0] = p[1]

def p_var(p):
	'''
	var : VAR
	'''
	p[0] = dict[p[1]]

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
	p[0] = None

def p_error(p):
	global syntaxError
	syntaxError = True

parser = yacc.yacc()

fileName = sys.argv[1]
inf = open(fileName, 'r')
line = inf.read()
result = parser.parse(line)
if(semanticError):
	print("SEMANTIC ERROR")
	semanticError = False
elif(syntaxError):
	print("SYNTAX ERROR")
	syntaxError = False

while True:
	try:
		s = input('>>>')
	except EOFError:
		break
	if not s: continue
	result = parser.parse(s)
	if(not semanticError and not syntaxError):
		print (result)
	if(semanticError):
		print("SEMANTIC ERROR")
		semanticError = False
	if(syntaxError):
		print("SYNTAX ERROR")
		syntaxError = False