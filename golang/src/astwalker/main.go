package main

import (
	"os"
	"fmt"
	"strings"
	"io/ioutil"
	"go/ast"
	"go/parser"
	"go/token"
)

func main() {

	if len(os.Args) < 2 {
		fmt.Printf("usage: %s <go-src>\n", os.Args[0])
		return
	}

	data, err := ioutil.ReadFile(os.Args[1])
    if err != nil {
        fmt.Println("File reading error", err)
        return
	}
	src := string(data)

	if err != nil {
		panic(err)
	}

	ParseAndWalk(os.Args[1], src)
}

var indent = 0
var indentSpace = "  "

func Indent() {
	for i := 0; i < indent; i++ {
		fmt.Printf(indentSpace)
	}
}

func Escape(val interface{}) string {
	value := fmt.Sprintf("%s", val)
	value = strings.ReplaceAll(value, "&", "&amp;")
	value = strings.ReplaceAll(value, "<", "&lt;")
	value = strings.ReplaceAll(value, ">", "&gt;")
	return value
}

func ParseAndWalk(filename string, source string) {
	fset := token.NewFileSet() // positions are relative to fset
	file, err := parser.ParseFile(fset, filename, source, parser.ParseComments)

	if err != nil {
		panic(err)
	}

	if file == nil {
		panic("parsing file failed with nil")
	}

	WalkFile(*file)
}

func WalkFile(node ast.File) {
/*
type File struct {
    Doc        *CommentGroup   // associated documentation; or nil
    Package    token.Pos       // position of "package" keyword
    Name       *Ident          // package name
    Decls      []Decl          // top-level declarations; or nil
    Scope      *Scope          // package scope (this file only)
    Imports    []*ImportSpec   // imports in this file
    Unresolved []*Ident        // unresolved identifiers in this file
    Comments   []*CommentGroup // list of all comments in the source file
}
*/

	Indent()
	fmt.Printf("<File package=\"%s\">\n", node.Name.Name)
	indent++;

	if node.Imports != nil {
		Indent()
		fmt.Printf("<Imports>\n")
		indent++
		for _, impo := range node.Imports {
			WalkImport(*impo)
		}
		indent--
		Indent()
		fmt.Printf("</Imports>\n")
	}

	WalkScope(*node.Scope)

	if node.Decls != nil {
		WalkDecls(node.Decls)
	}

	if node.Unresolved != nil {
		Indent()
		fmt.Printf("<Unresolved>\n")
		indent++

		for _, name := range node.Unresolved {
			kind := "?"
			if name.Obj != nil {
				kind = fmt.Sprintf("%s", name.Obj.Kind)
			}

			Indent()
			fmt.Printf("<Ident name=\"%s\" kind=\"%s\" />\n", name.Name, kind)
		}

		indent--
		Indent()
		fmt.Printf("</Unresolved>\n")
	}

	indent--;
	Indent()
	fmt.Printf("</File>\n");
}

func WalkDecls(decls []ast.Decl) {
/*
type Decl interface {
    Node
    // contains filtered or unexported methods
}
*/
	Indent()
	fmt.Printf("<Decls>\n")
	indent++

	for _, decl := range decls {
		switch v := decl.(type) {
		case *ast.FuncDecl:
			WalkFuncDecl(*v)
		case *ast.GenDecl:
			WalkGenDecl(*v)
		default:
			Indent()
			fmt.Printf("<UNSUPPORTED_DECL type=\"%T\" />\n", decl)
		}
	}

	indent--
	Indent()
	fmt.Printf("</Decls>\n")
}

func WalkGenDecl(node ast.GenDecl) {
/*
A GenDecl node (generic declaration node) represents an import, constant, type or variable
declaration. A valid Lparen position (Lparen.IsValid()) indicates a parenthesized declaration.

Relationship between Tok value and Specs element type:

token.IMPORT  *ImportSpec
token.CONST   *ValueSpec
token.TYPE    *TypeSpec
token.VAR     *ValueSpec

type GenDecl struct {
    Doc    *CommentGroup // associated documentation; or nil
    TokPos token.Pos     // position of Tok
    Tok    token.Token   // IMPORT, CONST, TYPE, VAR
    Lparen token.Pos     // position of '(', if any
    Specs  []Spec
    Rparen token.Pos // position of ')', if any
}
*/
	for _, spec := range node.Specs {
		Indent()
		fmt.Printf("<GenDecl type=\"%s\" spec=\"%T\">\n", node.Tok, spec)
		indent++

		switch v := spec.(type) {
		case *ast.ImportSpec:
			WalkImport(*v)
		case *ast.ValueSpec:
			WalkValueSpec(*v)
		case *ast.TypeSpec:
			WalkTypeSpec(*v)
		default:
			Indent()
			fmt.Printf("<UNSUPPORTED_SPEC type=\"%T\" />\n", spec)
		}

		indent--
		Indent()
		fmt.Printf("</GenDecl>\n")
	}
}

func WalkTypeSpec(spec ast.TypeSpec) {
/*
A TypeSpec node represents a type declaration (TypeSpec production).

type TypeSpec struct {
    Doc     *CommentGroup // associated documentation; or nil
    Name    *Ident        // type name
    Assign  token.Pos     // position of '=', if any; added in Go 1.9
    Type    Expr          // *Ident, *ParenExpr, *SelectorExpr, *StarExpr, or any of the *XxxTypes
    Comment *CommentGroup // line comments; or nil
}
*/
	Indent()
	fmt.Printf("<TypeSpec name=\"%s\">\n", spec.Name.Name)
	indent++

	WalkExpr(spec.Type)

	indent--
	Indent()
	fmt.Printf("</TypeSpec>\n")
}

func WalkValueSpec(spec ast.ValueSpec) {
/*
A ValueSpec node represents a constant or variable declaration (ConstSpec or VarSpec production).

type ValueSpec struct {
    Doc     *CommentGroup // associated documentation; or nil
    Names   []*Ident      // value names (len(Names) > 0)
    Type    Expr          // value type; or nil
    Values  []Expr        // initial values; or nil
    Comment *CommentGroup // line comments; or nil
}
*/
	for _, name := range spec.Names {
		WalkIdentExpr(*name)
	}

	if spec.Type != nil {
		Indent()
		fmt.Printf("<Type>\n")
		indent++

		WalkExpr(spec.Type)

		indent--
		Indent()
		fmt.Printf("</Type>\n")
	}

	if spec.Values != nil {
		for _, value := range spec.Values {
			Indent()
			fmt.Printf("<Values>\n")
			indent++
	
			WalkExpr(value)
	
			indent--
			Indent()
			fmt.Printf("</Values>\n")
		}
	}
}

func WalkFuncDecl(node ast.FuncDecl) {
/*
A FuncDecl node represents a function declaration.

type FuncDecl struct {
    Doc  *CommentGroup // associated documentation; or nil
    Recv *FieldList    // receiver (methods); or nil (functions)
    Name *Ident        // function/method name
    Type *FuncType     // function signature: parameters, results, and position of "func" keyword
    Body *BlockStmt    // function body; or nil for external (non-Go) function
}
*/
/*
A FuncType node represents a function type.

type FuncType struct {
    Func    token.Pos  // position of "func" keyword (token.NoPos if there is no "func")
    Params  *FieldList // (incoming) parameters; non-nil
    Results *FieldList // (outgoing) results; or nil
}
*/
	Indent()
	fmt.Printf("<FuncDecl name=\"%s\" external=\"%v\">\n",
		node.Name.Name, node.Body == nil)
	indent++

	if node.Recv != nil {
		WalkFieldList("Recv", *node.Recv)
	}

	if node.Type.Params != nil {
		WalkFieldList("Params", *node.Type.Params)
	}
	if node.Type.Results != nil {
		WalkFieldList("Results", *node.Type.Results)
	}

	if node.Body != nil {
		WalkBlockStmt(*node.Body)
	}

	indent--
	Indent()
	fmt.Printf("</FuncDecl>\n")
}

func WalkBlockStmt(block ast.BlockStmt) {
/*
A BlockStmt node represents a braced statement list.

type BlockStmt struct {
    Lbrace token.Pos // position of "{"
    List   []Stmt
    Rbrace token.Pos // position of "}"
}
*/
	Indent()
	fmt.Printf("<Stmts>\n")
	indent++

	for _, stmt := range block.List {
		WalkStmt(stmt)
	}

	indent--
	Indent()
	fmt.Printf("</Stmts>\n")
}

func WalkStmt(stmt ast.Stmt) {
/*
All statement nodes implement the Stmt interface.

type Stmt interface {
    Node
    // contains filtered or unexported methods
}
*/
	switch v := stmt.(type) {
	case *ast.ExprStmt:
		WalkExpr(v.X)
	case *ast.ReturnStmt:
		WalkReturnStmt(*v)
	case *ast.AssignStmt:
		WalkAssignStmt(*v)
	case *ast.IfStmt:
		WalkIfStmt(*v)
	case *ast.BlockStmt:
		WalkBlockStmt(*v)
	default:
		Indent()
		fmt.Printf("<UNSUPPORTED what=\"STMT\" type=\"%T\" />\n", stmt)
	}
}

func WalkIfStmt(stmt ast.IfStmt) {
/*
An IfStmt node represents an if statement.

type IfStmt struct {
    If   token.Pos // position of "if" keyword
    Init Stmt      // initialization statement; or nil
    Cond Expr      // condition
    Body *BlockStmt
    Else Stmt // else branch; or nil
}
*/
	Indent()
	fmt.Printf("<IfStmt>\n")
	indent++

	if stmt.Init != nil {
		Indent()
		fmt.Printf("<Init>\n")
		indent++

		WalkStmt(stmt.Init)

		indent--
		Indent()
		fmt.Printf("</Init>\n")
	}

	Indent()
	fmt.Printf("<Cond>\n")
	indent++

	WalkExpr(stmt.Cond)

	indent--
	Indent()
	fmt.Printf("</Cond>\n")

	WalkBlockStmt(*stmt.Body)

	if stmt.Else != nil {
		Indent()
		fmt.Printf("<Else>\n")
		indent++

		WalkStmt(stmt.Else)

		indent--
		Indent()
		fmt.Printf("</Else>\n")
	}

	indent--
	Indent()
	fmt.Printf("</IfStmt>\n")
}

func WalkAssignStmt(stmt ast.AssignStmt) {
/*
An AssignStmt node represents an assignment or a short variable declaration.

type AssignStmt struct {
    Lhs    []Expr
    TokPos token.Pos   // position of Tok
    Tok    token.Token // assignment token, DEFINE
    Rhs    []Expr
}
*/
	Indent()
	fmt.Printf("<AssignStmt tok=\"%s\">\n", stmt.Tok)
	indent++

	for _, lhs := range stmt.Lhs {
		Indent()
		fmt.Printf("<Lhs>\n")
		indent++
		WalkExpr(lhs)
		indent--
		Indent()
		fmt.Printf("</Lhs>\n")
	}

	for _, rhs := range stmt.Rhs {
		Indent()
		fmt.Printf("<Rhs>\n")
		indent++
		WalkExpr(rhs)
		indent--
		Indent()
		fmt.Printf("</Rhs>\n")
	}

	indent--
	Indent()
	fmt.Printf("</AssignStmt>\n")
}

func WalkReturnStmt(stmt ast.ReturnStmt) {
/*
A ReturnStmt node represents a return statement.

type ReturnStmt struct {
    Return  token.Pos // position of "return" keyword
    Results []Expr    // result expressions; or nil
}
*/
	Indent()
	fmt.Printf("<ReturnStmt>\n")
	indent++

	if stmt.Results != nil {
		for _, expr := range stmt.Results {
			WalkExpr(expr)
		}
	}

	indent--
	Indent()
	fmt.Printf("</ReturnStmt>\n")
}

func WalkExpr(expr ast.Expr) {
/*
All expression nodes implement the Expr interface.

type Expr interface {
    Node
    // contains filtered or unexported methods
}
*/
	switch v := expr.(type) {
	case *ast.Ident:
		WalkIdentExpr(*v)
	case *ast.BasicLit:
		WalkBasicLit(*v)
	case *ast.CompositeLit:
		WalkCompositeLit(*v)
	case *ast.SelectorExpr:
		WalkSelectorExpr(*v)
	case *ast.CallExpr:
		WalkCallExpr(*v)
	case *ast.UnaryExpr:
		WalkUnaryExpr(*v)
	case *ast.StarExpr:
		WalkStarExpr(*v)
	case *ast.BinaryExpr:
		WalkBinaryExpr(*v)
	case *ast.ParenExpr:
		WalkParenExpr(*v)
	case *ast.StructType:
		WalkStructType(*v)
	default:
		Indent()
		fmt.Printf("<UNSUPPORTED what=\"EXPR\" type=\"%T\" />\n", expr)
	}
}


func WalkStructType(typ ast.StructType) {
/*
A StructType node represents a struct type.

type StructType struct {
    Struct     token.Pos  // position of "struct" keyword
    Fields     *FieldList // list of field declarations
    Incomplete bool       // true if (source) fields are missing in the Fields list
}
*/
	Indent()
	fmt.Printf("<StructType incomplete=\"%v\">\n", typ.Incomplete)
	indent++

	WalkFieldList("Fields", *typ.Fields)

	indent--
	Indent()
	fmt.Printf("</StructType>\n")
}

func WalkParenExpr(expr ast.ParenExpr) {
/*
A ParenExpr node represents a parenthesized expression.

type ParenExpr struct {
    Lparen token.Pos // position of "("
    X      Expr      // parenthesized expression
    Rparen token.Pos // position of ")"
}
*/
	Indent()
	fmt.Printf("<ParenExpr>\n")

	WalkExpr(expr.X)
}

func WalkUnaryExpr(expr ast.UnaryExpr) {
/*
A UnaryExpr node represents a unary expression. Unary "*"
expressions are represented via StarExpr nodes.

type UnaryExpr struct {
    OpPos token.Pos   // position of Op
    Op    token.Token // operator
    X     Expr        // operand
}
*/
	Indent()
	fmt.Printf("<UnaryExpr op=\"%s\">\n", Escape(expr.Op))
	indent++

	WalkExpr(expr.X)

	indent--
	Indent()
	fmt.Printf("</UnaryExpr>\n")
}

func WalkStarExpr(expr ast.StarExpr) {
/*
A StarExpr node represents an expression of the form "*" Expression.
Semantically it could be a unary "*" expression, or a pointer type.

type StarExpr struct {
    Star token.Pos // position of "*"
    X    Expr      // operand
}
*/
	Indent()
	fmt.Printf("<StarExpr>\n")
	indent++

	WalkExpr(expr.X)

	indent--
	Indent()
	fmt.Printf("</StarExpr>\n")
}

func WalkBinaryExpr(expr ast.BinaryExpr) {
/*
A BinaryExpr node represents a binary expression.

type BinaryExpr struct {
    X     Expr        // left operand
    OpPos token.Pos   // position of Op
    Op    token.Token // operator
    Y     Expr        // right operand
}
*/
	Indent()
	fmt.Printf("<BinaryExpr op=\"%s\">\n", Escape(expr.Op))
	indent++

	WalkExpr(expr.X)
	WalkExpr(expr.Y)

	indent--
	Indent()
	fmt.Printf("</BinaryExpr>\n")
}

func WalkCallExpr(expr ast.CallExpr) {
/*
A CallExpr node represents an expression followed by an argument list.

type CallExpr struct {
    Fun      Expr      // function expression
    Lparen   token.Pos // position of "("
    Args     []Expr    // function arguments; or nil
    Ellipsis token.Pos // position of "..." (token.NoPos if there is no "...")
    Rparen   token.Pos // position of ")"
}
*/
	Indent()
	fmt.Printf("<CallExpr ellipsis=\"%v\">\n", expr.Ellipsis != token.NoPos)
	indent++

	Indent()
	fmt.Printf("<Fun>\n")
	indent++

	WalkExpr(expr.Fun)

	indent--
	Indent()
	fmt.Printf("</Fun>\n")

	Indent()
	fmt.Printf("<Args>\n")
	indent++

	if expr.Args != nil {
		for _, arg := range expr.Args {
			WalkExpr(arg)
		}
	}

	indent--
	Indent()
	fmt.Printf("</Args>\n")

	indent--
	Indent()
	fmt.Printf("</CallExpr>\n")
}

func WalkSelectorExpr(expr ast.SelectorExpr) {
/*
A SelectorExpr node represents an expression followed by a selector.

type SelectorExpr struct {
    X   Expr   // expression
    Sel *Ident // field selector
}
*/
	WalkExpr(expr.X)
	
	Indent()
	fmt.Printf("<Selector name=\"%s\" />\n", expr.Sel.Name)
}

func WalkIdentExpr(ident ast.Ident) {
/*
An Ident node represents an identifier.

type Ident struct {
    NamePos token.Pos // identifier position
    Name    string    // identifier name
    Obj     *Object   // denoted object; or nil
}
*/
	Indent()
	//fmt.Printf("<Identifier name=\"%s\" obj=\"%s\" />\n", ident.Name, ident.Obj)	
	fmt.Printf("<Identifier name=\"%s\" />\n", ident.Name)	
}

func WalkBasicLit(lit ast.BasicLit) {
/*
A BasicLit node represents a literal of basic type.

type BasicLit struct {
    ValuePos token.Pos   // literal position
    Kind     token.Token // token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
    Value    string      // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
}
*/
	Indent()	
	fmt.Printf("<BasicLit kind=\"%s\">%s</BasicLit>\n", lit.Kind, Escape(lit.Value))	
}

func WalkCompositeLit(lit ast.CompositeLit) {
/*
A CompositeLit node represents a composite literal.

type CompositeLit struct {
    Type       Expr      // literal type; or nil
    Lbrace     token.Pos // position of "{"
    Elts       []Expr    // list of composite elements; or nil
    Rbrace     token.Pos // position of "}"
    Incomplete bool      // true if (source) expressions are missing in the Elts list; added in Go 1.11
}
*/
	Indent()
	fmt.Printf("<CompositeLit incomplete=\"%v\">\n", lit.Incomplete)
	indent++
	
	if lit.Type != nil {
		Indent()
		fmt.Printf("<Type>\n")
		indent++

		WalkExpr(lit.Type)

		indent--
		Indent()
		fmt.Printf("</Type>\n")
	}

	if lit.Elts != nil {
		Indent()
		fmt.Printf("<Elements>\n")
		indent++

		for _, elt := range lit.Elts {
			WalkExpr(elt)
		}

		indent--
		Indent()
		fmt.Printf("</Elements>\n")
	}
	
	indent--
	Indent()
	fmt.Printf("</CompositeLit>\n")
}

func WalkFieldList(label string, fields ast.FieldList) {
/*
type FieldList struct {
    Opening token.Pos // position of opening parenthesis/brace, if any
    List    []*Field  // field list; or nil
    Closing token.Pos // position of closing parenthesis/brace, if any
}
*/
	Indent()
	fmt.Printf("<%s>\n", label)
	indent++

	for _, field := range fields.List {
		WalkField(*field)
	}

	indent--
	Indent()
	fmt.Printf("</%s>\n", label)
}

func WalkField(node ast.Field) {
/*
A Field represents a Field declaration list in a struct type, a method list in an
interface type, or a parameter/result declaration in a signature. Field.Names is
nil for unnamed parameters (parameter lists which only contain types) and embedded
struct fields. In the latter case, the field name is the type name.

type Field struct {
    Doc     *CommentGroup // associated documentation; or nil
    Names   []*Ident      // field/method/parameter names; or nil
    Type    Expr          // field/method/parameter type
    Tag     *BasicLit     // field tag; or nil
    Comment *CommentGroup // line comments; or nil
}
*/

	tag := ""
	if node.Tag != nil { tag = node.Tag.Value }

	if node.Names != nil {
		for _, name := range node.Names {
			Indent()
			fmt.Printf("<Field name=\"%s\" tag=\"%s\">\n", name.Name, tag)
			indent++

			WalkExpr(node.Type)

			indent--
			Indent()
			fmt.Printf("</Field>\n")
		}
	} else {
		Indent()
		fmt.Printf("<Field tag=\"%s\">\n", tag)
		indent++

		WalkExpr(node.Type)

		indent--
		Indent()
		fmt.Printf("</Field>\n")
	}
}

func WalkScope(node ast.Scope) {
/*
type Scope struct {
    Outer   *Scope
    Objects map[string]*Object
}
*/
	Indent()
	fmt.Printf("<Scope level=\"Package\">\n")
	indent++
	for k, v := range node.Objects {
		Indent()
		fmt.Printf("<Object name=\"%s\" kind=\"%s\" type=\"%T\" />\n", k, v.Kind, v)
	}
	indent--
	Indent()
	fmt.Printf("</Scope>\n")
}

func WalkImport(node ast.ImportSpec) {
/*
type ImportSpec struct {
    Doc     *CommentGroup // associated documentation; or nil
    Name    *Ident        // local package name (including "."); or nil
    Path    *BasicLit     // import path
    Comment *CommentGroup // line comments; or nil
    EndPos  token.Pos     // end of spec (overrides Path.Pos if nonzero)
}
*/
	name := ""
	if node.Name != nil { name = node.Name.Name }

	Indent()
	fmt.Printf("<Import name=\"%s\" path='%s' pathType=\"%s\" />\n",
		name,
		node.Path.Value,
		node.Path.Kind)

}
