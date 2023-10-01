#include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token
{
    TOK_EOF = -1,

    // commands
    TOK_FN = -2,
    TOK_USE = -3,
    TOK_LET = -4,

    // primary
    TOK_IDENT = -5,
    TOK_INT = -6,
    TOK_FLOAT = -7,
    TOK_STR = -8,
    TOK_ARG_TYPE = -9,
    TOK_IF = -10,
    TOK_ELSE = -11,
    TOK_FOR = -12,
    TOK_IN = -13,
    TOK_WHILE = -14,

    // keywords, symbols
    KW_I32 = -15,
    KW_F32 = -16,

    LEFT_PAREN = '(',
    RIGHT_PAREN = ')',
    LEFT_BRACE = '{',
    RIGHT_BRACE = '}',
    COMMA = ',',
    SEMICOLON = ';',
    COLON = ':',
    DOT = '.',
    TRAVERSE = -17, // ..
};

static std::string identifierStr; // Filled in if TOK_IDENT
static int intVal;                // Filled in if TOK_INT
static double floatVal;           // Filled in if TOK_FLOAT

/// gettok - Return the next token from standard input.
static int gettok()
{
    static int lastChar = ' ';

    if (lastChar == '.')
    {
        lastChar = getchar();
        if (lastChar == '.')
        {
            lastChar = getchar();
            return TRAVERSE;
        }
    }

    // Skip any whitespace.
    while (isspace(lastChar))
        lastChar = getchar();

    if (lastChar == '(')
    {
        lastChar = getchar();
        return LEFT_PAREN;
    }

    if (lastChar == ')')
    {
        lastChar = getchar();
        return RIGHT_PAREN;
    }

    // identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(lastChar))
    {
        identifierStr = lastChar;

        while (isalnum((lastChar = getchar())))
            identifierStr += lastChar;

        if (identifierStr == "i32")
            return TOK_ARG_TYPE;

        if (identifierStr == "f32")
            return TOK_ARG_TYPE;

        if (identifierStr == "fn")
            return TOK_FN;

        if (identifierStr == "use")
            return TOK_USE;

        if (identifierStr == "let")
            return TOK_LET;

        if (identifierStr == "if")
            return TOK_IF;

        if (identifierStr == "else")
            return TOK_ELSE;

        if (identifierStr == "for")
            return TOK_FOR;

        if (identifierStr == "while")
            return TOK_WHILE;

        if (identifierStr == "in")
            return TOK_IN;

        return TOK_IDENT;
    }

    // Integer: [0-9]+ or Float: [0-9]+.[0-9]+
    if (isdigit(lastChar) || (lastChar == '.'))
    {
        std::string numStr;
        bool isFloat = false;

        do
        {
            numStr += lastChar;

            if (lastChar == '.')
            {
                if (isFloat)
                {
                    break;
                }

                isFloat = true;
            }

            lastChar = getchar();
        } while (isdigit(lastChar) || (!isFloat && lastChar == '.'));

        if (isFloat)
        {
            floatVal = strtod(numStr.c_str(), nullptr);
            return TOK_FLOAT;
        }
        else
        {
            intVal = strtol(numStr.c_str(), nullptr, 10);
            return TOK_INT;
        }
    }

    if (lastChar == '/' && ((lastChar = getchar()) == '/'))
    {
        // Comment until end of line.
        // single-line comment
        do
            lastChar = getchar();
        while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

        if (lastChar != EOF)
            return gettok();
    }
    else if (lastChar == '/' && ((lastChar = getchar()) == '*'))
    {
        // Comment until the closing '*/'.
        // multi-line comment
        do
            lastChar = getchar();
        while (lastChar != EOF && lastChar != '*' && lastChar != '/');

        if (lastChar != EOF)
            return gettok();
    }

    // Check for end of file. Don't eat the EOF.
    if (lastChar == EOF)
        return TOK_EOF;

    // Otherwise, just return the character as its ascii value.
    int thisChar = lastChar;
    lastChar = getchar();
    return thisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace
{

    /// ExprAST - Base class for all expression nodes.
    class ExprAST
    {
    public:
        virtual ~ExprAST() = default;

        virtual Value *codegen() = 0;
    };

    // IntegerExprAST - Expression class for integer literals like "1".
    class IntegerExprAST : public ExprAST
    {
        int Val;

    public:
        IntegerExprAST(int Val) : Val(Val) {}

        Value *codegen() override;
    };

    // FloatExprAST - Expression class for float literals like "1.0".
    class FloatExprAST : public ExprAST
    {
        double Val;

    public:
        FloatExprAST(double Val) : Val(Val) {}

        Value *codegen() override;
    };

    /// VariableExprAST - Expression class for referencing a variable, like "a".
    class VariableExprAST : public ExprAST
    {
        std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}

        Value *codegen() override;
    };

    /// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST
    {
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                      std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value *codegen() override;
    };

    /// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST
    {
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string &Callee,
                    std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}

        Value *codegen() override;
    };

    /// IfExprAST - Expression class for if/else.
    class IfExprAST : public ExprAST
    {
        std::unique_ptr<ExprAST> Cond;
        std::vector<std::unique_ptr<ExprAST>> IfBlock;
        std::vector<std::unique_ptr<ExprAST>> ElseBlock;

    public:
        IfExprAST(std::unique_ptr<ExprAST> Cond, std::vector<std::unique_ptr<ExprAST>> IfBlock,
                  std::vector<std::unique_ptr<ExprAST>> ElseBlock)
            : Cond(std::move(Cond)), IfBlock(std::move(IfBlock)), ElseBlock(std::move(ElseBlock)) {}

        Value *codegen() override;
    };

    /// ForExprAST - Expression class for for/in.
    class ForExprAST : public ExprAST
    {
        std::string VarName;
        std::unique_ptr<ExprAST> Start, End, Step;
        std::vector<std::unique_ptr<ExprAST>> Body;

    public:
        ForExprAST(const std::string &VarName,
                   std::unique_ptr<ExprAST> Start,
                   std::unique_ptr<ExprAST> End,
                   std::unique_ptr<ExprAST> Step,
                   std::vector<std::unique_ptr<ExprAST>> Body)
            : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
              Step(std::move(Step)), Body(std::move(Body)) {}

        Value *codegen() override;
    };

    class WhileExprAST : public ExprAST
    {
        std::unique_ptr<ExprAST> End;
        std::vector<std::unique_ptr<ExprAST>> Body;

    public:
        WhileExprAST(
            std::unique_ptr<ExprAST> End,
            std::vector<std::unique_ptr<ExprAST>> Body)
            : End(std::move(End)), Body(std::move(Body)) {}

        Value *codegen() override;
    };

    /// PrototypeAST - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    class PrototypeAST
    {
        std::string Name;
        std::vector<std::pair<std::string, std::string>> Args; // Argument name and type pairs.

    public:
        PrototypeAST(const std::string &name, std::vector<std::pair<std::string, std::string>> args)
            : Name(name), Args(std::move(args)) {}

        Function *codegen();

        const std::string &getName() const { return Name; }
    };

    /// FunctionAST - This class represents a function definition itself.
    class FunctionAST
    {
        std::unique_ptr<PrototypeAST> Proto;
        std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                    std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}

        Function *codegen();
    };

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;

    // Make sure it's a declared binop.
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// integer
static std::unique_ptr<ExprAST> ParseIntExpr()
{
    auto Result = std::make_unique<IntegerExprAST>(intVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

// float
static std::unique_ptr<ExprAST> ParseFloatExpr()
{
    auto Result = std::make_unique<FloatExprAST>(floatVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

// parenthesis operator definition
/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')')
        return LogError("expected ')'");

    getNextToken(); // eat ).
    return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
    std::string IdName = identifierStr;

    getNextToken(); // eat identifier.

    if (CurTok != '(') // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);

    // Call.
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;

    if (CurTok != ')')
    {
        while (true)
        {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')')
                break;

            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");

            getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression '{' expression '}' 'else' '{' expression '}'
static std::unique_ptr<ExprAST> ParseIfExpr()
{
    getNextToken(); // eat the if.

    // condition.
    auto Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    std::vector<std::unique_ptr<ExprAST>> IfBlock;

    if (CurTok != '{')
        return LogError("expected '{' after if condition");

    getNextToken(); // eat '{'

    // if block
    while (CurTok != '}')
    {
        if (auto If = ParseExpression())
            IfBlock.push_back(std::move(If));

        if (CurTok == ';')
            getNextToken(); // eat ';' for the next expression
        else
            return nullptr;
    }

    getNextToken(); // eat '}'

    if (CurTok != TOK_ELSE)
        return LogError("expected 'else' after if block");

    getNextToken(); // eat 'else'

    std::vector<std::unique_ptr<ExprAST>> ElseBlock;

    if (CurTok != '{')
        return LogError("expected '{' after else");

    getNextToken(); // eat '{'

    // else block
    while (CurTok != '}')
    {
        if (auto Else = ParseExpression())
            ElseBlock.push_back(std::move(Else));

        if (CurTok == ';')
            getNextToken(); // eat ';' for the next expression
        else
            return nullptr;
    }

    getNextToken(); // eat '}'

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(IfBlock),
                                       std::move(ElseBlock));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr()
{
    getNextToken(); // eat the for.

    if (CurTok != TOK_IDENT)
    {
        LogError("expected identifier after for");
        return nullptr;
    }

    std::string IdName = identifierStr; // for i
    getNextToken();                     // eat identifier.

    if (CurTok != TOK_IN)
    {
        LogError("expected 'in' after for");
        return nullptr;
    }

    getNextToken(); // eat 'in' - for i in

    auto Start = ParseExpression(); // for i in 0

    if (!Start)
        return nullptr;

    // print current token
    if (CurTok != TRAVERSE)
    { // .. operator
        LogError("expected '..' after for start value");
        return nullptr;
    }

    getNextToken(); // eat  '..' for i in 0..

    auto End = ParseExpression(); // for i in 0..10

    if (!End)
        return nullptr;

    // The step value is optional.
    // neka bude prazan, a zadana vrijednost 1 prilikom generisanja koda
    std::unique_ptr<ExprAST> Step = nullptr;

    if (CurTok != '{')
    {
        LogError("expected '{' after for");
        return nullptr;
    }

    getNextToken(); // eat '{'

    // body of for loop
    std::vector<std::unique_ptr<ExprAST>> Body;

    while (CurTok != '}')
    {
        Body.push_back(std::move(ParseExpression()));

        if (CurTok == ';')
            getNextToken(); // eat ';' for the next expression
        else
            return nullptr;
    }

    getNextToken(); // eat '}'

    return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                        std::move(Step), std::move(Body));
}

static std::unique_ptr<ExprAST> ParseWhileExpr()
{
    getNextToken(); // eat the while

    auto End = ParseExpression();
    if (!End)
    {
        // errorParser("no end conditon");
        return nullptr;
    }

    if (CurTok != '{')
    {
        // errorParser("expected '{' after while condition");
        return nullptr;
    }

    getNextToken(); // eat '{'

    // body of while loop
    std::vector<std::unique_ptr<ExprAST>> Body;

    while (CurTok != '}')
    {
        Body.push_back(std::move(ParseExpression()));

        if (CurTok == ';')
            getNextToken(); // eat ';' for the next expression
        else
            return nullptr;
    }

    getNextToken(); // eat '}'

    return std::make_unique<WhileExprAST>(std::move(End), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST>
ParsePrimary()
{
    switch (CurTok)
    {
    case TOK_IDENT:
        return ParseIdentifierExpr();
    case TOK_INT:
        return ParseIntExpr();
    case TOK_FLOAT:
        return ParseFloatExpr();
    case LEFT_PAREN:
        return ParseParenExpr();
    case TOK_IF:
        return ParseIfExpr();
    case TOK_FOR:
        return ParseForExpr();
    case TOK_WHILE:
        return ParseWhileExpr();
    default:
        LogError("unknown token when expecting an expression");
        return nullptr;
        break;
    }
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS)
{
    // If this is a binop, find its precedence.
    while (true)
    {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS)
            return nullptr;

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS.
        LHS =
            std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParsePrimary();

    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != TOK_IDENT)
        return LogErrorP("Expected function name in prototype");

    std::string FnName = identifierStr;
    getNextToken();

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    getNextToken(); // Eat '('

    std::vector<std::pair<std::string, std::string>> ArgNamesTypes; // Argument name and type pairs.

    while (CurTok == TOK_IDENT)
    {
        std::string ArgName = identifierStr;
        getNextToken(); // Eat argument name

        if (CurTok != ':')
            return LogErrorP("Expected ':' after argument name");

        getNextToken(); // Eat ':'

        if (CurTok != TOK_ARG_TYPE)
            return LogErrorP("Expected argument type");

        std::string ArgType = identifierStr;
        getNextToken(); // Eat argument type

        ArgNamesTypes.push_back(std::make_pair(ArgName, ArgType));

        if (CurTok == ',')
            getNextToken(); // Eat ',' for the next argument
        else if (CurTok != ')')
            return LogErrorP("Expected ')' or ',' in prototype");
    }

    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    getNextToken(); // Eat ')'

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNamesTypes));
}

/// definition ::= 'fn' prototype expression
static std::unique_ptr<FunctionAST> ParseFunctionDefinition()
{
    getNextToken(); // eat fn.

    auto Proto = ParsePrototype();

    if (!Proto)
        return nullptr;

    if (CurTok != '{')
        return nullptr;

    getNextToken(); // eat '{'

    auto Body = ParseExpression();

    if (!Body)
        return nullptr;

    if (CurTok != ';')
        return nullptr;

    getNextToken(); // eat ';'

    if (CurTok != '}')
        return nullptr;

    getNextToken(); // eat '}'

    // todo: dodati ;

    return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // Create a vector of pairs for argument names and types
        std::vector<std::pair<std::string, std::string>> args;
        args.push_back(std::make_pair("arg1", "i32"));
        args.push_back(std::make_pair("arg2", "f32"));

        // Use std::make_unique to create a PrototypeAST object
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr", std::move(args));

        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external ::= 'use' prototype
static std::unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern.
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

Function *getFunction(std::string Name)
{
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}

Value *IntegerExprAST::codegen()
{
    // specificiramo da je 32-bitni int i da je u pitanju signed int
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

Value *FloatExprAST::codegen()
{
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen()
{
    // Look this variable up in the function.
    Value *V = NamedValues[Name];
    if (!V)
        return LogErrorV("Unknown variable name");
    return V;
}

Value *BinaryExprAST::codegen()
{
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    if (!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the addition.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the addition.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the addition.
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            // Convert both operands to double and then perform the addition.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
    case '-':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the subtraction.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the subtraction.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the subtraction.
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            // llvm::errs() << "L type: " << *L->getType() << "\n";
            // llvm::errs() << "R type: " << *R->getType() << "\n";

            // Convert both operands to double
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            // llvm::errs() << "L type: " << *L->getType() << "\n";
            // llvm::errs() << "R type: " << *R->getType() << "\n";
            return Builder->CreateSub(L, R, "subtmp");
        }

    case '*':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the multiplication.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the multiplication.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the multiplication.
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            // Convert both operands to double
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateMul(L, R, "multmp");
        }

    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
}

Value *CallExprAST::codegen()
{
    // Look up the name in the global module table.
    Function *CalleeF = getFunction(Callee);

    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // Check the number of arguments
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect number of arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        Value *ArgValue = Args[i]->codegen();
        if (!ArgValue)
            return nullptr;

        Type *ExpectedType = CalleeF->getFunctionType()->getParamType(i);

        // Perform type conversion if the argument type doesn't match the expected type.
        if (ArgValue->getType() != ExpectedType)
        {
            if (ArgValue->getType()->isIntegerTy() && ExpectedType->isDoubleTy())
            {
                // Convert integer to double.
                ArgValue = Builder->CreateSIToFP(ArgValue, Type::getDoubleTy(*TheContext));
            }
            else if (ArgValue->getType()->isDoubleTy() && ExpectedType->isIntegerTy())
            {
                // Convert double to integer.
                ArgValue = Builder->CreateFPToSI(ArgValue, ExpectedType);
            }
        }

        ArgsV.push_back(ArgValue);
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::codegen()
{
    Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    // Convert condition to a bool by comparing non-equal to 0.0.
    CondV = Builder->CreateFCmpONE(
        CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *IfBB = BasicBlock::Create(*TheContext, "if", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(CondV, IfBB, ElseBB);

    // Emit then value.
    Builder->SetInsertPoint(IfBB);

    std::vector<Value *> IfVs(IfBlock.size());
    for (auto &expr : IfBlock)
    {
        Value *IfV = expr->codegen();
        if (!IfV)
        {
            return nullptr;
        }
        IfVs.push_back(IfV);
    }

    Builder->CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    IfBB = Builder->GetInsertBlock();

    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);

    std::vector<Value *> ElseVs(ElseBlock.size());
    for (auto &expr : ElseBlock)
    {
        Value *ElseV = expr->codegen();
        if (!ElseV)
        {
            return nullptr;
        }
        ElseVs.push_back(ElseV);
    }

    Builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder->GetInsertBlock();

    // Emit merge block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder->SetInsertPoint(MergeBB);

    // mozda imam ovdje problem?
    PHINode *PN = nullptr;
    PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "ifftmp");

    PN->addIncoming(IfVs.back(), IfBB);
    PN->addIncoming(ElseVs.back(), ElseBB);
    return PN;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
// For petlju nije lako implementirat pa je ovo kod iz tutorijala. implementirana je while petlja
Value *ForExprAST::codegen()
{
    // Emit the start code first, without 'variable' in scope.
    // Type-checking and conversion if necessary.
    Type *VarType = Type::getDoubleTy(*TheContext);
    Value *StartVal = Start->codegen();
    Value *EndVal = End->codegen();

    if (!StartVal || !EndVal)
        return nullptr;

    // Check if the types are integers, and convert them to float if needed.
    if (StartVal->getType()->isIntegerTy())
    {
        StartVal = Builder->CreateSIToFP(StartVal, VarType, "caststart");
    }

    if (EndVal->getType()->isIntegerTy())
    {
        EndVal = Builder->CreateSIToFP(EndVal, VarType, "castend");
    }

    // Debugging: Print start and end values.
    // fprintf(stderr, "Start Value: %f\n", llvm::cast<llvm::ConstantFP>(StartVal)->getValueAPF().convertToFloat());
    // fprintf(stderr, "End Value: %f\n", llvm::cast<llvm::ConstantFP>(EndVal)->getValueAPF().convertToFloat());

    // Make the new basic block for the loop header, inserting after the current block.
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    // Insert an explicit fall through from the current block to the LoopBB.
    Builder->CreateBr(LoopBB);

    // Start insertion in LoopBB.
    Builder->SetInsertPoint(LoopBB);

    // Start the PHI node with an entry for Start.
    PHINode *Variable =
        Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    // Within the loop, the variable is defined equal to the PHI node.
    // If it shadows an existing variable, we have to restore it, so save it now.
    Value *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

    // Emit the body of the loop.
    for (const auto &BodyExpr : Body)
    {
        if (!BodyExpr->codegen())
            return nullptr;
    }

    // Emit the step value.
    Value *StepVal = nullptr;
    if (Step)
    {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    }
    else
    {
        // If not specified, use 1.0.
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

    // Convert condition to a bool by comparing non-equal to 0.0.
    Value *EndCond = Builder->CreateFCmpONE(
        Variable, EndVal, "loopcond");

    // Create the "after loop" block and insert it.
    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB =
        BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    // Insert the conditional branch into the end of LoopEndBB.
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    // Any new code will be inserted in AfterBB.
    Builder->SetInsertPoint(AfterBB);

    // Add a new entry to the PHI node for the backedge.
    Variable->addIncoming(NextVar, LoopEndBB);

    // Restore the unshadowed variable.
    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);

    // for expr always returns 0.0.
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Value *WhileExprAST::codegen()
{
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
    Builder->CreateBr(LoopBB);
    // Start insertion in LoopBB.
    Builder->SetInsertPoint(LoopBB);
    for (auto &expr : Body)
    {
        if (!expr->codegen())
            return nullptr;
    }
    Value *EndCond = End->codegen();
    if (!EndCond)
        return nullptr;
    BasicBlock *AfterBB =
        BasicBlock::Create(*TheContext, "afterloop", TheFunction);
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);
    // Any new code will be inserted in AfterBB.
    Builder->SetInsertPoint(AfterBB);

    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Function *PrototypeAST::codegen()
{
    // Prepare a vector of LLVM types for the function arguments based on their types.
    std::vector<Type *> ArgTypes;
    for (const auto &Arg : Args)
    {
        if (Arg.second == "i32")
        {
            // If the argument type is "i32," use the integer type.
            ArgTypes.push_back(Type::getInt32Ty(*TheContext));
        }
        else if (Arg.second == "f32")
        {
            // If the argument type is "double," use the double type.
            ArgTypes.push_back(Type::getDoubleTy(*TheContext));
        }
        else
        {
            // Handle other types as needed.
            fprintf(stderr, "Unsupported argument type: %s", Arg.second.c_str());
            return nullptr;
        }
    }

    // Create the function type based on the argument types and return type.
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), ArgTypes, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto &Arg : F->args())
    {
        Arg.setName(llvm::Twine(Args[Idx++].first));
    }

    return F;
}

Function *FunctionAST::codegen()
{
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction)
        return nullptr;

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value *RetVal = Body->codegen())
    {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*TheFunction);

        // Optimize the function
        TheFPM->run(*TheFunction);

        return TheFunction;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager()
{
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // Create a new pass manager attached to it.
    TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->add(createInstructionCombiningPass());
    // Reassociate expressions.
    TheFPM->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doInitialization();
}

static void HandleFunctionDefinition()
{
    if (auto FnAST = ParseFunctionDefinition())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            ExitOnErr(TheJIT->addModule(
                ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndPassManager();
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern()
{
    if (auto ProtoAST = ParseExtern())
    {
        if (auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read use: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr())
    {
        if (FnAST->codegen())
        {
            // Create a ResourceTracker to track JIT'd memory allocated to our
            // anonymous expression -- that way we can free it after executing.
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndPassManager();

            // Search the JIT for the __anon_expr symbol.
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            // Get the symbol's address and cast it to the right type (takes no
            // arguments, returns a double) so we can call it as a native function.
            double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
            double result = FP();
            fprintf(stderr, "Evaluated to %f\n", result);

            // Delete the anonymous expression module from the JIT.
            ExitOnErr(RT->remove());
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | extern | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, ">>> ");
        switch (CurTok)
        {
        case TOK_EOF:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case TOK_FN:
            HandleFunctionDefinition();
            break;
        case TOK_USE:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X)
{
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X)
{
    fprintf(stderr, "%f\n", X);
    return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, ">>> ");
    getNextToken();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    // Make the module, which holds all the code.
    InitializeModuleAndPassManager();

    // Run the main "interpreter loop" now.
    MainLoop();

    return 0;
}
