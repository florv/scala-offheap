// Based on AST matching sample by Eli Bendersky (eliben@gmail.com)
#include <string>
#include <iostream>
#include <fstream>
#include <unordered_map>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"

#include <boost/algorithm/string/replace.hpp>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory MatcherSampleCategory("Matcher Sample");

class BindGen {
public:

    BindGen(const std::string java_file, const std::string c_file) {
        // FIXME: Handle errors
        ofjava.open(java_file);
        ofc.open(c_file);

        ofjava << "package scala.offheap.numeric.jni;" << std::endl << std::endl;
        ofjava << "public class LapackJNI {" << std::endl;
        ofjava << "\tstatic {" << std::endl;
        ofjava << "\t\tSystem.loadLibrary(\"LapackJNI\");" << std::endl;
        ofjava << "\t}" << std::endl << std::endl;

        ofc << "#include <jni.h>" << std::endl;
        ofc << "#include <stdio.h>" << std::endl;
        ofc << "#include <cblas.h>" << std::endl;
        ofc << "#include <lapacke.h>" << std::endl << std::endl;
    }

    void addEnumConstant(const std::string name, const std::string value) {
        ofjava << "\tpublic static int " << name << " = " << value << ";" << std::endl;
        std::cout << "\tpublic static int " << name << " = " << value << ";" << std::endl;
    }

    void addDecl(const FunctionDecl *Decl) {
        generateJavaCode(Decl);
        generateCode(Decl);
    }

    void generateJavaCode(const FunctionDecl *Decl) {
        std::string retType = asJavaType(Decl->getReturnType());
        std::string funName = Decl->getNameInfo().getName().getAsString();
        ofjava << "\tpublic static native " << retType << " " << funName << "(";
        unsigned numParams = Decl->getNumParams();
        if (numParams > 0) {
            for (unsigned i = 0; i < Decl->getNumParams() - 1; ++i) {
                ofjava << toJavaParam(Decl->getParamDecl(i)) << ", ";
            }
            ofjava << toJavaParam(Decl->getParamDecl(numParams - 1));
        }
        ofjava << ");" << std::endl;
    }

    void generateCode(const FunctionDecl *Decl) {
        unsigned numParams = Decl->getNumParams();
        std::string funName = Decl->getNameInfo().getName().getAsString();
        ofc << "JNIEXPORT " << asCJavaType(Decl->getReturnType())
            << " JNICALL Java_scala_offheap_numeric_jni_LapackJNI_" << javaMangle(funName)
            << " (" << "JNIEnv *java_env, jclass java_class, ";
        if (numParams > 0) {
            for (unsigned i = 0; i < Decl->getNumParams() - 1; ++i) {
                ofc << toCJavaParam(Decl->getParamDecl(i)) << ", ";
            }
            ofc << toCJavaParam(Decl->getParamDecl(numParams - 1));
        }
        ofc << ") {" << std::endl;
        if (Decl->getReturnType()->isVoidType()) {
            ofc << "\t" + funName + "(";
        } else {
            ofc << "\treturn " + funName + "(";
        }
        if (numParams > 0) {
            for (unsigned i = 0; i < Decl->getNumParams() - 1; ++ i) {
                const ParmVarDecl *paramDecl = Decl->getParamDecl(i);
                ofc << castAsC(paramDecl)
                    << paramDecl->getNameAsString() + ", ";
            }
            const ParmVarDecl *paramDecl = Decl->getParamDecl(numParams - 1);
            ofc << castAsC(paramDecl) << paramDecl->getNameAsString();
        }
        ofc << ");" << std::endl
            << "}" << std::endl << std::endl;
    }

    void finish() {
        ofjava << "}" << std::endl;
        ofjava.close();
        ofc.close();
    }

private:
    std::ofstream ofjava;
    std::ofstream ofc;

    std::string javaMangle(std::string name) {
        boost::replace_all(name, "_", "_1");
        return name;
        //return std::replace(name.begin(), name.end(), '_', '|');
    }

    std::string toJavaParam(const ParmVarDecl *PVD) {
        std::string t = asJavaType(PVD->getOriginalType());
        return t + " " + PVD->getNameAsString();
    }

    std::string asJavaType(QualType QT) {
        const Type *t = QT.getTypePtr();
        if (t->isPointerType()) {
            return "long";
        } else if (t->isEnumeralType()) {
            return "int";
        } else if (t->isBuiltinType()) {
            // Following cast should be safe since isBuiltinType() checks that
            // the type isa<BuiltinType>.
            const BuiltinType *bt = static_cast<const BuiltinType*>(t);
            LangOptions lo = LangOptions();
            PrintingPolicy policy = PrintingPolicy(lo);
            return std::string(bt->getName(policy));
        } else if (t->isComplexType()) {
            // TODO: This probably won't work.
            return "long";
        } else {
            std::cout << "No java type to map (" + QualType::getAsString(QT.split()) + ") to." << std::endl;
            return "???";
        }
    }

    std::string toCJavaParam(const ParmVarDecl *PVD) {
        return asCJavaType(PVD->getOriginalType())
            + " " + PVD->getNameAsString();
    }

    std::string asCJavaType(QualType QT) {
        const Type *t = QT.getTypePtr();
        if (t->isVoidType()) {
            return "void";
        } else if (t->isEnumeralType()) {
            return "int";
        } else {
            std::string s = asJavaType(QT);
            if (s.compare("void") == 0) {
                std::cout << "Non-void type has void name, assuming long instead" << std::endl;
                return "long";
            }
            return s;
        }
    }

    std::string castAsC(const ParmVarDecl *PVD) {
        const Type *t = PVD->getOriginalType().getTypePtr();
        if (t->isPointerType()) {
            return "(" + QualType::getAsString(PVD->getType().split()) + ")";
        } else {
            return "";
        }
    }
};

typedef std::unordered_map<std::string, int> DeclBoolMap;

class DeclHandler : public MatchFinder::MatchCallback {
public:
    DeclHandler(BindGen *BG_) {
        BG = BG_;
    }

    virtual void run(const MatchFinder::MatchResult &Result) {
        SourceManager &sm = Result.Context->getSourceManager();
        const FunctionDecl *Decl = Result.Nodes.getNodeAs<FunctionDecl>("funDecl");
        std::string location = sm.getFilename(Decl->getLocation()).str();
        std::string name = Decl->getNameInfo().getName().getAsString();
        if (location == "/usr/include/cblas.h"
                || location == "/usr/include/lapacke.h") {
            if ( ! (Decl->isThisDeclarationADefinition()
                        && Decl->hasPrototype())) {
                std::cout << "Found function " << name
                          << " in " << location << std::endl;
                if (map.count(name)) {
                    std::cout << "Is a duplicate, ignoring it." << std::endl;
                } else {
                    map.insert( {{name, 1}} );
                    BG->addDecl(Decl);
                }
            }
        }
    }

private:
    BindGen *BG;
    DeclBoolMap map;
};

class EnumHandler : public MatchFinder::MatchCallback {
public:
    EnumHandler(BindGen *BG_) {
        BG = BG_;
    }

    virtual void run(const MatchFinder::MatchResult &Result) {
        SourceManager &sm = Result.Context->getSourceManager();
        const EnumConstantDecl *decl = Result.Nodes.getNodeAs<EnumConstantDecl>("enumConstantDecl");
        std::string location = sm.getFilename(decl->getLocation()).str();
        if (1) {
            std::string name = decl->getNameAsString();
            std::cout << "Found enum constant " << name << " in " << location << std::endl;
            llvm::APSInt intValue;
            const Expr *expr = decl->getInitExpr();
            if (expr != NULL) {
                expr->EvaluateAsInt(intValue, *Result.Context);
                BG->addEnumConstant(name, intValue.toString(10));
            } else {
                std::cout << "Not an int enum, skipping." << std::endl;
            }
        }
    }

private:
    BindGen *BG;
};

// Implements an ASTConsumer for reading the AST produced by the Clang parser
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(BindGen &BG) : HandlerForFunDecl(&BG), HandlerForEnum(&BG) {
    // The MatchFinder will look for function declaration
    Matcher.addMatcher(functionDecl().bind("funDecl"), &HandlerForFunDecl);
    Matcher.addMatcher(enumConstantDecl().bind("enumConstantDecl"), &HandlerForEnum);
  }

  // This is called for every translation unit. The MatchFinder is then run on the AST produced by the translation unit.
  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

  /*
  ~MyASTConsumer() {
  }
  */

private:
  DeclHandler HandlerForFunDecl;
  EnumHandler HandlerForEnum;
  MatchFinder Matcher;
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:

  MyFrontendAction() : BG("LapackJNI.java", "LapackJNI.c") {}

  ASTConsumer *CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    return new MyASTConsumer(BG);
  }

  ~MyFrontendAction() {
      BG.finish();
  }

private:
  BindGen BG;
};

int main(int argc, const char **argv) {
  CommonOptionsParser op(argc, argv, MatcherSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());

  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}
