CXX = clang
CXXFLAGS += -std=c++11 -I/usr/lib/llvm-3.5/include -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS
LDFLAGS += -lclang -lLLVM-3.5 -lLTO

USEDLIBS = \
    clangFrontend.a clangSerialization.a clangDriver.a \
    clangTooling.a clangParse.a clangSema.a \
    clangAnalysis.a clangRewriteFrontend.a clangRewrite.a \
    clangEdit.a clangAST.a clangLex.a clangBasic.a \
    LLVMSupport.a clangASTMatchers.a

LIB = /usr/lib/llvm-3.5/lib
LIBS = $(addprefix $(LIB)/lib,$(USEDLIBS))

bindgen: bindgen.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

bindgen.o: bindgen.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $^

clean:
	-rm bindgen.o bindgen
