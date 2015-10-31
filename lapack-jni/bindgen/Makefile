CXX = clang
CXXFLAGS += -std=c++11 -I/usr/lib/llvm-3.5/include -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS
LDFLAGS += -lclang -lLLVM-3.5 -lLTO -lz -lpthread -lcurses -ldl -lm -lstdc++

USEDLIBS = \
    clangFrontend.a clangSerialization.a clangDriver.a \
    clangTooling.a clangParse.a clangSema.a \
    clangAnalysis.a clangRewriteFrontend.a clangRewrite.a \
    clangEdit.a clangAST.a clangLex.a clangBasic.a \
    LLVMSupport.a clangASTMatchers.a

LIB = /usr/lib/llvm-3.5/lib
LIBS = $(addprefix $(LIB)/lib,$(USEDLIBS))
#LIBS = $(wildcard $(LIB)/*.a)

bindgen: bindgen.o
	$(CXX) -g $(CXXFLAGS) -o $@ $^ $(LIBS) $(LDFLAGS)

bindgen.o: bindgen.cpp
	$(CXX) -g $(CXXFLAGS) -o $@ -c $^

clean:
	-rm bindgen.o bindgen
