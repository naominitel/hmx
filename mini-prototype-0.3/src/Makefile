##########################################################################
#  Mini, a type inference engine based on constraint solving.            #
#  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               #
#  and Didier Rémy.                                                      #
#                                                                        #
#  This program is free software; you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation; version 2 of the License.               #
#                                                                        #
#  This program is distributed in the hope that it will be useful, but   #
#  WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  General Public License for more details.                              #
#                                                                        #
#  You should have received a copy of the GNU General Public License     #
#  along with this program; if not, write to the Free Software           #
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         #
#  02110-1301 USA                                                        #
#                                                                        #
##########################################################################

# $Id: Makefile 421 2006-12-22 09:27:42Z regisgia $

OCAMLMAKEFILE=../config/OCamlMakefile

SOURCES=								 \
  prettyPrinter.mli							 \
  prettyPrinter.ml							 \
  infiniteArray.mli							 \
  infiniteArray.ml							 \
  misc.mli								 \
  misc.ml								 \
  env.mli								 \
  env.ml								 \
  positions.mli								 \
  positions.ml								 \
  mark.mli								 \
  mark.ml								 \
  unionFind.mli								 \
  unionFind.ml								 \
  astPositions.mli							 \
  astPositions.ml							 \
  processing.mli							 \
  processing.ml								 \
  parsingExceptions.mli							 \
  parsingExceptions.ml							 \
  miniParser.mly							 \
  miniLexer.mll								 \
  basicSetEquations.mli							 \
  basicSetEquations.ml							 \
  intRank.mli								 \
  intRank.ml								 \
  coreAlgebra.mli							 \
  coreAlgebra.ml							 \
  multiEquation.mli							 \
  multiEquation.ml							 \
  unifier.mli								 \
  unifier.ml								 \
  constraint.mli							 \
  constraint.ml								 \
  solver.mli								 \
  solver.ml								 \
  miniAst.mli								 \
  miniAst.ml								 \
  miniAlgebra.mli							 \
  miniAlgebra.ml							 \
  print.mli								 \
  print.ml								 \
  constraintPrettyPrinter.mli						 \
  constraintPrettyPrinter.ml						 \
  miniTypingExceptions.mli						 \
  miniTypingExceptions.ml						 \
  miniPrettyPrinter.mli							 \
  miniPrettyPrinter.ml							 \
  miniKindInferencer.mli						 \
  miniKindInferencer.ml							 \
  miniTypingEnvironment.mli						 \
  miniTypingEnvironment.ml						 \
  miniTypes.mli								 \
  miniTypes.ml								 \
  miniInfer.mli								 \
  miniInfer.ml								 \
  constraintParser.mly							 \
  constraintLexer.mll							 \
  miniSyntacticAnalysis.mli						 \
  miniSyntacticAnalysis.ml						 \
  miniConstraintPrinter.mli						 \
  miniConstraintPrinter.ml						 \
  miniSolver.mli							 \
  miniSolver.ml								 \
  errors.mli								 \
  errors.ml								

DOC_FILES=$(filter %.ml %.mli, $(SOURCES))

RESULT=libmini

LIBS=str
OCAMLBCFLAGS=-thread -g
OCAMLNCFLAGS=
OCAMLBLDFLAGS=-thread -g
OCAMLDOCFLAGS=-keep-code -hide Env -colorize-code -intro ../doc/main
YFLAGS=-v
DOC_DIR=../doc
DOTFLAGS=-Nfontsize=48

all: bcl mini

-include $(OCAMLMAKEFILE)

mini: libmini.cma mini.ml
	$(OCAMLC) $(OCAMLBCFLAGS) -I . -c mini.ml
	$(OCAMLC) -g -o mini libmini.cma mini.cmo

WWW=../doc

TRASH=*~ mini.opt mini mini.o mini.cm{x,o,i} *.output \
	miniParser.ml constraintParser.ml miniLexer.ml constraintLexer.ml

realclean: clean clean-doc nobackup

