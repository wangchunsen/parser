package parser

import scala.quoted._
import scala.tasty._

object macros{
  inline def parserName:Parser.Name = ~parserNameImpl

  def parserNameImpl(implicit reflect: Reflection): Expr[Parser.Name] = {
    import reflect._

    def getClassOwner(symbl:Symbol):String = {
      var smb:Symbol = symbl
      while(smb != null){
        val isClass = IsClassSymbol.unapply(smb)
        if(isClass.isDefined) return isClass.get.fullName
        smb = smb.owner
      }
      "UnkonwClass"
    }


    val owner = reflect.rootContext.owner
    val name:String = owner match{
      case IsValSymbol(symbl) => 
        val valSymbol = symbl.tree.symbol
        val pos = valSymbol.pos
        s"${valSymbol.name} in ${getClassOwner(valSymbol)} at line ${pos.startLine + 1}"
      case IsDefSymbol(symbl) => 
        val defSymbl = symbl.tree.symbol
        val pos = defSymbl.pos
        s"${defSymbl.name} in ${getClassOwner(defSymbl)} at line ${pos.startLine + 1}"
      case _=> "unknow"
    }
    val expr = name.toExpr
    '{Parser.Name(~expr)}
  }
}