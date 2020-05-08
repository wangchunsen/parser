package csw.parser

import scala.reflect.macros.blackbox.{Context => MC}

object Macros{

  def parserNameImpl(c: MC):c.Expr[ParseName] = {
    import c.universe._

    def getClassOwner(symbl:Symbol):String = {
      var smb:Symbol = symbl
      while(smb != null && smb != NoSymbol){
        if(smb.isClass) return smb.fullName
        smb = smb.owner
      }
      "UnknownClass"
    }

    val owner = c.internal.enclosingOwner
    val position = owner.pos

    val name = s"${owner.name} in ${getClassOwner(owner)} at line ${position.line}"
    val res =
      q"""
       csw.parser.ParseName($name)
     """
      c.Expr(res)
  }
}