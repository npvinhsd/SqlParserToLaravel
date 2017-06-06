package MySqlPaser.MysqlAst.CreateDefination

import MySqlPaser.Helper.Helper
/**
  * Created by npvinh on 6/5/17.
  */
case class ConstrainDefination(column: String,
                               foreignKey: String,
                               tableReference: String,
                               columnReference: String) extends CreateDefination {
  override def toString: String = ""
}
