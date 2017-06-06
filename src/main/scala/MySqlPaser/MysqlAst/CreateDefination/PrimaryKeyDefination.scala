package MySqlPaser.MysqlAst.CreateDefination

import MySqlPaser.Helper.Helper

/**
  * Created by npvinh on 6/5/17.
  */
case class PrimaryKeyDefination(key:List[String]) extends CreateDefination {
  override def toString: String = {
    val result = "$table->primary([" + key.map(a => Helper.escapeStringSql(a)).mkString(", ") +"])"
    result
  }
}
