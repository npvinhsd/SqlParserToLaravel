package MySqlPaser.MysqlAst.CreateDefination

import MySqlPaser.Helper.Helper

/**
  * Created by npvinh on 6/5/17.
  */
case class KeyDefination(key:String, list:List[String]) extends CreateDefination {
  override def toString: String = {
    val result = "$table->index([" + list.map(a => Helper.escapeStringSql(a)).mkString(", ") +"])"
    result
  }
}
